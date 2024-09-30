step_func <- function(ds,
                      time_points=time_points1,
                      post_period1=c('2009-07-01', '2011-06-01'),
                      post_period2=c('2011-07-01', '2015-06-01'),
                      post_period3=c('2015-07-01', '2019-06-01'),
                      vax.vars=c('post1','post2', 'post3') ,
                      other.covars,
                      mod=mod,
                      denom,
                      N_CAAP,
                      N_tested,
                      group,
                      outcome_name ){
  
  ds <- ds[order(ds$date),]
  ds$time_index<-(1:nrow(ds))/nrow(ds)
  
  
  
  
  #Create the dummy variables for 3 post-vaccine time periods
  ds$post1<-as.numeric(ds$date >= post_period1[1] & ds$date <=  post_period1[2]) 
  ds$post2<-as.numeric(ds$date >= post_period2[1] & ds$date <=  post_period2[2]) 
  ds$post3<-as.numeric(ds$date >= post_period3[1] ) 
  
  ds$time_index<-1:nrow(ds)
  ds$sin12<-sin(2*pi* ds$time_index/12)
  ds$cos12<-cos(2*pi* ds$time_index/12)
  ds$sin6<-sin(2*pi* ds$time_index/6)
  ds$cos6<-cos(2*pi* ds$time_index/6)
  
  
  ds$obs <- as.factor(1:nrow(ds))
  
  ds$log.offset<-log(ds[,denom]+0.5)
  
  # Calculate the RSV test ratio log offset
  ds$log.ratio.offset <- log(ds[[denom]] / (1000 * ((ds[[N_CAAP]] + 0.5) / (ds[[N_tested]] + 0.5))))
  
  # Choose offset variable for CAAP or RSV-CAAP
  if (group == 'CAAP') {
    offset.vars <- 'offset(log.offset)'
  } else if (group == 'RSV_CAAP') {
    offset.vars <- 'offset(log.ratio.offset)'
  } else {
    stop("Unknown group: ", group)
  }
  
  
  seas.vars <- c('sin12','cos12','sin6','cos6')
  
  
  if((other.covars=='none')[1]){
    mod.vars<-c(seas.vars,vax.vars)
    mod.covars.cf <- seas.vars
  }else{
    mod.vars<-c(seas.vars,other.covars,vax.vars)
    mod.covars.cf<-c(seas.vars,other.covars)
    for(i in 1:length(other.covars)){
      ds[,other.covars[i]] <- scale(log( ds[,other.covars[i]])) 
    }
  }
  
  #Re-define time_index (it gets overwritten in loop above if other covars present)
  ds$time_index<-(1:nrow(ds))/nrow(ds)
  
  #form1<-as.formula(paste0(outcome_name, '~' ,paste0(c(mod.vars,offset.vars), collapse='+')))
  form1 <- as.formula(paste0(outcome_name, '~', paste(c(mod.vars, offset.vars), collapse='+')))
  
  control_params <- glm.control(maxit = 100)
  
  # Fit model
  mod1 <- glm.nb(form1, data = ds, control = control_params)
  
  preds <- predict(mod1, type='response')
  
  aic1<-AIC(mod1)
  deviance1<-summary(mod1)$deviance
  df1<-summary(mod1)$df[2]
  overdispersion<-deviance1/df1
  
  
  #GENERATE PREDICTIONS
  covars3 <-
    as.matrix(cbind(ds[, mod.vars])) 
  covars3 <- cbind.data.frame(rep(1, times = nrow(covars3)), covars3)
  names(covars3)[1] <- "Intercept"
  
  pred.coefs.reg.mean <-
    mvrnorm(n = 1000,
            mu = coef(mod1),
            Sigma = vcov(mod1))
  
  
  
  
  if (group == "CAAP") {
    offset <- ds$log.offset
  } else if (group == "RSV_CAAP") {
    offset <- ds$log.ratio.offset
  } else {
    stop("Unknown group: ", group)
  }
  
  # Calculate preds.stage1.regmean using the selected offset
  preds.stage1.regmean <- exp(as.matrix(covars3) %*% t(pred.coefs.reg.mean) + offset)
  
  preds.q<-t(apply(preds.stage1.regmean,1,quantile, probs=c(0.025,0.5,0.975)))
  
  #Then for counterfactual, set post-vax effects to 0.
  covars3.cf <-
    as.matrix(cbind(ds[, c(
      mod.covars.cf
    )], matrix(
      0, nrow = nrow(ds), ncol = length(vax.vars)
    )))
  covars3.cf <-
    cbind.data.frame(rep(1, times = nrow(covars3.cf)), covars3.cf)
  
  preds.stage1.regmean.cf <-    exp(as.matrix(covars3.cf) %*% t(pred.coefs.reg.mean)+ offset)
  
  
  
  rr.t <- preds.stage1.regmean / preds.stage1.regmean.cf
  rr.q.t <- t(apply(rr.t, 1, quantile, probs = c(0.025, 0.5, 0.975)))
  
  #incidence
  preds.stage1.regmean.inc <- apply(preds.stage1.regmean,2, function(X) X/exp(ds$log.offset)*1000)
  
  preds.stage1.regmean.cf.inc <- apply(preds.stage1.regmean.cf,2, function(X) X/exp(ds$log.offset)*1000)
  
  last.t<-nrow(rr.t) #evaluate at last time point
  preds.stage1.regmean.SUM <-   preds.stage1.regmean[last.t, ]
  preds.stage1.regmean.cf.SUM <-preds.stage1.regmean.cf[last.t, ]
  rr.post <- preds.stage1.regmean.SUM / preds.stage1.regmean.cf.SUM
  rr.q.post <- quantile(rr.post, probs = c(0.025, 0.5, 0.975))
  
  
  
  #Cumulative cases
  prevented.post.t <- preds.stage1.regmean.cf - preds.stage1.regmean
  cum.post.t <- apply(prevented.post.t, 2, function(x) cumsum(x))
  
  # Calculate incidence
  incidence <- apply(cum.post.t,2, function(X) X/exp(ds$log.offset)*1000)
  
  
  # Bind incidence with confidence intervals
  cum.post.inc.t.q <- as.data.frame(t(apply(incidence, 1, quantile, probs = c(0.025, 0.5, 0.975))))
  cum.post.inc.t.q <- cbind.data.frame(cum.post.inc.t.q, 'date' = as.Date(ds$date))
  
  # Rename columns
  cum.post.inc.t.q <- cum.post.inc.t.q %>%
    rename(median = `50%`, lcl = `2.5%`, ucl = `97.5%`)
  
  
  
  
  #Cumulative cases during stable period
  # Calculate prevented cases
  prevented.post.t.stable <- preds.stage1.regmean.cf - preds.stage1.regmean
  
  # Find the index corresponding to the date 2015-07-01
  start_date_index <- which(ds$date == as.Date("2015-07-01"))
  
  # Set values before 2015-07-01 to zero
  prevented.post.t.stable[1:(start_date_index - 1), ] <- 0
  
  # Calculate cumulative cases
  cum.post.t.stable <- apply(prevented.post.t.stable, 2, function(x) cumsum(x))
  
  # Calculate incidence
  incidence.stable <- apply(cum.post.t.stable,2, function(X) X/exp(ds$log.offset)*1000)
  
  # Bind incidence with confidence intervals
  cum.post.inc.t.q.stable <- as.data.frame(t(apply(incidence.stable, 1, quantile, probs = c(0.025, 0.5, 0.975))))
  cum.post.inc.t.q.stable <- cbind.data.frame(cum.post.inc.t.q.stable, 'date' = as.Date(ds$date))
  
  # Rename columns
  cum.post.inc.t.q.stable <- cum.post.inc.t.q.stable %>%
    rename(median = `50%`, lcl = `2.5%`, ucl = `97.5%`)
  
  # Extract values at the last month from cum.post.t
  cum.post.inc.t.q.stable.end <- cum.post.inc.t.q.stable[nrow(cum.post.inc.t.q.stable), ]
  
  
  
  
  
  
  
  # Plotting
  p.rr.trend <- rr.q.t %>% 
    as.data.frame() %>%
    rename(median=`50%`, lcl=`2.5%`, ucl=`97.5%`) %>%
    cbind.data.frame(.,ds) %>%
    ggplot(., aes( x=date, y=median)) +
    geom_line() +
    theme_classic() +
    geom_ribbon( aes(x=date, ymin=lcl, ymax=ucl)) +
    ylab('Rate ratio') +
    geom_hline(yintercept=1, lty=2, col='red')+
    geom_vline(xintercept=as.numeric(as.Date(post_period1[1])), lty=2, col='black') +
    geom_vline(xintercept = as.numeric(as.Date("2011-07-01")), lty = 2, col = 'black') +
    geom_vline(xintercept = as.numeric(as.Date("2015-07-01")), lty = 2, col = 'black')
  p.rr.trend <- p.rr.trend + scale_x_date(expand = c(0, 0), breaks = as.Date(c("2004-07-01", "2006-07-01", "2008-07-01", "2010-07-01", "2012-07-01", "2014-07-01", "2016-07-01", "2018-07-01")), labels = c("July 04", "July 06", "July 08", "July 10", "July 12", "July 14", "July 16", "July 18"))
  
  
  p.cum_prevented <- cum.post.inc.t.q %>%
    as.data.frame() %>%
    left_join(., ds, by = 'date') %>%
    ggplot(aes(x = date, y = median)) +
    geom_line() +
    theme_classic() +
    geom_ribbon(aes(x = date, ymin = lcl, ymax = ucl), alpha = 0.1) +
    ylab('Averted Cases per 1,000') +
    geom_hline(yintercept = 0, lty = 2, col = 'red') +
    geom_vline(xintercept = as.numeric(as.Date(post_period1[1])), lty = 2, col = 'black') +
    geom_vline(xintercept = as.numeric(as.Date("2011-07-01")), lty = 2, col = 'black') +
    geom_vline(xintercept = as.numeric(as.Date("2015-07-01")), lty = 2, col = 'black')
  p.cum_prevented <- p.cum_prevented + scale_x_date(expand = c(0, 0), breaks = as.Date(c("2004-07-01", "2006-07-01", "2008-07-01", "2010-07-01", "2012-07-01", "2014-07-01", "2016-07-01", "2018-07-01")), labels = c("July 04", "July 06", "July 08", "July 10", "July 12", "July 14", "July 16", "July 18"))
  
  #plot INCIDENCE
  preds.cf.q<-t(apply(preds.stage1.regmean.cf.inc,1,quantile, probs=c(0.025,0.5,0.975)))%>% 
    cbind.data.frame(., 'date'=as.Date(ds$date))  %>%
    rename(median_cf=`50%`, lcl_cf=`2.5%`, ucl_cf=`97.5%`)
  
  preds.q<-t(apply(preds.stage1.regmean.inc,1,quantile, probs=c(0.025,0.5,0.975)))%>% 
    cbind.data.frame(., 'date'=as.Date(ds$date))  %>%
    rename(median_pred=`50%`, lcl_pred=`2.5%`, ucl_pred=`97.5%`)
  
  all.preds <- preds.cf.q %>%
    left_join(preds.q, by='date') %>%
    cbind.data.frame('outcome'=ds[,outcome_name]) %>%
    mutate(outcome.inc = outcome / exp(ds$log.offset)*1000)
  
  
  
  
  
  p.preds <- ggplot(all.preds, aes(x = date, y = median_pred)) +
    # Add ribbon representing 95% CI around the median prediction
    geom_ribbon(aes(ymin = lcl_cf, ymax = ucl_cf), fill = 'royalblue') +
    # Add the median prediction line
    geom_line() +
    # Add points for actual outcomes
    geom_point(aes(y = outcome.inc), color = 'orangered', size = 0.5) +
    # Add dashed line for median counterfactual
    geom_line(aes(y = median_cf), color = 'royalblue', linetype = 2) +
    
    theme_classic() +
    # Set y-axis label
    ylab('Cases/1000') +
    # Allow y-axis to expand upwards without limits
    ylim(0, NA) +
    # Add vertical lines at specific dates
    geom_vline(xintercept = as.numeric(as.Date(post_period1[1])), linetype = 2, color = 'black') +
    geom_vline(xintercept = as.numeric(as.Date("2011-07-01")), linetype = 2, color = 'black') +
    geom_vline(xintercept = as.numeric(as.Date("2015-07-01")), linetype = 2, color = 'black') +
    # Customize x-axis
    scale_x_date(expand = c(0, 0), 
                 breaks = as.Date(c("2004-07-01", "2006-07-01", "2008-07-01", "2010-07-01", "2012-07-01", "2014-07-01", "2016-07-01", "2018-07-01")),
                 labels = c("July 04", "July 06", "July 08", "July 10", "July 12", "July 14", "July 16", "July 18"))
  
  
  
  
  
  
  
  
  # Changed so that years run from July through June
  agg.pred <- all.preds %>%
    mutate(year = ifelse(month(date) >= 7, year(date), year(date) - 1)) %>%
    group_by(year) %>%
    summarize(across(c(median_pred, lcl_cf, ucl_cf, median_cf, outcome.inc), sum))
  
  
  
  
  
  p.preds.agg <- agg.pred %>%
    ggplot( aes( x=year, y=median_pred)) +
    geom_ribbon(data=agg.pred, aes(x=year, ymin=lcl_cf, ymax=ucl_cf), alpha=0.1) +
    geom_line() +
    geom_point(data=agg.pred, aes(x=year, y=outcome.inc), color='red', alpha=0.3) +
    geom_line(data=agg.pred, aes(x=year, y=median_cf), color='white', lty=2) +
    theme_classic() +
    ylab('Cases/1,000') +
    ylim(0,NA)+
    geom_vline(xintercept=as.numeric(year(as.Date(post_period1[1]))), lty=2, col='black')
  
  rr.out <- list('rr.q.post' = rr.q.post, 'aic1'=aic1,'outcome'=ds[,outcome_name],
                 'preds.cf.q'=preds.cf.q,'preds.q'=preds.q,'form1'=form1,
                 'rr.q.t'=rr.q.t,'overdispersion'=overdispersion, 'dates'=ds$date,
                 'p.preds.agg'=p.preds.agg,
                 'p.preds'=p.preds,
                 'p.cum_prevented'=p.cum_prevented,
                 'p.rr.trend'=p.rr.trend,
                 'cum.post.inc.t.q' = cum.post.inc.t.q,
                 'cum.post.inc.t.q.stable' = cum.post.inc.t.q.stable,
                 'cum.post.inc.t.q.stable.end' =  cum.post.inc.t.q.stable.end,
                 'preds' = preds
                 
  )
  
  return(rr.out)
  
}