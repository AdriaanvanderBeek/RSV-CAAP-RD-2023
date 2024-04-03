step_func <- function(ds,
                     time_points=time_points1,
                     post_period1=c('2009-07-01', '2011-06-01'),
                     post_period2=c('2011-07-01', '2015-06-01'),
                     post_period3=c('2015-07-01', '2019-06-01'),
                     vax.vars=c('post1','post2', 'post3') ,
                     other.covars='none' ,
                     mod='pois',
                     denom, 
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
  ds$month<-month(ds$date)
  
  ds$month <-as.factor(ds$month)
  month.dummies <- as.data.frame(model.matrix(~month, data=ds))
  month.dummies <- month.dummies[,-1]
  
  ds$one<-1
  
  ds<-cbind.data.frame(ds, month.dummies)
  
  ds$obs <- as.factor(1:nrow(ds))
  ds$log.offset<-log(ds[,denom]+0.5)
  
  #seas.vars<- c(names(as.data.frame(month.dummies) ))
  
  seas.vars <- c('sin12','cos12','sin6','cos6')
  
  offset.vars<-'offset(log.offset)'
  
  if((other.covars=='none')[1]){
    mod.vars<-c(seas.vars,vax.vars)
    mod.covars.cf <- seas.vars
  }else{
    mod.vars<-c(seas.vars,other.covars,vax.vars)
    mod.covars.cf<-c(seas.vars,other.covars)
    for(i in 1:length(other.covars)){
      ds[,other.covars[i]] <- scale(log( ds[,other.covars[i]]+0.5)) 
    }
  }
  
  #Re-define time_index (it gets overwritten in loop above if other covars present)
  ds$time_index<-(1:nrow(ds))/nrow(ds)
  
  form1<-as.formula(paste0(outcome_name, '~'   ,paste0(c(mod.vars,offset.vars), collapse='+')))
  
  #Fit model
  if(mod=='pois'){
  mod1 <-
    glm(form1,
        data = ds, family='poisson'
        )
  }else if(mod=='negbin'){
    mod1 <-
      glm.nb(form1,
          data = ds
      )
  }
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
  
  preds.stage1.regmean <-
    exp(as.matrix(covars3) %*% t(pred.coefs.reg.mean) +ds$log.offset)
  
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
  
  preds.stage1.regmean.cf <-    exp(as.matrix(covars3.cf) %*% t(pred.coefs.reg.mean)+ ds$log.offset)
  
  #preds.cf.q<-t(apply(preds.stage1.regmean.cf,1,quantile, probs=c(0.025,0.5,0.975)))
  
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
  prevented.post.t <-    preds.stage1.regmean.cf - preds.stage1.regmean
  
  cum.post.t <-  apply(prevented.post.t,2, function(x) cumsum(x)   )
  
  cum.post.t.q <-   as.data.frame(t(apply(cum.post.t, 1, quantile, probs = c(0.025, 0.5, 0.975)))) %>% 
    cbind.data.frame(., 'date'=as.Date(ds$date))  %>%
    rename(median=`50%`, lcl=`2.5%`, ucl=`97.5%`)
  
  ###plots
  p.rr.trend <- rr.q.t %>% 
    as.data.frame() %>%
    rename(median=`50%`, lcl=`2.5%`, ucl=`97.5%`) %>%
    cbind.data.frame(.,ds) %>%
    ggplot(., aes( x=date, y=median)) +
    geom_line() +
    theme_classic() +
    geom_ribbon( aes(x=date, ymin=lcl, ymax=ucl), alpha=0.1) +
    ylab('Rate ratio') +
    geom_hline(yintercept=1, lty=2, col='red')+
    geom_vline(xintercept=as.numeric(as.Date(post_period1[1])), lty=2, col='black')
  
  p.cum_prevented <- cum.post.t.q %>% 
    as.data.frame() %>%
    left_join(.,ds, by='date') %>%
    ggplot( aes( x=date, y=median)) +
    geom_line() +
    theme_classic() +
    geom_ribbon(aes(x=date, ymin=lcl, ymax=ucl), alpha=0.1) +
    ylab('Cases averted') +
    geom_hline(yintercept=1, lty=2, col='red')+
    geom_vline(xintercept=as.numeric(as.Date(post_period1[1])), lty=2, col='black')
  
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
  
  p.preds <- all.preds %>%
    ggplot( aes( x=date, y=median_pred)) +
    geom_ribbon(data=all.preds, aes(x=date, ymin=lcl_cf, ymax=ucl_cf), alpha=0.1) +
    geom_line() +
    geom_point(data=all.preds, aes(x=date, y=outcome.inc), color='red', alpha=0.3) +
    geom_line(data=all.preds, aes(x=date, y=median_cf), color='blue', lty=2) +
    theme_classic() +
    ylab('Cases/1000') +
    ylim(0,NA)+
    geom_vline(xintercept=as.numeric(as.Date(post_period1[1])), lty=2, col='black')
  
  
  agg.pred <- all.preds %>%
    mutate(year=year(date)) %>%
    group_by(year) %>%
    summarize( across(c(median_pred,lcl_cf, ucl_cf, median_cf, outcome.inc), sum )) 
  
  p.preds.agg <- agg.pred %>%
    ggplot( aes( x=year, y=median_pred)) +
    geom_ribbon(data=agg.pred, aes(x=year, ymin=lcl_cf, ymax=ucl_cf), alpha=0.1) +
    geom_line() +
    geom_point(data=agg.pred, aes(x=year, y=outcome.inc), color='red', alpha=0.3) +
    geom_line(data=agg.pred, aes(x=year, y=median_cf), color='white', lty=2) +
    theme_classic() +
    ylab('Number of cases') +
    ylim(0,NA)+
    geom_vline(xintercept=as.numeric(year(as.Date(post_period1[1]))), lty=2, col='black')
  
  rr.out <- list('rr.q.post' = rr.q.post, 'aic1'=aic1,'outcome'=ds[,outcome_name],
                 'preds.cf.q'=preds.cf.q,'preds.q'=preds.q,'form1'=form1,
                 'rr.q.t'=rr.q.t,'overdispersion'=overdispersion, 'dates'=ds$date,
                 'p.preds.agg'=p.preds.agg,
                 'p.preds'=p.preds,
                 'p.cum_prevented'=p.cum_prevented,
                 'p.rr.trend'=p.rr.trend
                 )
  return(rr.out)
}
