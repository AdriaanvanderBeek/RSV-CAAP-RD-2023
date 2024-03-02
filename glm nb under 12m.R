
library(zoo)
library(htmlTable)
library(lme4)
library(lubridate)
library(reshape2)
library(MASS)
library(ggplot2)
library(dplyr)
source('./functions_its.R')




ds1<- read.csv('C:/Users/Adrian/OneDrive - Ben Gurion University of the Negev/Documents/PIDU/RSV 2023/RSV-CAAP RD 2023/resp under 12 months.csv')

names(ds1)

str(ds1)

ds1 <- ds1 %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) # sort by date

str(ds1)



# Round the numbers to the nearest integer
ds1$rsv.caap.cases <- round(ds1$rsv.caap.cases)
ds1$rsv.bronch.cases <- round(ds1$rsv.bronch.cases)




ds1.m <- reshape2::melt(ds1[,c('date','caap.cases','rsv.caap.cases','bronch.cases','rsv.bronch.cases')], id.vars=c('date'))

p1 <- ggplot(ds1.m, aes(x=date, y=value, color=variable)) +
  geom_line() +
  ylab("Number of cases") +
  xlab("Date") +
  theme_classic() +
  geom_hline(yintercept=0, col='black', lty=1) +
  scale_color_manual(values = c("#e41a1c", "#377eb8","#4daf4a","#984ea3")) +
  geom_vline(xintercept=as.Date(c('2009-07-01','2011-07-01')), col='gray', lty=2)
p1



p2 <-p1 +
  facet_wrap(~variable , scales='free') +
  theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) 

p2



ds1 <- ds1 %>%
  arrange(date) %>% #make sure it is sorted by date
  mutate(index = row_number()/n(), #time index for linear trend, scale between 0,1
         monthn=month(date),
         month = as.factor(monthn), #declare month as a categorical variable
         post1 = if_else(date >= "2009-07-01" & date < "2011-07-01" ,1, 0),
         post2 = if_else(date >= "2011-07-01" & date < "2015-07-01" ,1, 0),
         post3 = if_else(date >= "2015-07-01"  ,1, 0),
         log_pop = log(pop.12 + 0.5)
  )

#time_points1 <- unique(ds1$date)


# Negative binomial model
# Outcome: CAAP
mod1.caap <- glm.nb(caap.cases ~ month + index +
                     post1 + post2 + post3 +  # Post period
                     index*post1 + index*post2 + post3*index + # Interactions
                     offset(log_pop), # Population offset
                   data=ds1)

# Summary
summary(mod1.caap)
AIC(mod1.caap)

ds.mod1 <- ds1

ds.mod1$pred.caap <- predict(mod1.caap, type='response', newdata=ds.mod1)


ds1.counter <- ds.mod1 %>%
  mutate(post1=0,
         post2=0,
         post3=0)

ds.mod1$pred.caap.cf <- predict(mod1.caap, type='response', newdata=ds1.counter) # newdata is now "ds1.counter"


ds.mod1$pred.caap.lim <- ds.mod1$pred.caap

# Predict till 2013-06-01 (4 years post PCV intro)
subset_range <- ds.mod1$date >= as.Date('2004-07-01') & ds.mod1$date <= as.Date('2013-06-01')

ds.mod1$pred.caap.cf.lim[subset_range] <- ds.mod1$pred.caap.cf[subset_range]


# Set trend to zero from 2013-07-01
last_obs_index <- which(ds.mod1$date == as.Date('2013-06-01'))

last_12_months_values <- ds.mod1$pred.caap.cf[(last_obs_index - 11):last_obs_index]

repeat_range <- ds.mod1$date >= as.Date('2013-07-01') & ds.mod1$date <= as.Date('2019-06-01')
ds.mod1$pred.caap.cf.lim[repeat_range] <- rep(last_12_months_values, length.out = sum(repeat_range))


ds.mod1$rr1.caap <- ds.mod1$pred.caap / ds.mod1$pred.caap.cf




p4 <- ggplot(ds.mod1, aes(x=date, y=caap.cases)) +
  geom_point( pch=16,col='gray') +
  ylab("Number of CAAP cases") +
  xlab("Date") +
  theme_classic() +
  geom_line(data = ds.mod1, aes(x=date, y = caap.cases), col = 'black') +
  geom_line(data=ds.mod1, aes(x=date, y=pred.caap), col='blue', lty=2) +
  geom_line(data=ds.mod1, aes(x=date, y=pred.caap.cf.lim), col='red', lty=2) +
  ylim(0, max(ds.mod1$pred.caap.cf))+
  geom_hline(yintercept=0, col='black', lty=1) 
p4

# JUst observed and counterfactual
p4a <- ggplot(ds.mod1, aes(x=date, y=caap.cases)) +
  geom_point( pch=16,col='gray') +
  ylab("Number of CAAP cases") +
  xlab("Date") +
  theme_classic() +
  geom_line(data = ds.mod1, aes(x=date, y = caap.cases), col = 'black') +
  geom_line(data=ds.mod1, aes(x=date, y=pred.caap.cf.lim), col='red', lty=2) +
  ylim(0, max(ds.mod1$pred.caap.cf))+
  geom_hline(yintercept=0, col='black', lty=1) 
p4a



vax.intro.date <- as.Date('2009-07-01') #when introduce vaccine?

vax.eval.date <- as.Date('2011-07-01') #When start evaluation (usually at least 12 month after intro)



eval.period<-ds1$date> vax.eval.date

ds.mod1$caap.rr <- ds.mod1$pred.caap /ds.mod1$pred.caap.cf

rr.caap.eval<- mean(ds.mod1$caap.rr[eval.period])

print(rr.caap.eval)

# Set the seed for reproducibility
set.seed(100)

# Number of bootstrap samples
num_bootstrap <- 1000

# Empty vector to store bootstrapped rate ratios
bootstrap_rr <- numeric(num_bootstrap)

# Bootstrap procedure
for (i in 1:num_bootstrap) {
  # Sample indices with replacement
  sample_indices <- sample(which(eval.period), replace = TRUE)
  
  # Calculate rate ratio using the sampled indices
  bootstrap_rr[i] <- mean(ds.mod1$caap.rr[sample_indices])
}

# Calculate the confidence intervals (e.g., 95%)
lower_ci <- quantile(bootstrap_rr, probs = 0.025)
upper_ci <- quantile(bootstrap_rr, probs = 0.975)

# Print the confidence intervals
print(c(lower_ci, upper_ci))







mod2.caap.alt <- step_func(ds=ds1, outcome_name='caap.cases', denom='pop.12', mod='negbin', other.covars='index')

plot.step.func(mod2.caap.alt)

ds1$index <- 1:nrow(ds1)

mod1a <- step_func(ds=ds1, outcome_name='caap.cases', denom='pop.12', mod='negbin', other.covars='none')

mod1b <- step_func(ds=ds1, outcome_name='caap.cases', denom='pop.12', mod='negbin', other.covars='index')
