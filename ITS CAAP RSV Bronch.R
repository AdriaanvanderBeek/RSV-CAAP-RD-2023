library(zoo)
library(htmlTable)
library(lme4)
library(lubridate)
library(reshape2)
library(MASS)
library(ggplot2)
library(dplyr)
source('./functions_its.R')


# Read data
ds1<- read.csv('./CAAP RSV Bronch.csv') 

names(ds1)

str(ds1)

# change and sort date
ds1 <- ds1 %>%
  mutate( date  = as.Date(date) ) %>%
  arrange(date)

str(ds1)


time_points1 <- unique(ds1$date)



mod.caap.12 <- step_func(ds=ds1, outcome_name='caap.cases.12', denom='pop.12', mod='negbin', other.covars='none')
mod.rsv.caap.12 <- step_func(ds=ds1, outcome_name='rsv.caap.cases.12', denom='pop.12', mod='negbin', other.covars='none')
mod.bronch.12 <- step_func(ds=ds1, outcome_name='bronch.cases.12', denom='pop.12', mod='negbin', other.covars='none')
mod.rsv.bronch.12 <- step_func(ds=ds1, outcome_name='rsv.bronch.cases.12', denom='pop.12', mod='negbin', other.covars='none')
mod.caap.12.23 <- step_func(ds=ds1, outcome_name='caap.cases.12.23', denom='pop.12.23', mod='negbin', other.covars='none')

mod.rsv.caap.12.23 <- step_func(ds=ds1, outcome_name='rsv.caap.cases.12.23', denom='pop.12.23', mod='negbin', other.covars='none')

mod.caap.24.59 <- step_func(ds=ds1, outcome_name='caap.cases.24.59', denom='pop.24.59', mod='negbin', other.covars='none')
mod.rsv.caap.24.59 <- step_func(ds=ds1, outcome_name='rsv.caap.cases.24.59', denom='pop.24.59', mod='negbin', other.covars='none')
mod.caap.60 <- step_func(ds=ds1, outcome_name='caap.cases.60', denom='pop.60', mod='negbin', other.covars='none')
mod.rsv.caap.60 <- step_func(ds=ds1, outcome_name='rsv.caap.cases.60', denom='pop.60', mod='negbin', other.covars='none')

# Extract results
rr <- t(sapply(list(mod.caap.12, mod.rsv.caap.12, mod.bronch.12, mod.rsv.bronch.12, mod.caap.12.23, mod.rsv.caap.12.23, mod.caap.24.59, mod.rsv.caap.24.59, mod.caap.60, mod.rsv.caap.60), '[[','rr.q.post'))
round(rr,2)
#aic <- t(sapply(list(mod.caap.12, mod.rsv.caap.12, mod.bronch.12, mod.rsv.bronch.12, mod.caap.12.23, mod.rsv.caap.12.23, mod.caap.24.59, mod.rsv.caap.24.59, mod.caap.60, mod.rsv.caap.60), '[[','aic1'))
#aic

models <- list(mod.caap.12, mod.rsv.caap.12, mod.bronch.12, mod.rsv.bronch.12, mod.caap.12.23, mod.rsv.caap.12.23, mod.caap.24.59, mod.rsv.caap.24.59, mod.caap.60, mod.rsv.caap.60)

# Create a table of RRs
rr.all <- round(t(sapply(models, '[[','rr.q.post')),2)
row.names(rr.all) <- c('All cause CAAP <12m','RSV CAAP <12m','Bronchiolitis <12m', 'RSV Bronchiolitis <12m', 'All cause CAAP 12-23m', 'RSV CAAP 12-23m', 'All cause CAAP 24-59m', 'RSV CAAP 24-59m', 'All cause CAAP <60m', 'RSV CAAP <60m')
rr.all

pct.decline.all <- 100*(1-rr.all)

PercentDecline <- paste0(pct.decline.all[,2], 
                         '%, (', 
                         pct.decline.all[,3] ,'%,',pct.decline.all[,1]  ,'%)'    )

htmlTable(cbind(row.names(pct.decline.all), PercentDecline), align = "l")





# Plotting for each outcome
mod.caap.12$p.preds.agg

mod.caap.12$p.preds

mod.caap.12$p.cum_prevented

mod.caap.12$p.rr.trend

mod.rsv.bronch.12$p.preds.agg

mod.rsv.bronch.12$p.rr.trend


mod.rsv.caap.24.59$p.preds.agg

mod.rsv.caap.24.59$p.rr.trend


