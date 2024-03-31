
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
ds1<- read.csv('C:/Users/Adrian/OneDrive - Ben Gurion University of the Negev/Documents/PIDU/RSV 2023/CAAP RSV Bronch/CAAP RSV Bronch.csv') 

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
aic <- t(sapply(list(mod.caap.12, mod.rsv.caap.12, mod.bronch.12, mod.rsv.bronch.12, mod.caap.12.23, mod.rsv.caap.12.23, mod.caap.24.59, mod.rsv.caap.24.59, mod.caap.60, mod.rsv.caap.60), '[[','aic1'))
aic

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

# CAAP under 12 months
ds1.plot <- ds1
ds1.plot$preds.cf.q.lower = mod.caap.12$preds.cf.q[, '2.5%']
ds1.plot$preds.cf.q = mod.caap.12$preds.cf.q[, '50%']
ds1.plot$preds.cf.q.upper = mod.caap.12$preds.cf.q[, '97.5%']
ds1.plot$preds.q = mod.caap.12$preds.q[, '50%']

pct.decline <- 100*(1-mod.caap.12$rr.q.post)
decline <- round(pct.decline["50%"], digits = 1)
lower <- round(pct.decline["97.5%"], digits = 1)
upper <- round(pct.decline["2.5%"], digits = 1)

label_text <- paste0(decline, "% (", lower, "%, ", upper, "%)")

ggplot_obj <- ggplot(data = ds1.plot, aes(x = date, y = caap.cases.12)) +
  geom_text(x = max(ds1.plot$date), y = 250, label = label_text, hjust = 1, vjust = 1, size = 4, color = "black") + 
  geom_line(linetype = "solid", color = "black", size = 0.8) +
  geom_line(aes(y = preds.q), color = "royalblue", linetype = "solid", size = 0.8) +
  geom_line(aes(y = preds.cf.q), color = "orangered", linetype = "solid", size = 0.8) +
  geom_ribbon(aes(ymin = preds.cf.q.lower, ymax = preds.cf.q.upper), fill = "orangered", alpha = 0.3) +
  labs(x = "Year", y = "CAAP Cases <12 months") +
  geom_vline(xintercept = as.numeric(as.Date("2009-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2011-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2015-07-01")), linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle("CAAP; Children under 12 months")

print(ggplot_obj)


# RSV-CAAP under 12 months
ds1.plot <- ds1
ds1.plot$preds.cf.q.lower = mod.rsv.caap.12$preds.cf.q[, '2.5%']
ds1.plot$preds.cf.q = mod.rsv.caap.12$preds.cf.q[, '50%']
ds1.plot$preds.cf.q.upper = mod.rsv.caap.12$preds.cf.q[, '97.5%']
ds1.plot$preds.q = mod.rsv.caap.12$preds.q[, '50%']

pct.decline <- 100*(1-mod.rsv.caap.12$rr.q.post)
decline <- round(pct.decline["50%"], digits = 1)
lower <- round(pct.decline["97.5%"], digits = 1)
upper <- round(pct.decline["2.5%"], digits = 1)

label_text <- paste0(decline, "% (", lower, "%, ", upper, "%)")

ggplot_obj <- ggplot(data = ds1.plot, aes(x = date, y = rsv.caap.cases.12)) +
  geom_text(x = max(ds1.plot$date), y = 95, label = label_text, hjust = 1, vjust = 1, size = 4, color = "black") + 
  geom_line(color = "black", size = 0.8) +
  geom_line(aes(y = preds.q), color = "royalblue") +
  geom_line(aes(y = preds.cf.q), color = "orangered") +
  geom_ribbon(aes(ymin = preds.cf.q.lower, ymax = preds.cf.q.upper), fill = "orangered", alpha = 0.3) +
  labs(x = "Year", y = "RSV-CAAP Cases <12 months") +
  geom_vline(xintercept = as.numeric(as.Date("2009-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2011-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2015-07-01")), linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle("RSV-CAAP; Children under 12 months")

print(ggplot_obj)


# Bronchiolitis under 12 months
ds1.plot <- ds1
ds1.plot$preds.cf.q.lower = mod.bronch.12$preds.cf.q[, '2.5%']
ds1.plot$preds.cf.q = mod.bronch.12$preds.cf.q[, '50%']
ds1.plot$preds.cf.q.upper = mod.bronch.12$preds.cf.q[, '97.5%']
ds1.plot$preds.q = mod.bronch.12$preds.q[, '50%']

pct.decline <- 100*(1-mod.bronch.12$rr.q.post)
decline <- round(pct.decline["50%"], digits = 1)
lower <- round(pct.decline["97.5%"], digits = 1)
upper <- round(pct.decline["2.5%"], digits = 1)

label_text <- paste0(decline, "% (", lower, "%, ", upper, "%)")

ggplot_obj <- ggplot(data = ds1.plot, aes(x = date, y = bronch.cases.12)) +
  geom_text(x = max(ds1.plot$date), y = 350, label = label_text, hjust = 1, vjust = 1, size = 4, color = "black") + 
  geom_line(color = "black", size = 0.8) +
  geom_line(aes(y = preds.q), color = "royalblue") +
  geom_line(aes(y = preds.cf.q), color = "orangered") +
  geom_ribbon(aes(ymin = preds.cf.q.lower, ymax = preds.cf.q.upper), fill = "orangered", alpha = 0.3) +
  labs(x = "Year", y = "Bronchiolitis Cases <12 months") +
  geom_vline(xintercept = as.numeric(as.Date("2009-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2011-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2015-07-01")), linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle("Bronchiolitis; Children under 12 months")

print(ggplot_obj)

# RSV-Bronchiolitis under 12 months
ds1.plot <- ds1
ds1.plot$preds.cf.q.lower = mod.rsv.bronch.12$preds.cf.q[, '2.5%']
ds1.plot$preds.cf.q = mod.rsv.bronch.12$preds.cf.q[, '50%']
ds1.plot$preds.cf.q.upper = mod.rsv.bronch.12$preds.cf.q[, '97.5%']
ds1.plot$preds.q = mod.rsv.bronch.12$preds.q[, '50%']

pct.decline <- 100*(1-mod.rsv.bronch.12$rr.q.post)
decline <- round(pct.decline["50%"], digits = 1)
lower <- round(pct.decline["97.5%"], digits = 1)
upper <- round(pct.decline["2.5%"], digits = 1)

label_text <- paste0(decline, "% (", lower, "%, ", upper, "%)")

ggplot_obj <- ggplot(data = ds1.plot, aes(x = date, y = rsv.bronch.cases.12)) +
  geom_text(x = max(ds1.plot$date), y = 300, label = label_text, hjust = 1, vjust = 1, size = 4, color = "black") + 
  geom_line(color = "black", size = 0.8) +
  geom_line(aes(y = preds.q), color = "royalblue") +
  geom_line(aes(y = preds.cf.q), color = "orangered") +
  geom_ribbon(aes(ymin = preds.cf.q.lower, ymax = preds.cf.q.upper), fill = "orangered", alpha = 0.3) +
  labs(x = "Year", y = "RSV-Bronchiolitis Cases <12 months") +
  geom_vline(xintercept = as.numeric(as.Date("2009-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2011-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2015-07-01")), linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle("RSV-Bronchiolitis; Children under 12 months")

print(ggplot_obj)


# CAAP 12-23 months
ds1.plot <- ds1
ds1.plot$preds.cf.q.lower = mod.caap.12.23$preds.cf.q[, '2.5%']
ds1.plot$preds.cf.q = mod.caap.12.23$preds.cf.q[, '50%']
ds1.plot$preds.cf.q.upper = mod.caap.12.23$preds.cf.q[, '97.5%']
ds1.plot$preds.q = mod.caap.12.23$preds.q[, '50%']

pct.decline <- 100*(1-mod.caap.12.23$rr.q.post)
decline <- round(pct.decline["50%"], digits = 1)
lower <- round(pct.decline["97.5%"], digits = 1)
upper <- round(pct.decline["2.5%"], digits = 1)

label_text <- paste0(decline, "% (", lower, "%, ", upper, "%)")

ggplot_obj <- ggplot(data = ds1.plot, aes(x = date, y = caap.cases.12.23)) +
  geom_text(x = max(ds1.plot$date), y = 40, label = label_text, hjust = 1, vjust = 1, size = 4, color = "black") + 
  geom_line(color = "black", size = 0.8) +
  geom_line(aes(y = preds.q), color = "royalblue") +
  geom_line(aes(y = preds.cf.q), color = "orangered") +
  geom_ribbon(aes(ymin = preds.cf.q.lower, ymax = preds.cf.q.upper), fill = "orangered", alpha = 0.3) +
  labs(x = "Year", y = "CAAP Cases 12-23 months") +
  geom_vline(xintercept = as.numeric(as.Date("2009-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2011-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2015-07-01")), linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle("CAAP; Children 12-23 months")

print(ggplot_obj)


# RSV-CAAP 12-23 months
ds1.plot <- ds1
ds1.plot$preds.cf.q.lower = mod.rsv.caap.12.23$preds.cf.q[, '2.5%']
ds1.plot$preds.cf.q = mod.rsv.caap.12.23$preds.cf.q[, '50%']
ds1.plot$preds.cf.q.upper = mod.rsv.caap.12.23$preds.cf.q[, '97.5%']
ds1.plot$preds.q = mod.rsv.caap.12.23$preds.q[, '50%']

pct.decline <- 100*(1-mod.rsv.caap.12.23$rr.q.post)
decline <- round(pct.decline["50%"], digits = 1)
lower <- round(pct.decline["97.5%"], digits = 1)
upper <- round(pct.decline["2.5%"], digits = 1)

label_text <- paste0(decline, "% (", lower, "%, ", upper, "%)")

ggplot_obj <- ggplot(data = ds1.plot, aes(x = date, y = rsv.caap.cases.12.23)) +
  geom_text(x = max(ds1.plot$date), y = 25, label = label_text, hjust = 1, vjust = 1, size = 4, color = "black") + 
  geom_line(color = "black", size = 0.8) +
  geom_line(aes(y = preds.q), color = "royalblue") +
  geom_line(aes(y = preds.cf.q), color = "orangered") +
  geom_ribbon(aes(ymin = preds.cf.q.lower, ymax = preds.cf.q.upper), fill = "orangered", alpha = 0.3) +
  labs(x = "Year", y = "RSV-CAAP Cases 12-23 months") +
  geom_vline(xintercept = as.numeric(as.Date("2009-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2011-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2015-07-01")), linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle("RSV-CAAP; Children 12-23 months")

print(ggplot_obj)


# CAAP 24-59 months
ds1.plot <- ds1
ds1.plot$preds.cf.q.lower = mod.caap.24.59$preds.cf.q[, '2.5%']
ds1.plot$preds.cf.q = mod.caap.24.59$preds.cf.q[, '50%']
ds1.plot$preds.cf.q.upper = mod.caap.24.59$preds.cf.q[, '97.5%']
ds1.plot$preds.q = mod.caap.24.59$preds.q[, '50%']

pct.decline <- 100*(1-mod.caap.24.59$rr.q.post)
decline <- round(pct.decline["50%"], digits = 1)
lower <- round(pct.decline["97.5%"], digits = 1)
upper <- round(pct.decline["2.5%"], digits = 1)

label_text <- paste0(decline, "% (", lower, "%, ", upper, "%)")

ggplot_obj <- ggplot(data = ds1.plot, aes(x = date, y = caap.cases.24.59)) +
  geom_text(x = max(ds1.plot$date), y = 40, label = label_text, hjust = 1, vjust = 1, size = 4, color = "black") + 
  geom_line(color = "black", size = 0.8) +
  geom_line(aes(y = preds.q), color = "royalblue") +
  geom_line(aes(y = preds.cf.q), color = "orangered") +
  geom_ribbon(aes(ymin = preds.cf.q.lower, ymax = preds.cf.q.upper), fill = "orangered", alpha = 0.3) +
  labs(x = "Year", y = "CAAP Cases 24-59 months") +
  geom_vline(xintercept = as.numeric(as.Date("2009-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2011-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2015-07-01")), linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle("CAAP; Children 24-59 months")

print(ggplot_obj)


# RSV-CAAP 24-59 months
ds1.plot <- ds1
ds1.plot$preds.cf.q.lower = mod.rsv.caap.24.59$preds.cf.q[, '2.5%']
ds1.plot$preds.cf.q = mod.rsv.caap.24.59$preds.cf.q[, '50%']
ds1.plot$preds.cf.q.upper = mod.rsv.caap.24.59$preds.cf.q[, '97.5%']
ds1.plot$preds.q = mod.rsv.caap.24.59$preds.q[, '50%']

pct.decline <- 100*(1-mod.rsv.caap.24.59$rr.q.post)
decline <- round(pct.decline["50%"], digits = 1)
lower <- round(pct.decline["97.5%"], digits = 1)
upper <- round(pct.decline["2.5%"], digits = 1)

label_text <- paste0(decline, "% (", lower, "%, ", upper, "%)")

ggplot_obj <- ggplot(data = ds1.plot, aes(x = date, y = rsv.caap.cases.24.59)) +
  geom_text(x = max(ds1.plot$date), y = 15, label = label_text, hjust = 1, vjust = 1, size = 4, color = "black") + 
  geom_line(color = "black", size = 0.8) +
  geom_line(aes(y = preds.q), color = "royalblue") +
  geom_line(aes(y = preds.cf.q), color = "orangered") +
  geom_ribbon(aes(ymin = preds.cf.q.lower, ymax = preds.cf.q.upper), fill = "orangered", alpha = 0.3) +
  labs(x = "Year", y = "RSV-CAAP Cases 24-59 months") +
  geom_vline(xintercept = as.numeric(as.Date("2009-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2011-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2015-07-01")), linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle("RSV-CAAP; Children 24-59 months")

print(ggplot_obj)


# CAAP <60 months
ds1.plot <- ds1
ds1.plot$preds.cf.q.lower = mod.caap.60$preds.cf.q[, '2.5%']
ds1.plot$preds.cf.q = mod.caap.60$preds.cf.q[, '50%']
ds1.plot$preds.cf.q.upper = mod.caap.60$preds.cf.q[, '97.5%']
ds1.plot$preds.q = mod.caap.60$preds.q[, '50%']

pct.decline <- 100*(1-mod.caap.60$rr.q.post)
decline <- round(pct.decline["50%"], digits = 1)
lower <- round(pct.decline["97.5%"], digits = 1)
upper <- round(pct.decline["2.5%"], digits = 1)

label_text <- paste0(decline, "% (", lower, "%, ", upper, "%)")

ggplot_obj <- ggplot(data = ds1.plot, aes(x = date, y = caap.cases.60)) +
  geom_text(x = max(ds1.plot$date), y = 170, label = label_text, hjust = 1, vjust = 1, size = 4, color = "black") + 
  geom_line(color = "black", size = 0.8) +
  geom_line(aes(y = preds.q), color = "royalblue") +
  geom_line(aes(y = preds.cf.q), color = "orangered") +
  geom_ribbon(aes(ymin = preds.cf.q.lower, ymax = preds.cf.q.upper), fill = "orangered", alpha = 0.3) +
  labs(x = "Year", y = "CAAP Cases <60 months") +
  geom_vline(xintercept = as.numeric(as.Date("2009-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2011-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2015-07-01")), linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle("CAAP; Children under 60 months")

print(ggplot_obj)


# RSV-CAAP <60 months
ds1.plot <- ds1
ds1.plot$preds.cf.q.lower = mod.rsv.caap.60$preds.cf.q[, '2.5%']
ds1.plot$preds.cf.q = mod.rsv.caap.60$preds.cf.q[, '50%']
ds1.plot$preds.cf.q.upper = mod.rsv.caap.60$preds.cf.q[, '97.5%']
ds1.plot$preds.q = mod.rsv.caap.60$preds.q[, '50%']

pct.decline <- 100*(1-mod.rsv.caap.60$rr.q.post)
decline <- round(pct.decline["50%"], digits = 1)
lower <- round(pct.decline["97.5%"], digits = 1)
upper <- round(pct.decline["2.5%"], digits = 1)

label_text <- paste0(decline, "% (", lower, "%, ", upper, "%)")

ggplot_obj <- ggplot(data = ds1.plot, aes(x = date, y = rsv.caap.cases.60)) +
  geom_text(x = max(ds1.plot$date), y = 130, label = label_text, hjust = 1, vjust = 1, size = 4, color = "black") + 
  geom_line(color = "black", size = 0.8) +
  geom_line(aes(y = preds.q), color = "royalblue") +
  geom_line(aes(y = preds.cf.q), color = "orangered") +
  geom_ribbon(aes(ymin = preds.cf.q.lower, ymax = preds.cf.q.upper), fill = "orangered", alpha = 0.3) +
  labs(x = "Year", y = "RSV-CAAP Cases <60 months") +
  geom_vline(xintercept = as.numeric(as.Date("2009-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2011-07-01")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2015-07-01")), linetype = "dashed", color = "black") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle("RSV-CAAP; Children under 60 months")

print(ggplot_obj)
