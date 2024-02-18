
#Last date changed: 17-02-2024

library(ggplot2)
library(dplyr)
library(forecast)
library(kableExtra)




# Read data
RSV_CAAP_12 = read.csv("RSV CAAP under 12 months.csv")

head(RSV_CAAP_12)

str(RSV_CAAP_12)

# Change Excel dates to R dates
RSV_CAAP_12$Date <- as.Date(RSV_CAAP_12$Date)

str(RSV_CAAP_12)

# Sort by Date
RSV_CAAP_12 <- RSV_CAAP_12[order(RSV_CAAP_12$Date),]

# Line plot of the raw data
plot(RSV_CAAP_12$Date, RSV_CAAP_12$Inc_RSV_CAAP_12, type = "l", col = "grey40", lwd = 2,
     xlab = "Date", ylab = "Incidence/1,000", main = "RSV-CAAP incidence in children <12m")

# Plot trends in the Pre-, Early- and Late-PCV periods
plot <- ggplot(data = RSV_CAAP_12, aes(x = Date, y = Inc_RSV_CAAP_12)) +
  geom_line() +

  
  geom_smooth(data = subset(RSV_CAAP_12, Date >= as.Date("2004-07-01") & Date <= as.Date("2009-06-30")),
              method = "lm", formula = y ~ poly(x, 1), se = FALSE, color = "orangered") +
  geom_smooth(data = subset(RSV_CAAP_12, Date >= as.Date("2011-07-01") & Date <= as.Date("2015-06-30")),
              method = "lm", formula = y ~ poly(x, 1), se = FALSE, color = "orangered") +
  geom_smooth(data = subset(RSV_CAAP_12, Date >= as.Date("2015-07-01") & Date <= as.Date("2019-06-30")),
              method = "lm", formula = y ~ poly(x, 1), se = FALSE, color = "orangered") +
  
  
  labs(title = "RSV-CAAP incidence trend lines for Pre-, Early- and Late-PCV periods; Children <12m",
       x = "Date",
       y = "RSV-CAAP incidence/1,000") +
  theme_minimal()

print(plot)



# Variable first intervention (July 2011)
RSV_CAAP_12$Interv_1 <- ifelse(RSV_CAAP_12$Date >= as.Date("2004-07-01") & RSV_CAAP_12$Date < as.Date("2009-07-01"), 0,
                           ifelse(RSV_CAAP_12$Date >= as.Date("2009-07-01") & RSV_CAAP_12$Date < as.Date("2011-07-01"), NA,
                                  ifelse(RSV_CAAP_12$Date >= as.Date("2011-07-01") & RSV_CAAP_12$Date < as.Date("2019-07-01"), 1, NA)))

# Variable time from first intervention (July 2011)
RSV_CAAP_12 <- RSV_CAAP_12 %>%
  mutate(Time_Interv_1 = case_when(
    between(Date, as.Date("2004-07-01"), as.Date("2009-06-01")) ~ 0,
    between(Date, as.Date("2009-07-01"), as.Date("2011-06-01")) ~ NA_real_,
    between(Date, as.Date("2011-07-01"), as.Date("2019-06-01")) ~ row_number() - 84
  ))


# Variable time from stable CAAP incidence point (July 2015)
RSV_CAAP_12 <- RSV_CAAP_12 %>%
  mutate(Time_Interv_2 = case_when(
    between(Date, as.Date("2004-07-01"), as.Date("2009-06-01")) ~ 0,
    between(Date, as.Date("2009-07-01"), as.Date("2011-06-01")) ~ NA_real_,
    between(Date, as.Date("2011-07-01"), as.Date("2015-06-01")) ~ 0,
    between(Date, as.Date("2015-07-01"), as.Date("2019-06-01")) ~ row_number() - 132
  ))

# Convert CAAP incidence to time series object
ts_RSV_CAAP_12 <- ts(RSV_CAAP_12$Inc_RSV_CAAP_12, start = c(2004, 7), frequency = 12)

str(ts_RSV_CAAP_12)


# Decompose the time series
decomposition <- decompose(ts_RSV_CAAP_12)

# Examine the seasonal component of the time series
plot(decomposition)

# Extract the seasonal component
seasonal_component <- decomposition$seasonal

# Seasonally adjusted series
RSV_CAAP_12$seas_adj_RSV_CAAP_12 <- RSV_CAAP_12$Inc_RSV_CAAP_12 - seasonal_component



# Plot ACF and PACF un-adjusted model
par(mfrow=c(2, 1))  # Set up a 2x1 plotting grid
acf(RSV_CAAP_12$seas_adj_RSV_CAAP_12, main="ACF Plot (Un-adjusted)", lag.max=20)
pacf(RSV_CAAP_12$seas_adj_RSV_CAAP_12, main="PACF Plot (Un-adjusted)", lag.max=20)
par(mfrow=c(1, 1))


# Grid search ARIMA (p, q, d)

# Set the ranges for p, d, q
p_values <- 0:3
d_values <- 0:1
q_values <- 0:3

# Variables for lowest AIC, corresponding model, and order
lowest_aic <- Inf
selected_arima_model <- NULL
selected_arima_order <- c(0, 0, 0)

# Empty data frame to store results
results_table <- data.frame(p = numeric(0), d = numeric(0), q = numeric(0), AIC = numeric(0))

# Fit ARIMA(0,0,0) model (unadjusted model)
unadjusted_model <- arima(RSV_CAAP_12$seas_adj_RSV_CAAP_12, order = c(0, 0, 0))
unadjusted_aic <- AIC(unadjusted_model)

# Store results for the unadjusted model in the data frame
results_table <- rbind(results_table, c(0, 0, 0, unadjusted_aic))


for (p in p_values) {
  for (d in d_values) {
    for (q in q_values) {
      # Fit ARIMA model
      current_order <- c(p, d, q)
      current_arima_model <- arima(RSV_CAAP_12$seas_adj_RSV_CAAP_12, order = current_order)
      
      # Calculate AIC
      current_aic <- AIC(current_arima_model)
      
      # Store results in the data frame
      results_table <- rbind(results_table, c(p, d, q, current_aic))
      
      # Check if current AIC is lower than the lowest AIC
      if (current_aic < lowest_aic) {
        lowest_aic <- current_aic
        selected_arima_model <- current_arima_model
        selected_arima_order <- current_order
      }
    }
  }
}


colnames(results_table) <- c("p", "d", "q", "AIC")

# Sort
results_table <- results_table[order(results_table$AIC), ]

results_table$Rank <- seq_len(nrow(results_table))

results_table <- results_table[, c("Rank", names(results_table)[1:4])]

# Print table
kable(head(results_table, 5), format = "markdown", row.names = FALSE) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "scale_down") %>%
  add_header_above(c(" " = 1, "Five ARIMA p, d, q orders with lowest AIC" = 4))




# Fit the model
arima_model <- arima(RSV_CAAP_12$seas_adj_RSV_CAAP_12, order = c(0,0,1))


# Plot ACF and PACF of the residuals
par(mfrow = c(2, 1))
acf(residuals(arima_model), main = "ACF of Residuals")
pacf(residuals(arima_model), main = "PACF of Residuals")
par(mfrow=c(1, 1))

# Box-Ljung test
residuals <- residuals(arima_model)

box_ljung_test <- Box.test(residuals, type = "Ljung-Box", lag = 20)

print(box_ljung_test)



# Extract the fitted values from the ARIMA model
fitted_RSV_CAAP_12 <- fitted(arima_model)

# Add the fitted values to CAAP_12 data frame
RSV_CAAP_12$fitted_RSV_CAAP_12 <- fitted_RSV_CAAP_12

head(RSV_CAAP_12)



# Fit linear regression through fitted values with intervention points
lin_RSV_CAAP_12 <- lm(fitted_RSV_CAAP_12 ~ Time + Interv_1 + Time_Interv_1 + Time_Interv_2, data = RSV_CAAP_12[(RSV_CAAP_12$Time<61 | RSV_CAAP_12$Time>84 ), ])
summary_lin_RSV_CAAP_12 <- summary(lin_RSV_CAAP_12)
summary_lin_RSV_CAAP_12


# Confidence intervals of the model coefficients
confint_RSV_CAAP_12 <- confint.default(lin_RSV_CAAP_12, level = 0.95)


# Trends and level change
trends_table_RSV_CAAP_12 <- data.frame(
  Parameter = c("Pre-PCV period trend", "Level change Early-PCV vs. Pre-PCV", "Early-PCV trend", "Late-PCV trend"),
  Coefficient = round(c(
    summary(lin_RSV_CAAP_12)$coefficients[2,1],
    summary(lin_RSV_CAAP_12)$coefficients[3,1],
    (summary(lin_RSV_CAAP_12)$coefficients[4,1]) + (summary(lin_RSV_CAAP_12)$coefficients[2,1]),
    (summary(lin_RSV_CAAP_12)$coefficients[5,1]) + (summary(lin_RSV_CAAP_12)$coefficients[4,1]) + (summary(lin_RSV_CAAP_12)$coefficients[2,1])
  ), 3),
  CI_Lower = round(c(
    confint_RSV_CAAP_12[2,1],
    confint_RSV_CAAP_12[3,1],
    confint_RSV_CAAP_12[4,1] + confint_RSV_CAAP_12[2,1],
    confint_RSV_CAAP_12[5,1] + confint_RSV_CAAP_12[4,1] + confint_RSV_CAAP_12[2,1]
  ), 3),
  CI_Upper = round(c(
    confint_RSV_CAAP_12[2,2],
    confint_RSV_CAAP_12[3,2],
    confint_RSV_CAAP_12[4,2] + confint_RSV_CAAP_12[2,2],
    confint_RSV_CAAP_12[5,2] + confint_RSV_CAAP_12[4,2] + confint_RSV_CAAP_12[2,2]
  ), 3),
  p_value = round(c(
    summary(lin_RSV_CAAP_12)$coefficients[2,4],
    summary(lin_RSV_CAAP_12)$coefficients[3,4],
    summary(lin_RSV_CAAP_12)$coefficients[4,4],
    summary(lin_RSV_CAAP_12)$coefficients[5,4]
  ), 3)
)

names(trends_table_RSV_CAAP_12)[1:5] <- c("Period", "Trend", "Lower 95% CI", "Upper 95% CI", "p-value")

# Print table
kable(
  trends_table_RSV_CAAP_12,
  align = c("l", "c", "c", "c", "c"),
  col.names = c("Period", "Trend", "Lower 95% CI", "Upper 95% CI", "p value"),
  caption = "Trends and Level Change"
) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "scale_down")




# Calculate predicted values and confidence intervals
pred_RSV_CAAP_12 <- predict(lin_RSV_CAAP_12, interval = "confidence", level = 0.95)

# Add predicted values and confidence intervals to the data frame
RSV_CAAP_12$pred_values <- NA
RSV_CAAP_12$lower_ci <- NA
RSV_CAAP_12$upper_ci <- NA

RSV_CAAP_12[(RSV_CAAP_12$Time < 61 | RSV_CAAP_12$Time > 84), c("pred_values", "lower_ci", "upper_ci")] <- pred_RSV_CAAP_12


head(RSV_CAAP_12)


# PLot
RSV_CAAP_12_plot <- ggplot(RSV_CAAP_12, aes(x = Date, y = Inc_RSV_CAAP_12)) +
  geom_line(aes(y = Inc_RSV_CAAP_12, color = "RSV-CAAP incidence"), lwd = 1) +
  geom_line(aes(y = pred_values, color = "Trend"), lwd = 1) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = "Trend 95% CI"), alpha = 0.3, color = "royalblue2") +
  labs(title = "RSV-CAAP incidence plus trend; children under 12m",
       x = "Year", y = "Incidence/1,000") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.box.background = element_rect(color = "black", size = 1, linetype = "solid"),
        panel.grid = element_blank()) +  # Remove grid lines
  scale_fill_manual(values = c("Trend 95% CI" = "royalblue2")) +
  scale_color_manual(values = c("RSV-CAAP incidence" = "black", "Trend" = "royalblue2")) +
  scale_alpha_manual(values = c("Trend 95% CI" = 0.3)) +
  expand_limits(x = c(as.Date("2004-07-01"), as.Date("2019-06-30"))) +
  scale_x_date(expand = c(0, 0),
               breaks = as.Date(c("2004-07-01", "2005-07-01", "2006-07-01", "2007-07-01", "2008-07-01", "2009-07-01", "2010-07-01", "2011-07-01", "2012-07-01", "2013-07-01", "2014-07-01", "2015-07-01", "2016-07-01", "2017-07-01", "2018-07-01")),
               labels = c("04-05", "05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19")) +
  # Lines begin & end of interim period
  geom_vline(xintercept = as.Date("2009-07-01"), col = "grey40", lty = 2) +
  geom_vline(xintercept = as.Date("2011-07-01"), col = "grey40", lty = 2) +
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  labs(fill = NULL, color = NULL, alpha = NULL)

print(RSV_CAAP_12_plot)




# Subset Pre-PCV period
subset_RSV_CAAP_12 <- subset(RSV_CAAP_12, Date >= as.Date("2004-07-01") & Date <= as.Date("2009-06-30"))

# Fit the linear regression model
Pre_PCV_RSV_CAAP_12_model <- lm(subset_RSV_CAAP_12$fitted_RSV_CAAP_12 ~ Date , 
                            data = subset_RSV_CAAP_12)

# Months for the counterfactual period  (July 2009 - June 2013)
date_sequence <- seq(as.Date("2009-07-01"), as.Date("2013-06-01"), by = "months")

# Dataframe with Date variable July 2009 through June 2013
counterfact_dates <- data.frame(
  Date = date_sequence
)

# predictions for the counterfactual period
predicted_values <- predict(Pre_PCV_RSV_CAAP_12_model, newdata = counterfact_dates, interval = "confidence", level = 0.95)

# Temporary data frame with the counterfactual trend and confidence intervals
counterfact_data_RSV_CAAP_12 <- data.frame(
  Date = counterfact_dates,
  Pred_counterfact_RSV_CAAP_12 = predicted_values[, 1],
  Counterfact_Lower_CI = predicted_values[, 2],
  Counterfact_Upper_CI = predicted_values[, 3]
)

# Merge counterfactual data frame with original data frame
RSV_CAAP_12 <- merge(RSV_CAAP_12, counterfact_data_RSV_CAAP_12, by = "Date", all.x = TRUE)


# Prevent "gap" in plot between June 2009 and July 2009
RSV_CAAP_12$pred_values[RSV_CAAP_12$Date == as.Date("2009-07-01")] <- RSV_CAAP_12$Pred_counterfact_RSV_CAAP_12[RSV_CAAP_12$Date == as.Date("2009-07-01")]
RSV_CAAP_12$lower_ci[RSV_CAAP_12$Date == as.Date("2009-07-01")] <- RSV_CAAP_12$Counterfact_Lower_CI[RSV_CAAP_12$Date == as.Date("2009-07-01")]
RSV_CAAP_12$upper_ci[RSV_CAAP_12$Date == as.Date("2009-07-01")] <- RSV_CAAP_12$Counterfact_Upper_CI[RSV_CAAP_12$Date == as.Date("2009-07-01")]

# Stable Expected trend from July 2013
date_to_copy_from <- as.Date("2013-06-01")
dates_to_copy_to <- seq(as.Date("2013-07-01"), as.Date("2019-06-01"), by = "months")

RSV_CAAP_12$Pred_counterfact_RSV_CAAP_12[RSV_CAAP_12$Date %in% dates_to_copy_to] <- RSV_CAAP_12$Pred_counterfact_RSV_CAAP_12[RSV_CAAP_12$Date == date_to_copy_from]
RSV_CAAP_12$Counterfact_Lower_CI[RSV_CAAP_12$Date %in% dates_to_copy_to] <- RSV_CAAP_12$Counterfact_Lower_CI[RSV_CAAP_12$Date == date_to_copy_from]
RSV_CAAP_12$Counterfact_Upper_CI[RSV_CAAP_12$Date %in% dates_to_copy_to] <- RSV_CAAP_12$Counterfact_Upper_CI[RSV_CAAP_12$Date == date_to_copy_from]




# Plot and save
png("CAAP under 12 months plot.png", width = 800, height = 600)


RSV_CAAP_12_plus_expected <- ggplot(RSV_CAAP_12, aes(x = Date, y = Inc_RSV_CAAP_12)) +
  geom_line(aes(y = Inc_RSV_CAAP_12, color = "RSV-CAAP incidence"), lwd = 1) +
  geom_line(aes(y = pred_values, color = "Trend"), lwd = 1) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = "Trend 95% CI"), alpha = 0.3, color = "royalblue2") +
  geom_line(aes(y = Pred_counterfact_RSV_CAAP_12, color = "Expected trend"), lwd = 1) +
  geom_ribbon(aes(ymin = Counterfact_Lower_CI, ymax = Counterfact_Upper_CI, fill = "Expected trend 95% CI"), alpha = 0.3, color = "orangered2") +
  labs(title = "RSV-CAAP incidence plus trend; children under 12m",
       x = "Year", y = "Incidence/1,000") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.box.background = element_rect(color = "black", size = 1, linetype = "solid"),
        panel.grid = element_blank()) +
  scale_fill_manual(values = c("Expected trend 95% CI" = "orangered2", "Trend 95% CI" = "royalblue2")) +
  scale_color_manual(values = c("RSV-CAAP incidence" = "black", "Trend" = "royalblue2", "Expected trend" = "orangered2")) +
  scale_alpha_manual(values = c("Trend 95% CI" = 0.3)) +  # Adjust alpha here
  expand_limits(x = c(as.Date("2004-07-01"), as.Date("2019-06-30"))) +
  scale_x_date(expand = c(0, 0),
               breaks = as.Date(c("2004-07-01", "2005-07-01", "2006-07-01", "2007-07-01", "2008-07-01", "2009-07-01", "2010-07-01", "2011-07-01", "2012-07-01", "2013-07-01", "2014-07-01", "2015-07-01", "2016-07-01", "2017-07-01", "2018-07-01")),
               labels = c("04-05", "05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19")) +
  # Lines begin & end of interim period
  geom_vline(xintercept = as.Date("2009-07-01"), col = "grey40", lty = 2) +
  geom_vline(xintercept = as.Date("2011-07-01"), col = "grey40", lty = 2) +
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  labs(fill = NULL, color = NULL, alpha = NULL)  # Remove legend titles

print(RSV_CAAP_12_plus_expected)


dev.off()