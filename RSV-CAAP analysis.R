# Load necessary libraries
library(zoo)
library(htmlTable)
library(lme4)
library(lubridate)
library(reshape2)
library(MASS)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(tidyr)
library(kableExtra)

# Set the working directory (if needed)
setwd("C:/Users/Adrian/OneDrive - BGU/Documents/PIDU/RSV 2023/RSV-CAAP RD 2023")

# Source the external function file
source("./regression and plot function.R")


# Read the model data file
model_data <- read.csv("model_data.csv")

# Check
print(head(model_data))
str(model_data)

# Read dataset
ds1 <- read.csv('CAAP RSV under 60m.csv')

# Make sure the date column is in Date format and the data is sorted
ds1 <- ds1 %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# Define the unique time points
time_points1 <- unique(ds1$date)

# Check first few rows
print(head(model_data))
print(head(ds1))


# Loop through each row in the CSV and call the step_func
for (i in 1:nrow(model_data)) {

  cat("Processing:", model_data$model_name[i], "\n")
  
  # Call step_func with the required parameters
  result <- step_func(
    ds = ds1,
    outcome_name = model_data$outcome_name[i],
    denom = model_data$denom[i],
    N_CAAP = model_data$N_CAAP[i],
    N_tested = model_data$N_tested[i],
    mod = model_data$mod[i],
    other.covars = model_data$other_covars[i],
    group = model_data$group[i]
  )
  
  assign(model_data$model_name[i], result)
}





# Table AIC values; models with and models without covariate
comparison_table <- data.frame(
  Model = character(),
  AIC_without_covariate = numeric(),
  AIC_with_covariate = numeric(),
  stringsAsFactors = FALSE
)

# Loop through the models
n <- nrow(model_data) / 2
for (i in 1:n) {
  base_model_name <- model_data$model_name[i]
  cov_model_name <- model_data$model_name[i + n]
  
  aic_without <- round(get(base_model_name)[["aic1"]], 1)
  aic_with <- round(get(cov_model_name)[["aic1"]], 1)
  
  model_display_name <- model_data$name[i]
  
    comparison_table <- rbind(comparison_table, data.frame(
      Model = model_display_name,
      AIC_without_covariate = aic_without,
      AIC_with_covariate = aic_with
    ))
}

AIC_table <- kbl(comparison_table, escape = FALSE, row.names = FALSE) %>%
  kable_styling(full_width = FALSE, position = "center", font_size = 12) %>%
  add_header_above(c(" " = 1, "Comparison of AIC's of models without and models with covariate time" = 2))

AIC_table






# Table Rate Ratio at end of Late PCV Period
Rate_Ratios_Late_Period <- list()

# Loop through each model to extract rate ratios
for (i in 1:nrow(model_data)) {
  model_name <- model_data$model_name[i]
  
  # Check if the model is a non-covariate model (does not end with .cov)
  if (!grepl("\\.cov$", model_name)) {
    # Retrieve the model object
    model_obj <- get(model_name)
    
    # Extract the rr.q.t list and get the values from the last row (row 180)
    rr_q_t <- model_obj$rr.q.t
    rr_last_row_values <- rr_q_t[180, ]
    
    model_display_name <- model_data$name[i]
    
    Rate_Ratios_Late_Period[[model_display_name]] <- rr_last_row_values
  }
}

Rate_Ratios_Late_Period_df <- do.call(rbind, Rate_Ratios_Late_Period)
Rate_Ratios_Late_Period_df <- as.data.frame(Rate_Ratios_Late_Period_df)
colnames(Rate_Ratios_Late_Period_df) <- c("Rate_Ratio_LCI", "Rate_Ratio_Median", "Rate_Ratio_UCI")

Rate_Ratios_Late_Period_df <- Rate_Ratios_Late_Period_df[, c("Rate_Ratio_Median", "Rate_Ratio_LCI", "Rate_Ratio_UCI")]
Rate_Ratios_Late_Period_df <- round(Rate_Ratios_Late_Period_df, 1)

styled_rr_table <- kbl(Rate_Ratios_Late_Period_df, escape = FALSE) %>%
  kable_styling(full_width = FALSE, position = "center", font_size = 12) %>%
  add_header_above(c(" " = 1, "Rate Ratios at the end of the Late PCV Period" = 3))

styled_rr_table




# Table Cumulative Incidence at the end of the Late PCV Period
Cumulative_Incidence_Stable <- list()

# Loop through each model to extract cumulative incidence
for (i in 1:nrow(model_data)) {
  model_name <- model_data$model_name[i]
  
  # Check if the model is a non-covariate model (does not end with .cov)
  if (!grepl("\\.cov$", model_name)) {
    # Retrieve the model object
    model_obj <- get(model_name)
    
    cum_post_inc_t_q_stable <- model_obj$cum.post.inc.t.q.stable
    
    # Drop the date column if it exists
    if ("date" %in% colnames(cum_post_inc_t_q_stable)) {
      cum_post_inc_t_q_stable <- cum_post_inc_t_q_stable[, !colnames(cum_post_inc_t_q_stable) %in% "date"]
    }
    
    # Extract the values from the last row (row 180)
    cum_last_row_values <- cum_post_inc_t_q_stable[180, ]
    
    model_display_name <- model_data$name[i]
    
    Cumulative_Incidence_Stable[[model_display_name]] <- cum_last_row_values
  }
}

Cumulative_Incidence_Stable_df <- do.call(rbind, Cumulative_Incidence_Stable)
Cumulative_Incidence_Stable_df <- as.data.frame(Cumulative_Incidence_Stable_df)
colnames(Cumulative_Incidence_Stable_df) <- c("Cumulative_Inc_LCI", "Cumulative_Inc_Median", "Cumulative_Inc_UCI")

Cumulative_Incidence_Stable_df <- Cumulative_Incidence_Stable_df[, c("Cumulative_Inc_Median", "Cumulative_Inc_LCI", "Cumulative_Inc_UCI")]
Cumulative_Incidence_Stable_df <- round(Cumulative_Incidence_Stable_df, 1)

styled_ci_table <- kbl(Cumulative_Incidence_Stable_df, escape = FALSE) %>%
  kable_styling(full_width = FALSE, position = "center", font_size = 12) %>%
  add_header_above(c(" " = 1, "Cumulative Incidence per 1,000 at end of Late PCV Period" = 3))

styled_ci_table



