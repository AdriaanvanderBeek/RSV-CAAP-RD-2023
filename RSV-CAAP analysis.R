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
setwd("C:/Users/Adrian/OneDrive - BGU/Documents/PIDU/RSV 2023/RSV-CAAP RD 2024/RSV-CAAP-RD-2023")

# Source the external function file
source("./regression and plot function.R")


# Read the model data file
model_data <- read.csv("model_data.csv")

# Print the data to verify it's loaded correctly
print(head(model_data))
str(model_data)

# Read dataset that the models will use
ds1 <- read.csv('CAAP RSV under 60m.csv')

# Make sure the date column is in Date format and the data is sorted
ds1 <- ds1 %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# Define the unique time points
time_points1 <- unique(ds1$date)

# Ensure all parameters are present in your model_data
# Print the first few rows to verify
print(head(model_data))
print(head(ds1))


# Loop through each row in the CSV and call the step_func
for (i in 1:nrow(model_data)) {
  # Output the progress message
  cat("Processing:", model_data$outcome_name[i], "\n")
  
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
  
  # Assign the result to a variable named after the model
  assign(model_data$model_name[i], result)
}



# Initialize a dataframe to store the AIC values
comparison_table <- data.frame(
  Model = character(),
  AIC_without_cov = numeric(),
  AIC_with_cov = numeric(),
  stringsAsFactors = FALSE
)

# Loop through the models
n <- nrow(model_data) / 2
for (i in 1:n) {
  base_model_name <- model_data$model_name[i]
  cov_model_name <- model_data$model_name[i + n]
  
  # Extract the AIC values and round them
  aic_without <- round(get(base_model_name)[["aic1"]], 1)
  aic_with <- round(get(cov_model_name)[["aic1"]], 1)
  
  # Store the results in the comparison table
  comparison_table <- rbind(comparison_table, data.frame(
    Model = base_model_name,
    AIC_without_cov = aic_without,
    AIC_with_cov = aic_with
  ))
}

# Create the styled table
AIC_table <- kbl(comparison_table, escape = FALSE) %>%
  kable_styling(full_width = FALSE)

# Display the styled table
AIC_table









# Initialize lists to store the results
Rate_Ratios_Late_Period <- list()
Cumulative_Incidence_Stable <- list()

# Loop through each model name in model_data
for (i in 1:nrow(model_data)) {
  model_name <- model_data$model_name[i]
  
  # Retrieve the model object
  model_obj <- get(model_name)
  
  # Extract the rr.q.t list and get the values from the last row (row 180)
  rr_q_t <- model_obj$rr.q.t
  rr_last_row_values <- rr_q_t[180, ]
  Rate_Ratios_Late_Period[[model_name]] <- rr_last_row_values
  
  # Extract the cum.post.inc.t.q.stable list
  cum_post_inc_t_q_stable <- model_obj$cum.post.inc.t.q.stable
  
  # Drop the date column if it exists
  if ("date" %in% colnames(cum_post_inc_t_q_stable)) {
    cum_post_inc_t_q_stable <- cum_post_inc_t_q_stable[, !colnames(cum_post_inc_t_q_stable) %in% "date"]
  }
  
  # Extract the values from the last row (row 180)
  cum_last_row_values <- cum_post_inc_t_q_stable[180, ]
  Cumulative_Incidence_Stable[[model_name]] <- cum_last_row_values
}

# Convert the extracted values lists to data frames for easier handling
Rate_Ratios_Late_Period_df <- do.call(rbind, Rate_Ratios_Late_Period)
Rate_Ratios_Late_Period_df <- as.data.frame(Rate_Ratios_Late_Period_df)
colnames(Rate_Ratios_Late_Period_df) <- c("Rate_Ratio_LCI", "Rate_Ratio_Median", "Rate_Ratio_UCI")

Cumulative_Incidence_Stable_df <- do.call(rbind, Cumulative_Incidence_Stable)
Cumulative_Incidence_Stable_df <- as.data.frame(Cumulative_Incidence_Stable_df)
colnames(Cumulative_Incidence_Stable_df) <- c("Cumulative_Inc_LCI", "Cumulative_Inc_Median", "Cumulative_Inc_UCI")

# Print the extracted values
print(Rate_Ratios_Late_Period_df)
print(Cumulative_Incidence_Stable_df)

# Write the results to CSV files for further analysis
write.csv(Rate_Ratios_Late_Period_df, "Rate_Ratios_RSV_Late_Period_with ratio offset.csv", row.names = TRUE)
write.csv(Cumulative_Incidence_Stable_df, "Cumulative_Incidence_Stable.csv", row.names = TRUE)


