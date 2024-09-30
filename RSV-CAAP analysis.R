# Load necessary libraries

library(lubridate)
library(MASS)
library(ggplot2)
library(dplyr)
library(kableExtra)


# Source the external function file
source("./regression and plot function.R")


# Read the model data file
model_data <- read.csv("./model_data_unadj_rsv.csv")

# Check
print(head(model_data))
str(model_data)

# Read dataset
ds1 <- read.csv("./CAAP RSV under 60m.csv")

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
Rate_Ratios_Late_Period_df <- round(Rate_Ratios_Late_Period_df, 2)

RR_table <- kbl(Rate_Ratios_Late_Period_df, escape = FALSE) %>%
  kable_styling(full_width = FALSE, position = "center", font_size = 12) %>%
  add_header_above(c(" " = 1, "Rate Ratios at the end of the Late PCV Period" = 3))

RR_table




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

Cum_Inc_table <- kbl(Cumulative_Incidence_Stable_df, escape = FALSE) %>%
  kable_styling(full_width = FALSE, position = "center", font_size = 12) %>%
  add_header_above(c(" " = 1, "Cumulative Incidence per 1,000 at end of Late PCV Period" = 3))

Cum_Inc_table









#Table 1 and supplementary table 2




# Ensure that all the model objects are stored in a list named `mod_list`
mod_list <- list(
  "6" = mod.rsv.caap.6,
  "6.11" = mod.rsv.caap.6.11,
  "12" = mod.rsv.caap.12,
  "12.23" = mod.rsv.caap.12.23,
  "24.59" = mod.rsv.caap.24.59,
  "60" = mod.rsv.caap.60
)

# Define the study years
study_years <- c(
  "2004-2005", "2005-2006", "2006-2007", "2007-2008", "2008-2009", 
  "2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", 
  "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019"
)

# Define the specific months for each year
specific_months <- list(
  "2004-2005" = c("Dec", "Jan", "Feb", "Mar"),
  "2005-2006" = c("Dec", "Jan", "Feb", "Mar", "Apr"),
  "2006-2007" = c("Nov", "Dec", "Jan", "Feb", "Mar"),
  "2007-2008" = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr"),
  "2008-2009" = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
  "2009-2010" = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"),
  "2010-2011" = c("Nov", "Dec", "Jan", "Feb", "Mar"),
  "2011-2012" = c("Dec", "Jan", "Feb", "Mar", "Apr", "May"),
  "2012-2013" = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr"),
  "2013-2014" = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"),
  "2014-2015" = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"),
  "2015-2016" = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"),
  "2016-2017" = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr"),
  "2017-2018" = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr"),
  "2018-2019" = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")
)

# Define a function to process each age group
process_age_group <- function(age_group, population_var, model) {
  # Access the relevant data
  caap_cases <- ds1[[paste0("caap.cases.", age_group)]]
  rsv_caap_cases <- ds1[[paste0("unadj.rsv.caap.cases.", age_group)]]
  extrapolated_rsv_caap_cases <- ds1[[paste0("rsv.caap.cases.", age_group)]]
  expected_rsv_caap_cases <- model$preds
  population <- ds1[[population_var]]
  
  # Generate a sequence of months from July 2004 to June 2019
  months <- seq(as.Date("2004-07-01"), as.Date("2019-06-01"), by = "month")
  
  # Create a sequence of year labels from "2004-2005" to "2018-2019"
  year_labels <- ifelse(format(months, "%m") %in% c("07", "08", "09", "10", "11", "12"),
                        paste0(format(months, "%Y"), "-", as.numeric(format(months, "%Y")) + 1),
                        paste0(as.numeric(format(months, "%Y")) - 1, "-", format(months, "%Y")))
  
  # Create a data frame with months, year labels, CAAP cases, RSV-CAAP cases, etc.
  caap_df <- data.frame(Month = months, Year = year_labels,
                        CAAP_Cases = caap_cases, 
                        RSV_CAAP_Cases = rsv_caap_cases,
                        Extrapolated_RSV_CAAP_Cases = extrapolated_rsv_caap_cases,
                        Expected_RSV_CAAP_Cases = expected_rsv_caap_cases,
                        MonthLabel = format(months, "%b"),
                        Population = population)
  
  # Filter the population for July of each study year
  july_population <- subset(caap_df, format(Month, "%m") == "07")[, c("Year", "Population")]
  
  ####### PART 1: Annual Table #########
  # Aggregate the CAAP and RSV-CAAP cases by year
  yearly_caap_sums <- aggregate(CAAP_Cases ~ Year, data = caap_df, sum)
  yearly_rsv_caap_sums <- aggregate(RSV_CAAP_Cases ~ Year, data = caap_df, sum)
  yearly_extrapolated_rsv_sums <- aggregate(Extrapolated_RSV_CAAP_Cases ~ Year, data = caap_df, sum)
  yearly_expected_rsv_sums <- aggregate(Expected_RSV_CAAP_Cases ~ Year, data = caap_df, function(x) round(sum(x)))
  
  # Merge the data
  annual_table <- merge(yearly_caap_sums, yearly_rsv_caap_sums, by = "Year", all.x = TRUE)
  annual_table <- merge(annual_table, yearly_extrapolated_rsv_sums, by = "Year", all.x = TRUE)
  annual_table <- merge(annual_table, yearly_expected_rsv_sums, by = "Year", all.x = TRUE)
  annual_table <- merge(annual_table, july_population, by = "Year", all.x = TRUE)
  
  # Calculate incidences
  annual_table$CAAP_Incidence <- round((annual_table$CAAP_Cases / annual_table$Population) * 1000, 1)
  annual_table$Expected_RSV_Incidence <- round((annual_table$Expected_RSV_CAAP_Cases / annual_table$Population) * 1000, 1)
  
  # Remove the Population column
  annual_table <- annual_table[, !(names(annual_table) %in% "Population")]
  
  # Save the annual table to CSV
  write.csv(annual_table, file = paste0("Annual_Table_", age_group, ".csv"), row.names = FALSE)
  
  ####### PART 2: Selected Months Table #########
  # Initialize data frame for specific months
  specific_month_sums <- data.frame(Year = study_years)
  
  for (year in study_years) {
    selected_months <- specific_months[[year]]
    filtered_df <- subset(caap_df, Year == year & MonthLabel %in% selected_months)
    yearly_sum_caap <- sum(filtered_df$CAAP_Cases)
    yearly_sum_rsv_caap <- sum(filtered_df$RSV_CAAP_Cases)
    yearly_sum_extrapolated_rsv <- sum(filtered_df$Extrapolated_RSV_CAAP_Cases)
    yearly_sum_expected_rsv <- round(sum(filtered_df$Expected_RSV_CAAP_Cases))
    
    specific_month_sums[specific_month_sums$Year == year, "All_CAAP_Sum"] <- yearly_sum_caap
    specific_month_sums[specific_month_sums$Year == year, "RSV_CAAP_Sum"] <- yearly_sum_rsv_caap
    specific_month_sums[specific_month_sums$Year == year, "Extrapolated_RSV_Sum"] <- yearly_sum_extrapolated_rsv
    specific_month_sums[specific_month_sums$Year == year, "Expected_RSV_Sum"] <- yearly_sum_expected_rsv
    
    # Adjust population
    num_selected_months <- length(selected_months)
    adjusted_population <- july_population$Population[july_population$Year == year] / 12 * num_selected_months
    specific_month_sums[specific_month_sums$Year == year, "Adjusted_Population"] <- adjusted_population
    
    specific_months_string <- paste(selected_months[1], "-", selected_months[length(selected_months)], sep = "")
    specific_month_sums[specific_month_sums$Year == year, "Selected_Months"] <- specific_months_string
  }
  
  # Reorder columns and calculate incidences
  specific_month_sums <- specific_month_sums[, c("Year", "Selected_Months", "All_CAAP_Sum", "RSV_CAAP_Sum", "Extrapolated_RSV_Sum", "Expected_RSV_Sum", "Adjusted_Population")]
  specific_month_sums$CAAP_Incidence <- round((specific_month_sums$All_CAAP_Sum / specific_month_sums$Adjusted_Population) * 1000, 1)
  specific_month_sums$Expected_RSV_Incidence <- round((specific_month_sums$Expected_RSV_Sum / specific_month_sums$Adjusted_Population) * 1000, 1)
  
  # Remove adjusted population column and save
  specific_month_sums <- specific_month_sums[, !(names(specific_month_sums) %in% "Adjusted_Population")]
  write.csv(specific_month_sums, file = paste0("Seasonal_Table_", age_group, ".csv"), row.names = FALSE)
  
  cat("Tables saved for age group", age_group, "\n")
}

# List of age groups and their respective population variables
age_groups <- list("6" = "pop.12", "6.11" = "pop.12", "12" = "pop.12", "12.23" = "pop.12.23", "24.59" = "pop.24.59", "60" = "pop.60")

# Loop over each age group and process them
for (age_group in names(age_groups)) {
  process_age_group(age_group, age_groups[[age_group]], mod_list[[age_group]])
}



