
rm(list = ls())
# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

dogOwnerDistance <- read.csv("data/JoelDistance.csv")
household_survey <- read.csv("data/household_survey_summary.csv")

# Enhanced statistical comparison with 95% CI and component breakdown
household_time_by_strategy <- household_survey %>%
  group_by(strategy, vaccination_type) %>%
  summarize(
    n_households = n(),
    
    # Total time statistics
    mean_total_time = mean(total_household_time, na.rm = TRUE),
    median_total_time = median(total_household_time, na.rm = TRUE),
    sd_total_time = sd(total_household_time, na.rm = TRUE),
    se_total_time = sd_total_time / sqrt(n_households),  # Standard error
    ci_lower_total = mean_total_time - qt(0.975, n_households-1) * se_total_time,  # 95% CI lower
    ci_upper_total = mean_total_time + qt(0.975, n_households-1) * se_total_time,  # 95% CI upper
    
    # Travel time statistics (for central point only)
    mean_travel_time = mean(travel_time, na.rm = TRUE),
    median_travel_time = median(travel_time, na.rm = TRUE),
    sd_travel_time = sd(travel_time, na.rm = TRUE),
    se_travel_time = sd_travel_time / sqrt(sum(!is.na(travel_time))),
    ci_lower_travel = mean_travel_time - qt(0.975, sum(!is.na(travel_time))-1) * se_travel_time,
    ci_upper_travel = mean_travel_time + qt(0.975, sum(!is.na(travel_time))-1) * se_travel_time,
    
    # Waiting time statistics (for central point only)
    mean_waiting_time = mean(waiting_time, na.rm = TRUE),
    median_waiting_time = median(waiting_time, na.rm = TRUE),
    sd_waiting_time = sd(waiting_time, na.rm = TRUE),
    se_waiting_time = sd_waiting_time / sqrt(sum(!is.na(waiting_time))),
    ci_lower_waiting = mean_waiting_time - qt(0.975, sum(!is.na(waiting_time))-1) * se_waiting_time,
    ci_upper_waiting = mean_waiting_time + qt(0.975, sum(!is.na(waiting_time))-1) * se_waiting_time,
    
    # Range values
    min_time = min(total_household_time, na.rm = TRUE),
    max_time = max(total_household_time, na.rm = TRUE),
    .groups = "drop"
  )

print("Enhanced household time by vaccination strategy and type (with 95% CI):")
print(household_time_by_strategy)

# Create a formatted table with 95% CI as mean (95% CI)
formatted_household_time <- household_time_by_strategy %>%
  mutate(
    total_time_with_ci = sprintf("%.1f (%.1f-%.1f)", 
                                 mean_total_time, ci_lower_total, ci_upper_total),
    travel_time_with_ci = sprintf("%.1f (%.1f-%.1f)", 
                                  mean_travel_time, ci_lower_travel, ci_upper_travel),
    waiting_time_with_ci = sprintf("%.1f (%.1f-%.1f)", 
                                   mean_waiting_time, ci_lower_waiting, ci_upper_waiting)
  ) %>%
  dplyr::select(strategy, vaccination_type, n_households, 
         total_time_with_ci, travel_time_with_ci, waiting_time_with_ci)

print("Formatted household time summary with 95% CI:")
print(formatted_household_time)

# 3. Statistical tests for differences between strategies
# For central point vaccinations - total time
central_point_data <- filter(household_survey, vaccination_type == "Central point")
if(nrow(central_point_data) > 0 && length(unique(central_point_data$strategy)) > 1) {
  # Total household time test
  central_time_test <- t.test(
    total_household_time ~ strategy, 
    data = central_point_data
  )
  print("Total time difference for central point vaccination by strategy:")
  print(central_time_test)
  
  # Travel time test
  central_travel_test <- t.test(
    travel_time ~ strategy, 
    data = central_point_data
  )
  print("Travel time difference for central point vaccination by strategy:")
  print(central_travel_test)
  
  # Waiting time test
  central_waiting_test <- t.test(
    waiting_time ~ strategy, 
    data = central_point_data
  )
  print("Waiting time difference for central point vaccination by strategy:")
  print(central_waiting_test)
} else {
  print("Insufficient data for central point comparison across strategies")
}

# 4. Analysis of GPS distance data
# First, handle NAs and outliers
dogOwnerDistance <- dogOwnerDistance %>%
  # Remove rows with NA in TravelDistanceKM
  filter(!is.na(TravelDistanceKM)) %>%
  # Handle outliers using IQR method, grouped by delivery strategy type
  group_by(Delivery.strategy.type) %>%
  mutate(
    Q1 = quantile(TravelDistanceKM, 0.25, na.rm = TRUE),
    Q3 = quantile(TravelDistanceKM, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    is_outlier = TravelDistanceKM > (Q3 + 1.5 * IQR) | TravelDistanceKM < (Q1 - 1.5 * IQR),
    TravelDistanceKM_clean = ifelse(is_outlier, NA, TravelDistanceKM)
  ) %>%
  ungroup() %>%
  dplyr::select(-Q1, -Q3, -IQR)  # Remove temporary columns

# Count and report on outliers
outlier_summary <- dogOwnerDistance %>%
  group_by(Delivery.strategy.type) %>%
  summarize(
    total_observations = n(),
    outliers = sum(is_outlier, na.rm = TRUE),
    outlier_percentage = round(outliers / total_observations * 100, 1),
    .groups = "drop"
  )

print("Outlier summary by delivery strategy type:")
print(outlier_summary)


# Summarize clean distance data by strategy and grouped delivery approach
distance_summary <- dogOwnerDistance %>%
  filter(!is.na(TravelDistanceKM_clean)) %>%
  # Add a new column that groups delivery strategies
  mutate(
    delivery_group = case_when(
      Delivery.strategy.type %in% c("Central Point - Subvillage", "Central Point - Village") ~ "Central point",
      Delivery.strategy.type %in% c("House to House", "On Demand", "Visit for Other Reasons") ~ "House-to-house",
      TRUE ~ "Other"
    )
  ) %>%
  # Group by strategy and the new grouped delivery type
  group_by(Trial_Arm, delivery_group) %>%
  summarize(
    n_observations = n(),
    mean_distance = mean(TravelDistanceKM_clean, na.rm = TRUE),
    median_distance = median(TravelDistanceKM_clean, na.rm = TRUE),
    sd_distance = sd(TravelDistanceKM_clean, na.rm = TRUE),
    # Calculate 95% CI
    se_distance = sd_distance / sqrt(n_observations),
    ci_lower = mean_distance - qt(0.975, n_observations-1) * se_distance,
    ci_upper = mean_distance + qt(0.975, n_observations-1) * se_distance,
    .groups = "drop"
  )

print("Travel distance by strategy and grouped delivery type:")
print(distance_summary)

# Simple statistical significance tests for travel distance differences
# Prepare data
test_data <- dogOwnerDistance %>%
  filter(!is.na(TravelDistanceKM_clean)) %>%
  mutate(
    delivery_group = case_when(
      Delivery.strategy.type %in% c("Central Point - Subvillage", "Central Point - Village") ~ "Central point",
      Delivery.strategy.type %in% c("House to House", "On Demand", "Visit for Other Reasons") ~ "House-to-house",
      TRUE ~ "Other"
    )
  ) %>%
  filter(delivery_group != "Other")

# Test: Central Point delivery - Continuous vs Pulse arms
central_data <- test_data %>% filter(delivery_group == "Central point")
continuous_central <- central_data$TravelDistanceKM_clean[central_data$Trial_Arm == "Continuous"]
pulse_central <- central_data$TravelDistanceKM_clean[central_data$Trial_Arm == "Pulse"]
# significant test
 wilcox.test(continuous_central, pulse_central)

 
# Create a formatted table with 95% CI as mean (95% CI)
formatted_distance_summary <- distance_summary %>%
  mutate(
    distance_with_ci = sprintf("%.2f (%.2f-%.2f)", 
                               mean_distance, ci_lower, ci_upper)
  ) %>%
  dplyr::select(Trial_Arm, delivery_group, n_observations, distance_with_ci)

print("Formatted distance summary with 95% CI:")
print(formatted_distance_summary)

###############################################################








