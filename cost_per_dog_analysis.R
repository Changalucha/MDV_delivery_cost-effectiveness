
rm(list = ls())
library(ggplot2)
library(ggrepel)
library(tidyverse)
require(dplyr)
library(lubridate)
library(reshape2)
library(dunn.test)
library(lme4)
# ANALYSIS OF COST PER DOG

# 1. Load dataset
by_location_calc <- read.csv("data/monthly_expanded_costs.csv", stringsAsFactors = FALSE)

# 3. Aggregate vaccination costs at the ward level and calculate per-dog costs
# First aggregate totals, then filter out zero-dog months, then calculate per-dog costs
rural_urban_cost <- by_location_calc %>%
  group_by(district, ward, strategy, location, vaccination_year) %>%
  summarise(
    total_dogs = sum(vaccinated_dogs, na.rm = TRUE),
    # Sum all cost components first (no division yet)
    vaccination_cost_sum = sum(vaccination_cost, na.rm = TRUE),
    fuel_VX_sum = sum(fuel_VX, na.rm = TRUE),
    fuel_NVX_sum = sum(fuel_NVX, na.rm = TRUE),
    # Combine storage cost and zeepot cost as they are related
    storage_cost_sum = sum(proportional_storage_cost, na.rm = TRUE) + sum(proport_zeepot_cost, na.rm = TRUE),
    rental_cost_sum = sum(rental_cost, na.rm = TRUE),
    rental_NVX_sum = sum(rental_NVX, na.rm = TRUE),
    labour_PR_VX_sum = sum(labour_PR_VX, na.rm = TRUE),
    labour_PR_NVX_sum = sum(labour_PR_NVX, na.rm = TRUE),
    Travel_PR_VX_sum = sum(Travel_PR_VX, na.rm = TRUE),
    Travel_PR_NVX_sum = sum(Travel_PR_NVX, na.rm = TRUE),
    perdiem_PR_VX_sum = sum(perdiem_PR_VX, na.rm = TRUE),
    perdiem_PR_NVX_sum = sum(perdiem_PR_NVX, na.rm = TRUE),
    HH_trav_cost_sum = sum(HH_trav_cost, na.rm = TRUE),
    HH_wait_cost_sum = sum(HH_wait_cost, na.rm = TRUE),
    HH_cost_sum = sum(HH_cost, na.rm = TRUE),
    total_cost_sum = sum(total_cost, na.rm = TRUE),
    .groups = "drop"
  ) 

rural_urban_cost <- rural_urban_cost %>%
  mutate(
    Vacc_delivery = rowSums(cbind(vaccination_cost_sum, labour_PR_VX_sum, perdiem_PR_VX_sum, Travel_PR_VX_sum,
                                  fuel_VX_sum, rental_cost_sum), na.rm = TRUE),
    # Combined storage costs
    vacc_storage = storage_cost_sum,
    non_vacc_delivery = rowSums(cbind(labour_PR_NVX_sum, perdiem_PR_NVX_sum, Travel_PR_NVX_sum,
                                      rental_NVX_sum, fuel_NVX_sum), na.rm = TRUE))


# OVERALL COST PER DOG
# 4. Calculate provider administrative cost (total minus household burden)
rural_urban_cost$provider_costs <- rural_urban_cost$total_cost_sum - rural_urban_cost$HH_cost_sum
# calculate cost per dog
rural_urban_cost <- rural_urban_cost %>%
  group_by(district, ward, strategy, location, vaccination_year) %>%
  filter(total_dogs > 0) %>%  # Remove months with zero vaccinations
  mutate(
    # Now calculate per-dog costs safely
    vaccine_bundle = vaccination_cost_sum / total_dogs,
    fuel_VX_cost = fuel_VX_sum / total_dogs,
    fuel_NVX_cost = fuel_NVX_sum / total_dogs,
    storage_cost = storage_cost_sum / total_dogs,  # Combined storage cost
    vehicle_rent_VL = rental_cost_sum / total_dogs,
    vehicle_rent_NVL = rental_NVX_sum / total_dogs,
    labour_VX_cost = labour_PR_VX_sum / total_dogs,
    labour_NVX_cost = labour_PR_NVX_sum / total_dogs,
    travel_VX_cost = Travel_PR_VX_sum / total_dogs,
    travel_NVX_cost = Travel_PR_NVX_sum / total_dogs,
    perdiem_VX_cost = perdiem_PR_VX_sum / total_dogs,
    perdiem_NVX_cost = perdiem_PR_NVX_sum / total_dogs,
    HH_trav = HH_trav_cost_sum / total_dogs,
    HH_wait = HH_wait_cost_sum / total_dogs,
    Household_costs = HH_cost_sum / total_dogs,
    main_total = total_cost_sum / total_dogs,
    VC_delivery = Vacc_delivery / total_dogs,
    NVC_delivery = non_vacc_delivery / total_dogs,
    admin_cost = provider_costs / total_dogs
  )

# summarise cost per dog with 95% CI By strategy
strategy_ci <- rural_urban_cost %>%
  group_by(strategy) %>%
  summarise(
    n = n(),
    mean_cost = mean(main_total, na.rm = TRUE),
    se = sd(main_total, na.rm = TRUE) / sqrt(n()),
    median_cost = median(main_total, na.rm = TRUE),
    lower_ci = mean_cost - qt(0.975, n() - 1) * se,
    upper_ci = mean_cost + qt(0.975, n() - 1) * se,
    .groups = "drop"
  )

print(strategy_ci)

# Both Welch's t-test and Wilcoxon test for comparison
# Welch's t-test (allow unequal variance)
welch_test <- t.test(main_total ~ strategy, data = rural_urban_cost, var.equal = FALSE)
print("Welch's t-test results:")
print(welch_test)

# Non-parametric alternative to Welch t-test
wilcox_test <- wilcox.test(main_total ~ strategy, data = rural_urban_cost)
print("Wilcoxon test results:")
print(wilcox_test)

# 4. Summarize ward-level costs across location and strategy
ward_perDog_summary <- rural_urban_cost %>%
  group_by(strategy) %>%
  summarize(
    n_wards = n(),
    mean_cost = mean(main_total),
    median_cost = median(main_total),
    sd_cost = sd(main_total),
    min_cost = min(main_total),
    max_cost = max(main_total),
    total_dogs = sum(total_dogs),
    .groups = "drop"
  ) %>%
  mutate(strategy = factor(strategy, 
                           levels = c("continuous", "pulsed"),
                           labels = c("Community-led", "Team-led")))

print(ward_perDog_summary)

# Both parametric and non-parametric tests for cost component comparisons
# Provider administrative cost comparison
admin_welch_test <- t.test(admin_cost ~ strategy, data = rural_urban_cost, var.equal = FALSE)
admin_wilcox_test <- wilcox.test(admin_cost ~ strategy, data = rural_urban_cost)
print("Provider Administrative Cost Comparison:")
print("Welch t-test:")
print(admin_welch_test)
print("Wilcoxon test:")
print(admin_wilcox_test)

# Household cost comparison  
household_welch_test <- t.test(Household_costs ~ strategy, data = rural_urban_cost, var.equal = FALSE)
household_wilcox_test <- wilcox.test(Household_costs ~ strategy, data = rural_urban_cost)
print("Household Cost Comparison:")
print("Welch t-test:")
print(household_welch_test)
print("Wilcoxon test:")
print(household_wilcox_test)

# Test each cost component for significant differences between strategies
cost_components <- c("fuel_VX_cost", "fuel_NVX_cost", 
                     "storage_cost", "vehicle_rent_VL", 
                     "vehicle_rent_NVL", "labour_VX_cost", "labour_NVX_cost",
                     "travel_VX_cost", "travel_NVX_cost", "perdiem_VX_cost", 
                     "perdiem_NVX_cost", "Household_costs", "admin_cost", "VC_delivery", "NVC_delivery")

# Create results dataframe for both tests
component_results <- data.frame(
  component = character(),
  welch_p_value = numeric(),
  wilcox_p_value = numeric(),
  welch_significant = character(),
  wilcox_significant = character(),
  stringsAsFactors = FALSE
)

# Test each component with both tests
for(component in cost_components) {
  welch_result <- t.test(rural_urban_cost[[component]] ~ rural_urban_cost$strategy, var.equal = FALSE)
  wilcox_result <- wilcox.test(rural_urban_cost[[component]] ~ rural_urban_cost$strategy)
  
  component_results <- rbind(component_results, data.frame(
    component = component,
    welch_p_value = welch_result$p.value,
    wilcox_p_value = wilcox_result$p.value,
    welch_significant = ifelse(welch_result$p.value < 0.05, "Yes", "No"),
    wilcox_significant = ifelse(wilcox_result$p.value < 0.05, "Yes", "No"),
    stringsAsFactors = FALSE
  ))
}

# Sort by Welch p-value
component_results <- component_results[order(component_results$welch_p_value), ]

# Add adjusted p-values for multiple testing
component_results$welch_p_adjusted <- p.adjust(component_results$welch_p_value, method = "bonferroni")
component_results$wilcox_p_adjusted <- p.adjust(component_results$wilcox_p_value, method = "bonferroni")
component_results$welch_significant_adjusted <- ifelse(component_results$welch_p_adjusted < 0.05, "Yes", "No")
component_results$wilcox_significant_adjusted <- ifelse(component_results$wilcox_p_adjusted < 0.05, "Yes", "No")

print("Cost Component Significance Testing (Both Welch and Wilcoxon):")
print(component_results)

# Summary of means by strategy for significant components (based on Welch test)
significant_components <- component_results$component[component_results$welch_significant == "Yes"]

if(length(significant_components) > 0) {
  component_means <- rural_urban_cost %>%
    group_by(strategy) %>%
    summarise(across(all_of(significant_components), mean, na.rm = TRUE), .groups = "drop")
  
  print("Mean values for significant components (Welch test):")
  print(component_means)
}

# First create the combined cost variables in the dataset
rural_urban_cost <- rural_urban_cost %>%
  mutate(
    fuel_combined = fuel_VX_cost + fuel_NVX_cost,
    storage_combined = storage_cost,  # Already combined above
    rental_combined = vehicle_rent_VL + vehicle_rent_NVL,
    labour_combined = labour_VX_cost + labour_NVX_cost,
    travel_combined = travel_VX_cost + travel_NVX_cost,
    perdiem_combined = perdiem_VX_cost + perdiem_NVX_cost
  )

# Test combined cost components with both tests
combined_components <- c("fuel_combined", "storage_combined", 
                         "rental_combined", "labour_combined", "travel_combined", 
                         "perdiem_combined", "Household_costs", "admin_cost")

# Create results dataframe for combined components
combined_results <- data.frame(
  component = character(),
  welch_p_value = numeric(),
  wilcox_p_value = numeric(),
  welch_significant = character(),
  wilcox_significant = character(),
  stringsAsFactors = FALSE
)

# Test each combined component with both tests
for(component in combined_components) {
  welch_result <- t.test(rural_urban_cost[[component]] ~ rural_urban_cost$strategy, var.equal = FALSE)
  wilcox_result <- wilcox.test(rural_urban_cost[[component]] ~ rural_urban_cost$strategy)
  
  combined_results <- rbind(combined_results, data.frame(
    component = component,
    welch_p_value = welch_result$p.value,
    wilcox_p_value = wilcox_result$p.value,
    welch_significant = ifelse(welch_result$p.value < 0.05, "Yes", "No"),
    wilcox_significant = ifelse(wilcox_result$p.value < 0.05, "Yes", "No"),
    stringsAsFactors = FALSE
  ))
}

# Sort by Welch p-value and add multiple testing correction
combined_results <- combined_results[order(combined_results$welch_p_value), ]
combined_results$welch_p_adjusted <- p.adjust(combined_results$welch_p_value, method = "bonferroni")
combined_results$wilcox_p_adjusted <- p.adjust(combined_results$wilcox_p_value, method = "bonferroni")
combined_results$welch_significant_adjusted <- ifelse(combined_results$welch_p_adjusted < 0.05, "Yes", "No")
combined_results$wilcox_significant_adjusted <- ifelse(combined_results$wilcox_p_adjusted < 0.05, "Yes", "No")

print("Combined Cost Component Significance Testing (Both Welch and Wilcoxon):")
print(combined_results)

# Show means for all components by strategy
component_means_summary <- rural_urban_cost %>%
  group_by(strategy) %>%
  summarise(
    vaccine = mean(vaccine_bundle, na.rm = TRUE),
    fuel = mean(fuel_combined, na.rm = TRUE),
    storage = mean(storage_combined, na.rm = TRUE),
    rental = mean(rental_combined, na.rm = TRUE),
    labour = mean(labour_combined, na.rm = TRUE),
    travel = mean(travel_combined, na.rm = TRUE),
    perdiem = mean(perdiem_combined, na.rm = TRUE),
    household = mean(Household_costs, na.rm = TRUE),
    providerCost = mean(admin_cost, na.rm = TRUE),
    .groups = "drop"
  )

print("Mean values by strategy for combined components:")
print(component_means_summary)

# ========================================================================================
# RURAL URBAN ANALYSIS 
# ========================================================================================
# Objective: Compare cost components between rural and urban wards, across vaccination strategies
# 6. Summarize ward-level costs across location and strategy
ward_summary <- rural_urban_cost %>%
  group_by(location, strategy) %>%
  summarize(
    n_wards = n(),
    mean_cost = mean(main_total),
    median_cost = median(main_total),
    sd_cost = sd(main_total),
    min_cost = min(main_total),
    max_cost = max(main_total),
    total_dogs = sum(total_dogs),
    .groups = "drop"
  ) %>%
  mutate(strategy = factor(strategy, 
                           levels = c("continuous", "pulsed"),
                           labels = c("Community-led", "Team-led")))

# Print ward-level summary table
print(ward_summary)

# 7. Compare mean cost per dog between strategies, separately for rural and urban
# Both parametric and non-parametric alternatives for rural and urban comparisons
rural_welch_test <- t.test(main_total ~ strategy, data = rural_urban_cost, 
                           subset = location == "rural", var.equal = FALSE)
rural_wilcox_test <- wilcox.test(main_total ~ strategy, data = rural_urban_cost, 
                                 subset = location == "rural")
print("Rural cost comparison:")
print("Welch t-test:")
print(rural_welch_test)
print("Wilcoxon test:")
print(rural_wilcox_test)

urban_welch_test <- t.test(main_total ~ strategy, data = rural_urban_cost, 
                           subset = location == "urban", var.equal = FALSE)
urban_wilcox_test <- wilcox.test(main_total ~ strategy, data = rural_urban_cost, 
                                 subset = location == "urban")
print("Urban cost comparison:")
print("Welch t-test:")
print(urban_welch_test)
print("Wilcoxon test:")
print(urban_wilcox_test)

# 8. Non-parametric comparison across all groups
# Kruskal-Wallis test on strategy-location interaction
kruskal.test(main_total ~ interaction(strategy, location), data = rural_urban_cost)

# Pairwise comparisons with Bonferroni correction
pairwise.wilcox.test(
  rural_urban_cost$main_total,
  interaction(rural_urban_cost$strategy, rural_urban_cost$location),
  p.adjust.method = "bonferroni"  # Adjust p-values for multiple testing
)

# 9. Ward-level component cost breakdown by location and strategy
ward_component_summary <- rural_urban_cost %>%
  group_by(strategy, location) %>%
  summarize(
    # Vaccine costs
    vaccine = mean(vaccine_bundle, na.rm = TRUE),
    
    # Fuel costs
    fuel_VX = mean(fuel_VX_cost, na.rm = TRUE),
    fuel_NVX = mean(fuel_NVX_cost, na.rm = TRUE),
    fuel = mean(fuel_VX_cost + fuel_NVX_cost, na.rm = TRUE),
    
    # Storage costs (combined)
    storage = mean(storage_cost, na.rm = TRUE),
    
    # Rental costs
    rent_VL = mean(vehicle_rent_VL, na.rm = TRUE),
    rent_NVL = mean(vehicle_rent_NVL, na.rm = TRUE),
    rental = mean(vehicle_rent_VL + vehicle_rent_NVL, na.rm = TRUE),
    
    # Labour costs
    labour_VX = mean(labour_VX_cost, na.rm = TRUE),
    labour_NVX = mean(labour_NVX_cost, na.rm = TRUE),
    labour = mean(labour_VX_cost + labour_NVX_cost, na.rm = TRUE),
    
    # Travel costs
    travel_VX = mean(travel_VX_cost, na.rm = TRUE),
    travel_NVX = mean(travel_NVX_cost, na.rm = TRUE),
    travel = mean(travel_VX_cost + travel_NVX_cost, na.rm = TRUE),
    
    # Per diem
    perdiem_VX = mean(perdiem_VX_cost, na.rm = TRUE),
    perdiem_NVX = mean(perdiem_NVX_cost, na.rm = TRUE),
    perdiem = mean(perdiem_VX_cost + perdiem_NVX_cost, na.rm = TRUE),
    
    # Household costs
    household = mean(Household_costs, na.rm = TRUE),
    
    # Provider/admin costs
    providerCost = mean(admin_cost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(strategy = factor(strategy, 
                           levels = c("continuous", "pulsed"),
                           labels = c("Community-led", "Team-led")))

# Function to get component costs in long format with all combinations
get_component_costs_long <- function(data) {
  # Get mean costs for each component by location and strategy
  component_costs <- data %>%
    group_by(location, strategy) %>%
    summarise(
      fuel_VX_cost = mean(fuel_VX_cost, na.rm = TRUE),
      fuel_NVX_cost = mean(fuel_NVX_cost, na.rm = TRUE),
      storage_cost = mean(storage_cost, na.rm = TRUE),
      labour_VX_cost = mean(labour_VX_cost, na.rm = TRUE),
      labour_NVX_cost = mean(labour_NVX_cost, na.rm = TRUE),
      travel_VX_cost = mean(travel_VX_cost, na.rm = TRUE),
      travel_NVX_cost = mean(travel_NVX_cost, na.rm = TRUE),
      perdiem_VX_cost = mean(perdiem_VX_cost, na.rm = TRUE),
      perdiem_NVX_cost = mean(perdiem_NVX_cost, na.rm = TRUE),
      Household_costs = mean(Household_costs, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Convert to long format
  component_costs_long <- component_costs %>%
    pivot_longer(
      cols = c(fuel_VX_cost, fuel_NVX_cost, storage_cost, labour_VX_cost, 
               labour_NVX_cost, travel_VX_cost, travel_NVX_cost, 
               perdiem_VX_cost, perdiem_NVX_cost, Household_costs),
      names_to = "component",
      values_to = "mean_cost"
    ) %>%
    arrange(component, location, strategy)
  
  return(component_costs_long)
}

# Function to perform tests and get results in long format
get_component_tests_long <- function(data) {
  cost_components <- c("fuel_VX_cost", "fuel_NVX_cost", 
                       "storage_cost", "labour_VX_cost", 
                       "labour_NVX_cost", "travel_VX_cost", "travel_NVX_cost",
                       "perdiem_VX_cost", "perdiem_NVX_cost", "Household_costs")
  
  # Create empty dataframe for results
  all_results <- data.frame()
  
  # Loop through each component and location combination
  for(component in cost_components) {
    for(location in c("rural", "urban")) {
      # Get data for this location
      location_data <- data[data$location == location, ]
      
      if(nrow(location_data) > 0 && 
         length(unique(location_data$strategy)) == 2) {  # Make sure we have both strategies
        
        # Perform tests
        tryCatch({
          welch_test <- t.test(location_data[[component]] ~ location_data$strategy, 
                               var.equal = FALSE)
          wilcox_test <- wilcox.test(location_data[[component]] ~ location_data$strategy)
          
          # Get mean costs for each strategy in this location
          means <- location_data %>%
            group_by(strategy) %>%
            summarise(mean_cost = mean(.data[[component]], na.rm = TRUE), .groups = "drop")
          
          continuous_mean <- means$mean_cost[means$strategy == "continuous"]
          pulsed_mean <- means$mean_cost[means$strategy == "pulsed"]
          
          # Create result row
          result_row <- data.frame(
            component = component,
            location = location,
            continuous_mean = ifelse(length(continuous_mean) > 0, continuous_mean, NA),
            pulsed_mean = ifelse(length(pulsed_mean) > 0, pulsed_mean, NA),
            welch_p = welch_test$p.value,
            wilcox_p = wilcox_test$p.value,
            welch_sig = welch_test$p.value < 0.05,
            wilcox_sig = wilcox_test$p.value < 0.05,
            stringsAsFactors = FALSE
          )
          
          all_results <- rbind(all_results, result_row)
          
        }, error = function(e) {
          cat("Error processing", component, "in", location, ":", e$message, "\n")
        })
      }
    }
  }
  
  # Add multiple testing correction
  if(nrow(all_results) > 0) {
    all_results <- all_results %>%
      mutate(
        welch_p_adj = p.adjust(welch_p, method = "bonferroni"),
        wilcox_p_adj = p.adjust(wilcox_p, method = "bonferroni"),
        welch_sig_adj = welch_p_adj < 0.05,
        wilcox_sig_adj = wilcox_p_adj < 0.05,
        # Add effect direction
        higher_cost_strategy = ifelse(continuous_mean > pulsed_mean, "continuous", "pulsed"),
        cost_difference = abs(continuous_mean - pulsed_mean)
      ) %>%
      arrange(component, location)
  }
  
  return(all_results)
}

# Run the analysis
# Get the test results in long format
component_tests_long <- get_component_tests_long(rural_urban_cost)

# Display results
print(component_tests_long)

######################################################################################
# COST PER DOG ANALYSIS OVER TIME - QUARTERLY BASIS
# Objective: Understand how cost per dog varies by strategy and time (quarterly intervals)

# ========================================================================================
# PART 1: QUARTERLY ANALYSIS (Quarterly intervals following vaccination calendar)
# ========================================================================================

# 0. Annual cost per dog 
annual_perdog <- rural_urban_cost %>%
  group_by(strategy, vaccination_year) %>%
  summarise(
    n = n(),
    mean_cost = mean(main_total, na.rm = TRUE),
    se = sd(main_total, na.rm = TRUE) / sqrt(n()),
    median_cost = median(main_total, na.rm = TRUE),
    lower_ci = mean_cost - qt(0.975, n() - 1) * se,
    upper_ci = mean_cost + qt(0.975, n() - 1) * se,
    .groups = "drop"
  )

print(annual_perdog)

# Assign numeric values to vaccination_year
rural_urban_cost <- rural_urban_cost %>%
  mutate(year_num = case_when(
    vaccination_year == "first_year" ~ 1,
    vaccination_year == "second_year" ~ 2,
    vaccination_year == "third_year" ~ 3
  ))

# conduct analysis of trend (Tests if costs are increasing, decreasing, or stable over time)
# Regression for continuous strategy
lm_cont <- lm(main_total ~ year_num, data = filter(rural_urban_cost, strategy == "continuous"))
summary(lm_cont)

# Regression for pulsed strategy
lm_pulsed <- lm(main_total ~ year_num, data = filter(rural_urban_cost, strategy == "pulsed"))
summary(lm_pulsed)

# Test if the RATE OF CHANGE differs between strategies
lm_trend_comparison <- lm(main_total ~ year_num * strategy, data = rural_urban_cost)
summary(lm_trend_comparison)

# 1. Create quarterly intervals following vaccination calendar (starting 2020-10-16)
quarterly_costs_summary <- by_location_calc %>%
  mutate(
    date = as.Date(month_interval),
    # Define vaccination calendar quarters starting from 2020-10-16
    # Q1: 2020-10-16 to 2021-01-15
    # Q2: 2021-01-16 to 2021-04-15  
    # Q3: 2021-04-16 to 2021-07-15
    # Q4: 2021-07-16 to 2021-09-16 (end of first year)
    vaccination_start = as.Date("2020-10-16"),
    days_since_start = as.numeric(date - vaccination_start),
    quarter_num = ceiling(pmax(1, days_since_start + 1) / 91.25), # ~91.25 days per quarter
    vaccination_quarter = paste0("Q", quarter_num),
    # Extract year from date for labeling
    calendar_year = year(date),
    quarter_interval = paste0(calendar_year, "-", vaccination_quarter)
  ) %>%
  group_by(district, ward, vaccination_year, strategy, quarter_interval, vaccination_quarter) %>%
  summarise(
    overall_costs = sum(total_cost, na.rm = TRUE),
    all_dogs = sum(vaccinated_dogs, na.rm = TRUE),
    cost_perDog = ifelse(all_dogs > 0, overall_costs/all_dogs, NA_real_),
    .groups = "drop"
  )

# 2. Aggregate quarterly totals by strategy and vaccination year
quarterly_summary_by_strategy_year <- quarterly_costs_summary %>%
  group_by(vaccination_year, strategy, quarter_interval, vaccination_quarter) %>%
  summarise(
    total_cost = sum(overall_costs, na.rm = TRUE),
    total_dogs = sum(all_dogs, na.rm = TRUE),
    cost_perDog = ifelse(total_dogs > 0, total_cost / total_dogs, NA_real_),
    .groups = "drop"
  )

# 3. Create proper date formatting for quarters based on vaccination calendar
quarterly_summary_by_strategy_year <- quarterly_summary_by_strategy_year %>%
  separate(quarter_interval, into = c("year", "quarter"), sep = "-", remove = FALSE) %>%
  mutate(
    quarter_num = as.numeric(str_extract(quarter, "\\d")),
    # Create vaccination calendar-based quarter dates
    vaccination_start = as.Date("2020-10-16"),
    quarter_date = vaccination_start + (quarter_num - 1) * 91.25,
    year_num = as.numeric(year)
  )

# ========================================================================================
# QUARTERLY ANALYSIS: Efficiency and Correlation Analysis
# ========================================================================================

# Aggregate quarterly costs by ward with vaccination calendar quarterly intervals
quarterly_costs <- by_location_calc %>%
  mutate(
    date = as.Date(month_interval),
    # Use vaccination calendar quarters
    vaccination_start = as.Date("2020-10-16"),
    days_since_start = as.numeric(date - vaccination_start),
    quarter_num = ceiling(pmax(1, days_since_start + 1) / 91.25),
    vaccination_quarter = paste0("Q", quarter_num),
    calendar_year = year(date),
    quarter_interval = paste0(calendar_year, "-", vaccination_quarter)
  ) %>%
  dplyr::select(
    district, ward, strategy, vaccination_year, quarter_interval,
    vaccinated_dogs, vaccination_cost, fuel_VX, fuel_NVX, rental_cost, rental_NVX, 
    labour_PR_VX, labour_PR_NVX, Travel_PR_VX, Travel_PR_NVX, perdiem_PR_VX, 
    perdiem_PR_NVX, HH_cost, proportional_storage_cost, proport_zeepot_cost
  ) %>%
  # Compute total cost per row first
  mutate(
    # Combine storage costs as they are related
    total_storage_cost = proportional_storage_cost + proport_zeepot_cost,
    total_cost = rowSums(across(c(vaccination_cost, fuel_VX, fuel_NVX, rental_cost, rental_NVX, 
                                  labour_PR_VX, labour_PR_NVX, Travel_PR_VX, Travel_PR_NVX, perdiem_PR_VX, 
                                  perdiem_PR_NVX, HH_cost, proportional_storage_cost, proport_zeepot_cost), 
                                ~replace_na(., 0)))) %>%
  # Aggregate by quarter
  group_by(district, ward, strategy, vaccination_year, quarter_interval) %>%
  summarise(across(
    .cols = c(
      vaccinated_dogs, vaccination_cost, fuel_VX, fuel_NVX, rental_cost, rental_NVX, 
      labour_PR_VX, labour_PR_NVX, Travel_PR_VX, Travel_PR_NVX, perdiem_PR_VX, 
      perdiem_PR_NVX, HH_cost, total_storage_cost, total_cost
    ),
    .fns = ~sum(.x, na.rm = TRUE)
  ), .groups = "drop")

# Define cost variables (updated with combined storage)
cost_vars <- c("vaccination_cost", "fuel_VX", "fuel_NVX", "rental_cost", "rental_NVX", 
               "labour_PR_VX", "labour_PR_NVX", "Travel_PR_VX", "Travel_PR_NVX", "perdiem_PR_VX", 
               "perdiem_PR_NVX", "HH_cost", "total_storage_cost")

# Calculate per-dog costs for each component at ward level (quarterly)
quarterly_costs <- quarterly_costs %>%
  mutate(
    across(all_of(cost_vars), 
           ~ifelse(vaccinated_dogs > 0, ./vaccinated_dogs, NA_real_), 
           .names = "{.col}_per_dog"),
    cost_per_dog = ifelse(vaccinated_dogs > 0, total_cost / vaccinated_dogs, NA_real_)
  )

# Calculate quarterly vaccination intensity metrics
ward_quarterly_efficiency <- quarterly_costs %>%
  filter(vaccinated_dogs > 0) %>%
  group_by(district, ward, strategy, vaccination_year) %>%
  summarize(
    quarters_active = n_distinct(quarter_interval),
    total_dogs = sum(vaccinated_dogs, na.rm = TRUE),
    avg_dogs_per_quarter = total_dogs / quarters_active,
    avg_cost_per_dog = weighted.mean(cost_per_dog, vaccinated_dogs, na.rm = TRUE),
    .groups = "drop"
  )

# Prepare data with strategy labels
ward_quarterly_efficiency_plot <- ward_quarterly_efficiency %>%
  mutate(
    strategy_label = factor(strategy, 
                            levels = c("continuous", "pulsed"),
                            labels = c("Community-led", "Team-led")),
    log_cost_per_dog = log(avg_cost_per_dog)
  )

# Calculate correlation statistics for annotation
correlation_stats <- ward_quarterly_efficiency_plot %>%
  group_by(strategy_label) %>%
  summarise(
    correlation = cor(avg_dogs_per_quarter, log_cost_per_dog, use = "complete.obs"),
    p_value = cor.test(avg_dogs_per_quarter, log_cost_per_dog)$p.value,
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    correlation_text = paste0("r = ", round(correlation, 3)),
    p_text = ifelse(p_value < 0.001, "p < 0.001", 
                    ifelse(p_value < 0.01, paste0("p = ", round(p_value, 3)),
                           paste0("p = ", round(p_value, 2)))),
    annotation_text = paste0(correlation_text, "\n", p_text, "\n", "n = ", n),
    # position anotations
    x_pos = max(ward_quarterly_efficiency_plot$avg_dogs_per_quarter, na.rm = TRUE) * 0.7,
    y_pos = max(ward_quarterly_efficiency_plot$log_cost_per_dog, na.rm = TRUE) * 0.9
  )

# Create the correlation plot
correlation_plot <- ggplot(ward_quarterly_efficiency_plot, 
                           aes(x = avg_dogs_per_quarter, y = log_cost_per_dog)) +
  geom_point(aes(color = strategy_label), alpha = 0.6, size = 2) +
  geom_smooth(aes(color = strategy_label), method = "lm", se = TRUE, alpha = 0.3) +
  facet_wrap(~ strategy_label, scales = "free") +
  
  # Add correlation statistics as annotations
  geom_text(data = correlation_stats,
            aes(x = x_pos, y = y_pos, label = annotation_text),
            hjust = 1, vjust = 1, size = 3.5, 
            fontface = "bold", color = "black",
            bbox = list(boxstyle = "round,pad=0.3", facecolor = "white", alpha = 0.8)) +
  
  scale_color_manual(values = c("Community-led" = "#1f77b4", "Team-led" = "#ff7f0e")) +
  
  labs(
    title = "",
    subtitle = "",
    x = "Average Dogs Vaccinated per Quarter",
    y = "Log(Cost per Dog)",
    color = "Strategy",
    caption = ""
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since we have facets
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 15),
    strip.text = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(correlation_plot)

# Save the plot
ggsave("figure/vaccination_intensity_cost_correlation.png", 
       correlation_plot, width = 12, height = 6, dpi = 300)

# ========================================================================================
# QUARTERLY ANALYSIS: Low vs High Vaccination Period Comparison
# ========================================================================================

# Calculate low/high vaccination periods at the ward level (quarterly), excluding quarters with zero dogs
quarterly_costs <- quarterly_costs %>%
  group_by(district, ward, strategy) %>%
  mutate(low_vacc_period = vaccinated_dogs < median(vaccinated_dogs[vaccinated_dogs > 0], na.rm = TRUE)) %>%
  ungroup()

# Perform quarterly analysis by strategy with ward as the unit of analysis using both tests
quarterly_results_by_strategy <- data.frame(
  strategy = character(),
  cost_component = character(),
  high_period_mean = numeric(),
  low_period_mean = numeric(),
  ratio = numeric(),
  welch_p_value = numeric(),
  wilcox_p_value = numeric(),
  stringsAsFactors = FALSE
)

# Get unique strategies
strategies <- unique(quarterly_costs$strategy)

# Loop through each strategy (quarterly analysis)
for(strat in strategies) {
  # Calculate ward-level averages for high/low periods (quarterly)
  ward_quarterly_results <- quarterly_costs %>%
    filter(strategy == strat) %>%
    group_by(district, ward, low_vacc_period) %>%
    summarize(across(ends_with("_per_dog"), ~mean(., na.rm = TRUE)), .groups = "drop")
  
  # For each cost component
  for(var in cost_vars) {
    per_dog_var <- paste0(var, "_per_dog")
    
    # Skip if variable doesn't exist
    if(!per_dog_var %in% names(ward_quarterly_results)) {
      next
    }
    
    # Create datasets for high and low vaccination periods
    high_period_wards <- ward_quarterly_results %>% 
      filter(!low_vacc_period) %>% 
      pull(per_dog_var)
    
    low_period_wards <- ward_quarterly_results %>% 
      filter(low_vacc_period) %>% 
      pull(per_dog_var)
    
    # Remove NAs and Infs
    high_period_wards <- high_period_wards[is.finite(high_period_wards)]
    low_period_wards <- low_period_wards[is.finite(low_period_wards)]
    
    # Skip if not enough data
    if(length(high_period_wards) < 2 || length(low_period_wards) < 2) {
      next
    }
    
    # Calculate means (ward-level averages)
    high_mean <- mean(high_period_wards, na.rm = TRUE)
    low_mean <- mean(low_period_wards, na.rm = TRUE)
    
    # Calculate ratio (handle division by zero)
    if(high_mean > 0) {
      ratio <- low_mean / high_mean
    } else {
      ratio <- NA
    }
    
    # Try both tests with safeguards
    tryCatch({
      # Check for constant data
      if(sd(high_period_wards) > 1e-10 && sd(low_period_wards) > 1e-10) {
        welch_result <- t.test(low_period_wards, high_period_wards, var.equal = FALSE)
        wilcox_result <- wilcox.test(low_period_wards, high_period_wards)
        welch_p_val <- welch_result$p.value
        wilcox_p_val <- wilcox_result$p.value
      } else {
        welch_p_val <- NA
        wilcox_p_val <- NA
      }
      
      # Store results with strategy
      quarterly_results_by_strategy <- rbind(quarterly_results_by_strategy, data.frame(
        strategy = strat,
        cost_component = var,
        high_period_mean = high_mean,
        low_period_mean = low_mean,
        ratio = ratio,
        welch_p_value = welch_p_val,
        wilcox_p_value = wilcox_p_val,
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      cat("Error processing", var, "for strategy", strat, ":", e$message, "\n")
    })
  }
}

# Format and print quarterly results by strategy
if(nrow(quarterly_results_by_strategy) > 0) {
  quarterly_results_by_strategy <- quarterly_results_by_strategy %>% 
    group_by(strategy) %>%
    arrange(strategy, desc(ratio)) %>%
    mutate(across(where(is.numeric), ~round(., 3)))
  
  # Print summary for each strategy
  for(strat in strategies) {
    cat("\n\n============================================\n")
    cat("QUARTERLY RESULTS FOR", toupper(strat), "STRATEGY\n")
    cat("============================================\n")
    print(filter(quarterly_results_by_strategy, strategy == strat))
  }
} else {
  print("No valid quarterly results to display")
}

# Additional summary statistics for quarterly vaccination patterns
quarterly_vaccination_stats <- quarterly_costs %>% 
  filter(vaccinated_dogs > 0) %>%
  group_by(district, ward) %>% 
  summarize(
    avg_dogs_quarterly = mean(vaccinated_dogs, na.rm = TRUE),
    median_dogs_quarterly = median(vaccinated_dogs, na.rm = TRUE),
    .groups = "drop"
  )

quarterly_median_of_medians <- median(quarterly_vaccination_stats$median_dogs_quarterly)
quarterly_median_of_means <- median(quarterly_vaccination_stats$avg_dogs_quarterly)

# Quick summary of total cost per dog by vaccination intensity
quick_summary <- quarterly_costs %>%
  filter(vaccinated_dogs > 0, is.finite(cost_per_dog)) %>%
  mutate(
    period_type = ifelse(low_vacc_period, "Low Vaccination", "High Vaccination")
  ) %>%
  group_by(strategy, period_type) %>%
  summarise(
    avg_total_cost_per_dog = round(mean(cost_per_dog, na.rm = TRUE), 2),
    median_total_cost_per_dog = round(median(cost_per_dog, na.rm = TRUE), 2),
    n_ward_quarters = n(),
    .groups = "drop"
  )

print(quick_summary)

# ========================================================================================
# PART 2: DISTRICT-LEVEL ANALYSIS (District, Strategy, Vaccination Year)
# ========================================================================================

# Select relevant cost variables for detailed analysis
summarised_data <- by_location_calc %>%
  dplyr::select(
    district, ward, strategy, vaccination_year, month_interval, Delivery.strategy.type,
    vaccinated_dogs, vaccination_cost, fuel_VX, fuel_NVX, rental_cost, rental_NVX, 
    labour_PR_VX, labour_PR_NVX, Travel_PR_VX, Travel_PR_NVX, perdiem_PR_VX, 
    perdiem_PR_NVX, HH_cost, proportional_storage_cost, proport_zeepot_cost
  )

# Compute total cost per row (combining storage costs)
summarised_data <- summarised_data %>%
  mutate(
    combined_storage_cost = proportional_storage_cost + proport_zeepot_cost,
    total_cost = rowSums(across(c(vaccination_cost, fuel_VX, fuel_NVX, rental_cost, rental_NVX, 
                                  labour_PR_VX, labour_PR_NVX, Travel_PR_VX, Travel_PR_NVX, perdiem_PR_VX, 
                                  perdiem_PR_NVX, HH_cost, combined_storage_cost), 
                                ~replace_na(., 0))))

# Update cost_vars for district analysis
district_cost_vars <- c("vaccination_cost", "fuel_VX", "fuel_NVX", "rental_cost", "rental_NVX", 
                        "labour_PR_VX", "labour_PR_NVX", "Travel_PR_VX", "Travel_PR_NVX", "perdiem_PR_VX", 
                        "perdiem_PR_NVX", "HH_cost", "combined_storage_cost")

# Aggregate data by district, strategy, and vaccination year (proper unit of analysis)
district_costs <- summarised_data %>%
  group_by(district, strategy, vaccination_year) %>%
  summarise(across(
    .cols = c(
      vaccinated_dogs, vaccination_cost, fuel_VX, fuel_NVX, rental_cost, rental_NVX, 
      labour_PR_VX, labour_PR_NVX, Travel_PR_VX, Travel_PR_NVX, perdiem_PR_VX, 
      perdiem_PR_NVX, HH_cost, combined_storage_cost
    ),
    .fns = ~sum(.x, na.rm = TRUE)
  ), .groups = "drop")

# Calculate total cost for each district-strategy-year combination
district_costs <- district_costs %>%
  rowwise() %>%
  mutate(total_cost = sum(c_across(all_of(district_cost_vars)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cost_per_dog = ifelse(vaccinated_dogs > 0, total_cost / vaccinated_dogs, NA_real_))

# Remove rows with zero dogs vaccinated or invalid cost per dog
district_costs <- district_costs %>%
  filter(vaccinated_dogs > 0, is.finite(cost_per_dog))

# Calculate overall cost per dog by district and strategy (across all years)
district_strategy_summary <- district_costs %>%
  group_by(district, strategy) %>%
  summarize(
    total_cost = sum(total_cost, na.rm = TRUE),
    total_dogs = sum(vaccinated_dogs, na.rm = TRUE),
    cost_per_dog = total_cost / total_dogs,
    years_active = n_distinct(vaccination_year),
    avg_annual_dogs = total_dogs / years_active,
    .groups = "drop"
  ) %>%
  arrange(strategy, desc(cost_per_dog))

# Calculate strategy performance metrics
strategy_performance <- district_strategy_summary %>%
  group_by(strategy) %>%
  summarize(
    n_districts = n(),
    avg_cost_per_dog = weighted.mean(cost_per_dog, total_dogs),
    median_cost_per_dog = median(cost_per_dog),
    sd_cost_per_dog = sd(cost_per_dog),
    min_cost_per_dog = min(cost_per_dog),
    max_cost_per_dog = max(cost_per_dog),
    total_dogs_all_districts = sum(total_dogs),
    .groups = "drop"
  )

print(strategy_performance)

# Calculate district variation from strategy mean
district_strategy_summary <- district_strategy_summary %>%
  left_join(strategy_performance %>% 
              dplyr::select(strategy, avg_cost_per_dog, sd_cost_per_dog), 
            by = "strategy", suffix = c("", "_strategy_avg")) %>%
  mutate(
    pct_diff_from_avg = ((cost_per_dog / avg_cost_per_dog) - 1) * 100,
    z_score = (cost_per_dog - avg_cost_per_dog) / sd_cost_per_dog
  )

# Identify high-cost and low-cost districts by strategy
high_cost_districts <- district_strategy_summary %>%
  filter(z_score > 1) %>%
  arrange(strategy, desc(z_score))

low_cost_districts <- district_strategy_summary %>%
  filter(z_score < -1) %>%
  arrange(strategy, z_score)

# print results
print(high_cost_districts %>% 
        dplyr::select(district, strategy, cost_per_dog, pct_diff_from_avg, z_score))

print(low_cost_districts %>% 
        dplyr::select(district, strategy, cost_per_dog, pct_diff_from_avg, z_score))

# ========================================================================================
# DISTRICT-LEVEL ANALYSIS: Cost Component Breakdown
# ========================================================================================

# Calculate per-dog costs for each component by district and strategy
district_component_costs <- district_costs %>%
  group_by(district, strategy) %>%
  summarize(
    total_dogs = sum(vaccinated_dogs, na.rm = TRUE),
    across(all_of(district_cost_vars), ~sum(., na.rm = TRUE) / total_dogs, .names = "{.col}_per_dog"),
    .groups = "drop"
  )

# Analyze cost drivers for high-cost districts
if(nrow(high_cost_districts) > 0) {
  high_cost_drivers <- district_component_costs %>%
    inner_join(high_cost_districts %>% dplyr::select(district, strategy), 
               by = c("district", "strategy")) %>%
    pivot_longer(
      cols = ends_with("_per_dog"),
      names_to = "component", 
      values_to = "cost_per_dog_component"
    ) %>%
    group_by(strategy, district) %>%
    arrange(strategy, district, desc(cost_per_dog_component)) %>%
    slice_head(n = 5)
  
  cat("\n\nTOP 5 COST DRIVERS IN HIGH-COST DISTRICTS:\n")
  cat("==========================================\n")
  print(high_cost_drivers)
}

# ========================================================================================
# DISTRICT-LEVEL ANALYSIS: Strategy Comparison Within Districts
# ========================================================================================

# Compare strategy performance within the same district using both tests
districts_with_both_strategies <- district_strategy_summary %>%
  group_by(district) %>%
  summarize(n_strategies = n_distinct(strategy), .groups = "drop") %>%
  filter(n_strategies > 1) %>%
  pull(district)

if(length(districts_with_both_strategies) > 0) {
  strategy_comparison_within_district <- district_strategy_summary %>%
    filter(district %in% districts_with_both_strategies) %>%
    dplyr::select(district, strategy, cost_per_dog, total_dogs) %>%
    pivot_wider(
      names_from = strategy,
      values_from = c(cost_per_dog, total_dogs),
      names_sep = "_"
    )
  
  # Calculate performance differences
  if("cost_per_dog_continuous" %in% names(strategy_comparison_within_district) && 
     "cost_per_dog_pulsed" %in% names(strategy_comparison_within_district)) {
    
    strategy_comparison_within_district <- strategy_comparison_within_district %>%
      mutate(
        cost_diff = cost_per_dog_continuous - cost_per_dog_pulsed,
        cost_ratio = cost_per_dog_continuous / cost_per_dog_pulsed,
        dogs_diff = total_dogs_continuous - total_dogs_pulsed
      ) %>%
      arrange(desc(abs(cost_diff)))
    
    cat("\n\nSTRATEGY COMPARISON WITHIN DISTRICTS:\n")
    cat("====================================\n")
    print(strategy_comparison_within_district)
  }}

# Test strategy differences between districts using mixed effect linear model
# compare cost per dog between trial arms across multiple districts
# Fit mixed-effects model
model <- lmer(cost_per_dog ~ strategy + (1 | district), 
              data = district_strategy_summary, 
              weights = total_dogs)

summary(model)


#################################################################################
# COST-EFFECTIVENESS BY DELIVERY APPROACH
# Better approach to include delivery type and month
expanded_rural_urban_cost <- by_location_calc %>%
  # Group by all desired variables
  group_by(district, ward, strategy, vaccination_year, Delivery.strategy.type) %>%
  # Sum costs and dogs, then calculate per-dog costs with combined storage
  summarise(
    total_dogs = sum(vaccinated_dogs, na.rm = TRUE),
    vaccine_bundle = sum(vaccination_cost, na.rm = TRUE)/ sum(vaccinated_dogs, na.rm = TRUE),
    fuel_VX_cost = sum(fuel_VX, na.rm = TRUE)/ sum(vaccinated_dogs, na.rm = TRUE),
    fuel_NVX_cost = sum(fuel_NVX, na.rm = TRUE)/ sum(vaccinated_dogs, na.rm = TRUE),
    storage_cost = (sum(proportional_storage_cost, na.rm = TRUE) + sum(proport_zeepot_cost, na.rm = TRUE))/ sum(vaccinated_dogs, na.rm = TRUE),
    vehicle_rent_VL = sum(rental_cost, na.rm = TRUE)/ sum(vaccinated_dogs, na.rm = TRUE),
    vehicle_rent_NVL = sum(rental_NVX, na.rm = TRUE)/ sum(vaccinated_dogs, na.rm = TRUE),
    labour_VX_cost = sum(labour_PR_VX, na.rm = TRUE)/ sum(vaccinated_dogs, na.rm = TRUE),
    labour_NVX_cost = sum(labour_PR_NVX, na.rm = TRUE)/ sum(vaccinated_dogs, na.rm = TRUE),
    travel_VX_cost = sum(Travel_PR_VX, na.rm = TRUE)/ sum(vaccinated_dogs, na.rm = TRUE),
    travel_NVX_cost = sum(Travel_PR_NVX, na.rm = TRUE)/ sum(vaccinated_dogs, na.rm = TRUE),
    perdiem_VX_cost = sum(perdiem_PR_VX, na.rm = TRUE)/ sum(vaccinated_dogs, na.rm = TRUE),
    perdiem_NVX_cost = sum(perdiem_PR_NVX, na.rm = TRUE)/ sum(vaccinated_dogs, na.rm = TRUE),
    HH_trav = sum(HH_trav_cost, na.rm = TRUE)/ sum(vaccinated_dogs, na.rm = TRUE),
    HH_wait = sum(HH_wait_cost, na.rm = TRUE)/ sum(vaccinated_dogs, na.rm = TRUE),
    Household_costs = sum(HH_cost, na.rm = TRUE)/ sum(vaccinated_dogs, na.rm = TRUE),
    total_cost = sum(total_cost, na.rm = TRUE),
    # Only calculate per-dog cost when dogs > 0
    main_total = ifelse(sum(vaccinated_dogs, na.rm = TRUE) > 0,
                        sum(total_cost, na.rm = TRUE) / sum(vaccinated_dogs, na.rm = TRUE),
                        NA),  # Set to NA instead of Inf
    .groups = "drop"
  ) %>%
  # Filter out entries with no dogs
  filter(total_dogs > 0)

# Create a simplified delivery strategy classification
expanded_rural_urban_cost <- expanded_rural_urban_cost %>%
  mutate(
    delivery_simplified = case_when(
      Delivery.strategy.type %in% c("House to House", "On Demand", "Visit for Other Reasons") ~ "Household Level",
      Delivery.strategy.type == "Central Point - Subvillage" ~ "Central Point - Subvillage",
      Delivery.strategy.type == "Central Point - Village" ~ "Central Point - Village",
      TRUE ~ Delivery.strategy.type  # Keep original for any other types
    )
  )

# Cost analysis by simplified delivery strategy type
delivery_costs <- expanded_rural_urban_cost %>%
  group_by(delivery_simplified, strategy) %>%
  summarize(
    n_wards = n(),
    mean_cost = mean(main_total),
    median_cost = median(main_total),
    sd_cost = sd(main_total),
    total_dogs = sum(total_dogs),
    .groups = "drop"
  ) %>%
  mutate(strategy = factor(strategy, 
                           levels = c("continuous", "pulsed"),
                           labels = c("Community-led", "Team-led")))

# Statistical test for Central Point - Village only
central_village_test <- expanded_rural_urban_cost %>%
  filter(delivery_simplified == "Central Point - Village")

welch_central <- t.test(main_total ~ strategy, data = central_village_test, var.equal = FALSE)
wilcox_central <- wilcox.test(main_total ~ strategy, data = central_village_test)

# Component analysis by simplified delivery strategy using medians
robust_component_analysis <- expanded_rural_urban_cost %>%
  group_by(delivery_simplified, strategy) %>%
  summarize(
    n_observations = n(),
    total_dogs = sum(total_dogs, na.rm = TRUE),
    mean_vaccine = mean(vaccine_bundle, na.rm = TRUE),
    median_vaccine = median(vaccine_bundle, na.rm = TRUE),
    mean_fuel = mean(fuel_VX_cost + fuel_NVX_cost, na.rm = TRUE),
    median_fuel = median(fuel_VX_cost + fuel_NVX_cost, na.rm = TRUE),
    mean_storage = mean(storage_cost, na.rm = TRUE),  # Already combined
    median_storage = median(storage_cost, na.rm = TRUE),  # Already combined
    mean_rental = mean(vehicle_rent_VL + vehicle_rent_NVL, na.rm = TRUE),
    median_rental = median(vehicle_rent_VL + vehicle_rent_NVL, na.rm = TRUE),
    mean_labour = mean(labour_VX_cost + labour_NVX_cost, na.rm = TRUE),
    median_labour = median(labour_VX_cost + labour_NVX_cost, na.rm = TRUE),
    mean_travel = mean(travel_VX_cost + travel_NVX_cost, na.rm = TRUE),
    median_travel = median(travel_VX_cost + travel_NVX_cost, na.rm = TRUE),
    mean_perdiem = mean(perdiem_VX_cost + perdiem_NVX_cost, na.rm = TRUE),
    median_perdiem = median(perdiem_VX_cost + perdiem_NVX_cost, na.rm = TRUE),
    mean_household = mean(Household_costs, na.rm = TRUE),
    median_household = median(Household_costs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(strategy = factor(strategy, 
                           levels = c("continuous", "pulsed"),
                           labels = c("Community-led", "Team-led")))

# Convert to long format for visualization
robust_component_long <- robust_component_analysis %>%
  pivot_longer(cols = starts_with("median_"),
               names_to = "component", 
               values_to = "cost_per_dog") %>%
  mutate(component = gsub("median_", "", component))

# Calculate average cost per component across all strategies and delivery methods
# This will be used to order the components
component_ordering <- robust_component_long %>%
  group_by(component) %>%
  summarize(avg_cost = mean(cost_per_dog, na.rm = TRUE)) %>%
  arrange(desc(avg_cost))

# Convert component to a factor with levels ordered by cost
robust_component_long <- robust_component_long %>%
  mutate(component = factor(component, levels = component_ordering$component))





