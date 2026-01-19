
#################################################################################
# COST-EFFECTIVENESS ANALYSIS OF MASS DOG VACCINATION (MDV)
# This script calculates cost per dog for different vaccination strategies and components
#################################################################################

rm(list = ls())

# Load required libraries
library(tidyverse)
require(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(purrr)
library(broom)

# Load dataset for monthly ward resources
monthly_ward_costs <- read.csv("data/monthly_expanded_costs.csv")

#################################################################################
# 1. OVERALL COST PER DOG ANALYSIS (ALL COMPONENTS)
#################################################################################

# Aggregate costs annually by ward
yrly_ward_costs <- monthly_ward_costs %>%
  group_by(district, ward, strategy, vaccination_year) %>%
  summarise(
    total_dogs = sum(vaccinated_dogs, na.rm = TRUE),
    vaccine_bundle = sum(vaccination_cost, na.rm = TRUE),
    labour_VX_cost = sum(labour_PR_VX, na.rm = TRUE),
    perdiem_VX_cost = sum(perdiem_PR_VX, na.rm = TRUE),
    travel_VX_cost = sum(Travel_PR_VX, na.rm = TRUE),
    fuel_VX_cost = sum(fuel_VX, na.rm = TRUE),
    vehicle_rent_VL = sum(rental_cost, na.rm = TRUE),
    
    HH_wait = sum(HH_wait_cost, na.rm = TRUE),
    HH_trav = sum(HH_trav_cost, na.rm = TRUE),
    Household_costs = sum(HH_cost, na.rm = TRUE),
    
    labour_NVX_cost = sum(labour_PR_NVX, na.rm = TRUE),
    perdiem_NVX_cost = sum(perdiem_PR_NVX, na.rm = TRUE),
    travel_NVX_cost = sum(Travel_PR_NVX, na.rm = TRUE),
    vehicle_rent_NVL = sum(rental_NVX, na.rm = TRUE),
    fuel_NVX_cost = sum(fuel_NVX, na.rm = TRUE),
    
    # Combine zeepot and refrigeration costs into storage_cost
    storage_cost = sum(proport_zeepot_cost, na.rm = TRUE) + sum(proportional_storage_cost, na.rm = TRUE),
    
    main_total = sum(total_cost, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate sub-component totals
yrly_ward_costs <- yrly_ward_costs %>%
  mutate(
    # Vaccination delivery costs (excluding storage)
    Vacc_delivery = rowSums(cbind(vaccine_bundle, labour_VX_cost, perdiem_VX_cost, travel_VX_cost,  
                                  fuel_VX_cost, vehicle_rent_VL), na.rm = TRUE),
    
    # Non-vaccination costs (including storage)
    non_vacc_NV = rowSums(cbind(labour_NVX_cost, perdiem_NVX_cost, travel_NVX_cost,
                                vehicle_rent_NVL, fuel_NVX_cost, storage_cost), na.rm = TRUE)
  )

# Calculate cost per dog for each component
per_dog_cost <- yrly_ward_costs %>%
  filter(total_dogs > 0) %>%  # Remove observations with 0 dogs
  group_by(district, ward, strategy, vaccination_year) %>%
  summarise(
    PD_vaccine_bundle = vaccine_bundle / total_dogs,
    PD_labour_VX_cost = labour_VX_cost / total_dogs,
    PD_perdiem_VX_cost = perdiem_VX_cost / total_dogs,
    PD_travel_VX_cost = travel_VX_cost / total_dogs,
    PD_fuel_VX_cost = fuel_VX_cost / total_dogs,
    PD_vehicle_rent_VL = vehicle_rent_VL / total_dogs,
    PD_Vacc_delivery = Vacc_delivery / total_dogs,
    
    PD_HH_trav = HH_trav / total_dogs,
    PD_HH_wait = HH_wait / total_dogs,
    PD_Household_costs = Household_costs / total_dogs,
    
    PD_labour_NVX_cost = labour_NVX_cost / total_dogs,
    PD_perdiem_NVX_cost = perdiem_NVX_cost / total_dogs,
    PD_travel_NVX_cost = travel_NVX_cost / total_dogs,
    PD_vehicle_rent_NVL = vehicle_rent_NVL / total_dogs,
    PD_fuel_NVX_cost = fuel_NVX_cost / total_dogs,
    PD_storage_cost = storage_cost / total_dogs,
    PD_non_vacc_NV = non_vacc_NV / total_dogs,
    
    PD_main_total = main_total / total_dogs,
    .groups = "drop"
  )

# Define all cost variables for analysis (updated to use storage_cost instead of separate components)
all_cost_variables <- c("PD_labour_VX_cost", "PD_perdiem_VX_cost", 
                        "PD_travel_VX_cost", "PD_fuel_VX_cost", "PD_vehicle_rent_VL", 
                        "PD_Vacc_delivery",
                        "PD_HH_trav", "PD_HH_wait", "PD_Household_costs", 
                        "PD_labour_NVX_cost", "PD_perdiem_NVX_cost", "PD_travel_NVX_cost", 
                        "PD_vehicle_rent_NVL", "PD_fuel_NVX_cost", "PD_storage_cost", "PD_non_vacc_NV",
                        "PD_main_total")

# Calculate means and standard deviations by strategy
overall_summary <- per_dog_cost %>%
  group_by(strategy) %>%
  summarise(across(all_of(all_cost_variables), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

# Perform t-tests for all cost variables
overall_ttests <- map_dfr(all_cost_variables, function(var) {
  test <- t.test(as.formula(paste(var, "~ strategy")), 
                 data = per_dog_cost, var.equal = FALSE)
  tidy(test) %>%
    mutate(variable = var) %>%
    dplyr::select(variable, estimate, estimate1, estimate2, statistic, parameter, p.value)
})

# Create final overall results table with desired column order
overall_results <- overall_ttests %>%
  rename(
    mean_diff = estimate,
    mean_continuous = estimate1,
    mean_pulsed = estimate2,
    t_stat = statistic,
    df = parameter,
    p_value = p.value
  ) %>%
  # Add standard deviations from summary
  left_join(
    overall_summary %>%
      dplyr::select(strategy, ends_with("_sd")) %>%
      pivot_longer(cols = -strategy, names_to = "temp", values_to = "sd") %>%
      mutate(variable = str_remove(temp, "_sd")) %>%
      dplyr::select(-temp) %>%
      pivot_wider(names_from = strategy, values_from = sd, names_prefix = "sd_"),
    by = "variable"
  ) %>%
  # Reorder columns as requested
  dplyr::select(variable, mean_diff, mean_pulsed, sd_pulsed, mean_continuous, sd_continuous, 
                t_stat, df, p_value) %>%
  # Format all numeric columns to 3 decimal places
  mutate(across(where(is.numeric), ~round(.x, 3)))

print("OVERALL COST PER DOG RESULTS:")
print(overall_results)

#################################################################################
# 2. RURAL AREA COST PER DOG ANALYSIS (ALL COMPONENTS)
#################################################################################

# Assign rural/urban status
yrly_ward_costs <- yrly_ward_costs %>%
  mutate(location = ifelse(district %in% c("Bunda Town Council", "Musoma Municipal Council",
                                           "Tarime Town Council"), "urban", "rural"))

# Filter for rural areas and calculate per dog costs
per_dog_cost_rural <- yrly_ward_costs %>%
  filter(location == "rural" & total_dogs > 0) %>%
  group_by(district, ward, strategy, vaccination_year) %>%
  summarise(
    PD_vaccine_bundle = vaccine_bundle / total_dogs,
    PD_labour_VX_cost = labour_VX_cost / total_dogs,
    PD_perdiem_VX_cost = perdiem_VX_cost / total_dogs,
    PD_travel_VX_cost = travel_VX_cost / total_dogs,
    PD_fuel_VX_cost = fuel_VX_cost / total_dogs,
    PD_vehicle_rent_VL = vehicle_rent_VL / total_dogs,
    PD_Vacc_delivery = Vacc_delivery / total_dogs,
    
    PD_HH_trav = HH_trav / total_dogs,
    PD_HH_wait = HH_wait / total_dogs,
    PD_Household_costs = Household_costs / total_dogs,
    
    PD_labour_NVX_cost = labour_NVX_cost / total_dogs,
    PD_perdiem_NVX_cost = perdiem_NVX_cost / total_dogs,
    PD_travel_NVX_cost = travel_NVX_cost / total_dogs,
    PD_vehicle_rent_NVL = vehicle_rent_NVL / total_dogs,
    PD_fuel_NVX_cost = fuel_NVX_cost / total_dogs,
    PD_storage_cost = storage_cost / total_dogs,
    PD_non_vacc_NV = non_vacc_NV / total_dogs,
    
    PD_main_total = main_total / total_dogs,
    .groups = "drop"
  )

# Calculate rural summary statistics
rural_summary <- per_dog_cost_rural %>%
  group_by(strategy) %>%
  summarise(across(all_of(all_cost_variables), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

# Perform t-tests for rural areas
rural_ttests <- map_dfr(all_cost_variables, function(var) {
  test <- t.test(as.formula(paste(var, "~ strategy")), 
                 data = per_dog_cost_rural, var.equal = FALSE)
  tidy(test) %>%
    mutate(variable = var) %>%
    dplyr::select(variable, estimate, estimate1, estimate2, statistic, parameter, p.value)
})

# Create final rural results table
rural_results <- rural_ttests %>%
  rename(
    mean_diff = estimate,
    mean_continuous = estimate1,
    mean_pulsed = estimate2,
    t_stat = statistic,
    df = parameter,
    p_value = p.value
  ) %>%
  left_join(
    rural_summary %>%
      dplyr::select(strategy, ends_with("_sd")) %>%
      pivot_longer(cols = -strategy, names_to = "temp", values_to = "sd") %>%
      mutate(variable = str_remove(temp, "_sd")) %>%
      dplyr::select(-temp) %>%
      pivot_wider(names_from = strategy, values_from = sd, names_prefix = "sd_"),
    by = "variable"
  ) %>%
  dplyr::select(variable, mean_diff, mean_pulsed, sd_pulsed, mean_continuous, sd_continuous, 
                t_stat, df, p_value) %>%
  # Format all numeric columns to 3 decimal places
  mutate(across(where(is.numeric), ~round(.x, 3)))

print("RURAL COST PER DOG RESULTS:")
print(rural_results)

#################################################################################
# 3. URBAN AREA COST PER DOG ANALYSIS (ALL COMPONENTS)
#################################################################################

# Filter for urban areas and calculate per dog costs
per_dog_cost_urban <- yrly_ward_costs %>%
  filter(location == "urban" & total_dogs > 0) %>%
  group_by(district, ward, strategy, vaccination_year) %>%
  summarise(
    PD_vaccine_bundle = vaccine_bundle / total_dogs,
    PD_labour_VX_cost = labour_VX_cost / total_dogs,
    PD_perdiem_VX_cost = perdiem_VX_cost / total_dogs,
    PD_travel_VX_cost = travel_VX_cost / total_dogs,
    PD_fuel_VX_cost = fuel_VX_cost / total_dogs,
    PD_vehicle_rent_VL = vehicle_rent_VL / total_dogs,
    PD_Vacc_delivery = Vacc_delivery / total_dogs,
    
    PD_HH_trav = HH_trav / total_dogs,
    PD_HH_wait = HH_wait / total_dogs,
    PD_Household_costs = Household_costs / total_dogs,
    
    PD_labour_NVX_cost = labour_NVX_cost / total_dogs,
    PD_perdiem_NVX_cost = perdiem_NVX_cost / total_dogs,
    PD_travel_NVX_cost = travel_NVX_cost / total_dogs,
    PD_vehicle_rent_NVL = vehicle_rent_NVL / total_dogs,
    PD_fuel_NVX_cost = fuel_NVX_cost / total_dogs,
    PD_storage_cost = storage_cost / total_dogs,
    PD_non_vacc_NV = non_vacc_NV / total_dogs,
    
    PD_main_total = main_total / total_dogs,
    .groups = "drop"
  )

# Calculate urban summary statistics
urban_summary <- per_dog_cost_urban %>%
  group_by(strategy) %>%
  summarise(across(all_of(all_cost_variables), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

# Perform t-tests for urban areas
urban_ttests <- map_dfr(all_cost_variables, function(var) {
  test <- t.test(as.formula(paste(var, "~ strategy")), 
                 data = per_dog_cost_urban, var.equal = FALSE)
  tidy(test) %>%
    mutate(variable = var) %>%
    dplyr::select(variable, estimate, estimate1, estimate2, statistic, parameter, p.value)
})

# Create final urban results table
urban_results <- urban_ttests %>%
  rename(
    mean_diff = estimate,
    mean_continuous = estimate1,
    mean_pulsed = estimate2,
    t_stat = statistic,
    df = parameter,
    p_value = p.value
  ) %>%
  left_join(
    urban_summary %>%
      dplyr::select(strategy, ends_with("_sd")) %>%
      pivot_longer(cols = -strategy, names_to = "temp", values_to = "sd") %>%
      mutate(variable = str_remove(temp, "_sd")) %>%
      dplyr::select(-temp) %>%
      pivot_wider(names_from = strategy, values_from = sd, names_prefix = "sd_"),
    by = "variable"
  ) %>%
  dplyr::select(variable, mean_diff, mean_pulsed, sd_pulsed, mean_continuous, sd_continuous, 
                t_stat, df, p_value) %>%
  # Format all numeric columns to 3 decimal places
  mutate(across(where(is.numeric), ~round(.x, 3)))

print("URBAN COST PER DOG RESULTS:")
print(urban_results)

#################################################################################
# 4. COST PER DOG BY VACCINATION YEAR (SUB-TOTALS ONLY)
#################################################################################

# Define sub-total variables
subtotal_variables <- c("PD_Vacc_delivery", "PD_Household_costs", "PD_non_vacc_NV", "PD_main_total")

# Calculate yearly summary statistics
yearly_summary <- per_dog_cost %>%
  group_by(vaccination_year, strategy) %>%
  summarise(across(all_of(subtotal_variables), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

# Get unique vaccination years
years <- unique(per_dog_cost$vaccination_year)

# Perform t-tests by year
yearly_ttests <- map_dfr(years, function(yr) {
  year_data <- per_dog_cost %>% filter(vaccination_year == yr)
  
  map_dfr(subtotal_variables, function(var) {
    test <- t.test(as.formula(paste(var, "~ strategy")), 
                   data = year_data, var.equal = FALSE)
    tidy(test) %>%
      mutate(variable = var, vaccination_year = yr) %>%
      dplyr::select(vaccination_year, variable, estimate, estimate1, estimate2, 
                    statistic, parameter, p.value)
  })
})

# Create final yearly results table
yearly_results <- yearly_ttests %>%
  rename(
    mean_diff = estimate,
    mean_continuous = estimate1,
    mean_pulsed = estimate2,
    t_stat = statistic,
    df = parameter,
    p_value = p.value
  ) %>%
  left_join(
    yearly_summary %>%
      dplyr::select(vaccination_year, strategy, ends_with("_sd")) %>%
      pivot_longer(cols = c(-vaccination_year, -strategy), names_to = "temp", values_to = "sd") %>%
      mutate(variable = str_remove(temp, "_sd")) %>%
      dplyr::select(-temp) %>%
      pivot_wider(names_from = strategy, values_from = sd, names_prefix = "sd_"),
    by = c("vaccination_year", "variable")
  ) %>%
  dplyr::select(vaccination_year, variable, mean_diff, mean_pulsed, sd_pulsed, 
                mean_continuous, sd_continuous, t_stat, df, p_value) %>%
  # Format all numeric columns to 3 decimal places
  mutate(across(where(is.numeric), ~round(.x, 3)))

print("YEARLY COST PER DOG RESULTS (SUB-TOTALS):")
print(yearly_results)

#################################################################################
# 5. COST PER DOG BY DISTRICT (SUB-TOTALS ONLY)
#################################################################################

# Calculate district summary statistics
district_summary <- per_dog_cost %>%
  group_by(district, strategy) %>%
  summarise(across(all_of(subtotal_variables), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

# Get unique districts
districts <- unique(per_dog_cost$district)

# Perform t-tests by district
district_ttests <- map_dfr(districts, function(dist) {
  district_data <- per_dog_cost %>% filter(district == dist)
  
  # Check if both strategies exist
  if(length(unique(district_data$strategy)) < 2) return(NULL)
  
  map_dfr(subtotal_variables, function(var) {
    test <- t.test(as.formula(paste(var, "~ strategy")), 
                   data = district_data, var.equal = FALSE)
    tidy(test) %>%
      mutate(variable = var, district = dist) %>%
      dplyr::select(district, variable, estimate, estimate1, estimate2, 
                    statistic, parameter, p.value)
  })
})

# Create final district results table
district_results <- district_ttests %>%
  rename(
    mean_diff = estimate,
    mean_continuous = estimate1,
    mean_pulsed = estimate2,
    t_stat = statistic,
    df = parameter,
    p_value = p.value
  ) %>%
  left_join(
    district_summary %>%
      dplyr::select(district, strategy, ends_with("_sd")) %>%
      pivot_longer(cols = c(-district, -strategy), names_to = "temp", values_to = "sd") %>%
      mutate(variable = str_remove(temp, "_sd")) %>%
      dplyr::select(-temp) %>%
      pivot_wider(names_from = strategy, values_from = sd, names_prefix = "sd_"),
    by = c("district", "variable")
  ) %>%
  dplyr::select(district, variable, mean_diff, mean_pulsed, sd_pulsed, 
                mean_continuous, sd_continuous, t_stat, df, p_value) %>%
  # Format all numeric columns to 3 decimal places
  mutate(across(where(is.numeric), ~round(.x, 3)))

print("DISTRICT COST PER DOG RESULTS (SUB-TOTALS):")
print(district_results)

#################################################################################
# 6. COST PER DOG BY DELIVERY APPROACH (SUB-TOTALS ONLY)
#################################################################################

# Create simplified delivery strategy classification
monthly_ward_costs <- monthly_ward_costs %>%
  mutate(
    delivery_simplified = case_when(
      Delivery.strategy.type %in% c("House to House", "On Demand", "Visit for Other Reasons") ~ "Household_Level",
      Delivery.strategy.type == "Central Point - Subvillage" ~ "Central_Point_Subvillage",
      Delivery.strategy.type == "Central Point - Village" ~ "Central_Point_Village",
      TRUE ~ Delivery.strategy.type
    )
  )

# Aggregate costs by delivery approach
delivery_ward_costs <- monthly_ward_costs %>%
  group_by(district, ward, strategy, vaccination_year, delivery_simplified) %>%
  summarise(
    total_dogs = sum(vaccinated_dogs, na.rm = TRUE),
    vaccine_bundle = sum(vaccination_cost, na.rm = TRUE),
    labour_VX_cost = sum(labour_PR_VX, na.rm = TRUE),
    perdiem_VX_cost = sum(perdiem_PR_VX, na.rm = TRUE),
    travel_VX_cost = sum(Travel_PR_VX, na.rm = TRUE),
    fuel_VX_cost = sum(fuel_VX, na.rm = TRUE),
    vehicle_rent_VL = sum(rental_cost, na.rm = TRUE),
    
    # Combine storage costs
    storage_cost = sum(proport_zeepot_cost, na.rm = TRUE) + sum(proportional_storage_cost, na.rm = TRUE),
    
    HH_wait = sum(HH_wait_cost, na.rm = TRUE),
    HH_trav = sum(HH_trav_cost, na.rm = TRUE),
    Household_costs = sum(HH_cost, na.rm = TRUE),
    
    labour_NVX_cost = sum(labour_PR_NVX, na.rm = TRUE),
    perdiem_NVX_cost = sum(perdiem_PR_NVX, na.rm = TRUE),
    travel_NVX_cost = sum(Travel_PR_NVX, na.rm = TRUE),
    vehicle_rent_NVL = sum(rental_NVX, na.rm = TRUE),
    fuel_NVX_cost = sum(fuel_NVX, na.rm = TRUE),
    
    main_total = sum(total_cost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_dogs > 0) %>%  # Remove observations with 0 dogs
  mutate(
    # Vaccination delivery costs (excluding storage)
    Vacc_delivery = rowSums(cbind(vaccine_bundle, labour_VX_cost, perdiem_VX_cost, travel_VX_cost,  
                                  fuel_VX_cost, vehicle_rent_VL), na.rm = TRUE),
    
    # Non-vaccination costs (including storage)
    non_vacc_NV = rowSums(cbind(labour_NVX_cost, perdiem_NVX_cost, travel_NVX_cost,
                                vehicle_rent_NVL, fuel_NVX_cost, storage_cost), na.rm = TRUE)
  )

# Calculate per dog costs by delivery approach
delivery_per_dog_cost <- delivery_ward_costs %>%
  mutate(
    PD_Vacc_delivery = Vacc_delivery / total_dogs,
    PD_Household_costs = Household_costs / total_dogs,
    PD_non_vacc_NV = non_vacc_NV / total_dogs,
    PD_main_total = main_total / total_dogs
  )

# Calculate delivery summary statistics
delivery_summary <- delivery_per_dog_cost %>%
  group_by(delivery_simplified, strategy) %>%
  summarise(across(all_of(subtotal_variables), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

# Get unique delivery approaches
delivery_types <- unique(delivery_per_dog_cost$delivery_simplified)

# Perform t-tests by delivery approach
delivery_ttests <- map_dfr(delivery_types, function(deliv) {
  delivery_data <- delivery_per_dog_cost %>% filter(delivery_simplified == deliv)
  
  # Check if both strategies exist
  if(length(unique(delivery_data$strategy)) < 2) return(NULL)
  
  map_dfr(subtotal_variables, function(var) {
    test <- t.test(as.formula(paste(var, "~ strategy")), 
                   data = delivery_data, var.equal = FALSE)
    tidy(test) %>%
      mutate(variable = var, delivery_simplified = deliv) %>%
      dplyr::select(delivery_simplified, variable, estimate, estimate1, estimate2, 
                    statistic, parameter, p.value)
  })
})

# Create final delivery results table
delivery_results <- delivery_ttests %>%
  rename(
    mean_diff = estimate,
    mean_continuous = estimate1,
    mean_pulsed = estimate2,
    t_stat = statistic,
    df = parameter,
    p_value = p.value
  ) %>%
  left_join(
    delivery_summary %>%
      dplyr::select(delivery_simplified, strategy, ends_with("_sd")) %>%
      pivot_longer(cols = c(-delivery_simplified, -strategy), names_to = "temp", values_to = "sd") %>%
      mutate(variable = str_remove(temp, "_sd")) %>%
      dplyr::select(-temp) %>%
      pivot_wider(names_from = strategy, values_from = sd, names_prefix = "sd_"),
    by = c("delivery_simplified", "variable")
  ) %>%
  dplyr::select(delivery_simplified, variable, mean_diff, mean_pulsed, sd_pulsed, 
                mean_continuous, sd_continuous, t_stat, df, p_value) %>%
  # Format all numeric columns to 3 decimal places
  mutate(across(where(is.numeric), ~round(.x, 3)))

print("DELIVERY APPROACH COST PER DOG RESULTS (SUB-TOTALS):")
print(delivery_results)






