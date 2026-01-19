

# ========================================================================================
# COMPLETE MUSOMA MUNICIPAL COUNCIL ANALYSIS - FULL SCRIPT
# ========================================================================================

# Clear environment
rm(list = ls())

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# ========================================================================================
# STEP 1: DATA LOADING AND PREPARATION
# ========================================================================================

cat("STEP 1: DATA LOADING AND PREPARATION\n")

# Load dataset
by_location_calc <- read.csv("data/monthly_expanded_costs.csv", stringsAsFactors = FALSE)

# Select relevant cost variables for detailed analysis
summarised_data <- by_location_calc %>%
  dplyr::select(
    district, ward, strategy, vaccination_year, month_interval, Delivery.strategy.type,
    vaccinated_dogs, vaccination_cost, fuel_VX, fuel_NVX, rental_cost, rental_NVX, 
    labour_PR_VX, labour_PR_NVX, Travel_PR_VX, Travel_PR_NVX, perdiem_PR_VX, 
    perdiem_PR_NVX, HH_cost, proportional_storage_cost, proport_zeepot_cost, total_cost
  )

# Extract Musoma Municipal Council data and add transformations WITHOUT filtering
musoma_data <- summarised_data %>%
  filter(district == "Musoma Municipal Council") %>%
  mutate(
    # Create quarterly intervals (vaccination year: Nov-Oct)
    quarter = case_when(
      month_interval %in% c(11, 12, 1) ~ "Q1",  # Nov-Jan (campaign start)
      month_interval %in% c(2, 3, 4) ~ "Q2",    # Feb-Apr 
      month_interval %in% c(5, 6, 7) ~ "Q3",    # May-Jul
      month_interval %in% c(8, 9, 10) ~ "Q4",   # Aug-Oct (campaign end)
      TRUE ~ paste0("M", month_interval)
    ),
    # Simplify delivery strategy names
    delivery_simplified = case_when(
      Delivery.strategy.type %in% c("House to House", "On Demand", "Visit for Other Reasons") ~ "Household_Level",
      Delivery.strategy.type == "Central Point - Subvillage" ~ "Central_Point_Subvillage",
      Delivery.strategy.type == "Central Point - Village" ~ "Central_Point_Village",
      TRUE ~ gsub(" ", "_", Delivery.strategy.type)
    ),
    # Calculate cost per dog (but don't filter based on this)
    cost_per_dog = ifelse(vaccinated_dogs > 0, total_cost / vaccinated_dogs, NA_real_)
  )
# NO FILTERING HERE - keep all observations like in per_DogCost1

# Extract all other districts for comparison (also without aggressive filtering)
other_districts_data <- summarised_data %>%
  filter(district != "Musoma Municipal Council") %>%
  mutate(
    # Create quarterly intervals (vaccination year: Nov-Oct)
    quarter = case_when(
      month_interval %in% c(11, 12, 1) ~ "Q1",  # Nov-Jan (campaign start)
      month_interval %in% c(2, 3, 4) ~ "Q2",    # Feb-Apr 
      month_interval %in% c(5, 6, 7) ~ "Q3",    # May-Jul
      month_interval %in% c(8, 9, 10) ~ "Q4",   # Aug-Oct (campaign end)
      TRUE ~ paste0("M", month_interval)
    ),
    # Simplify delivery strategy names
    delivery_simplified = case_when(
      Delivery.strategy.type %in% c("House to House", "On Demand", "Visit for Other Reasons") ~ "Household_Level",
      Delivery.strategy.type == "Central Point - Subvillage" ~ "Central_Point_Subvillage",
      Delivery.strategy.type == "Central Point - Village" ~ "Central_Point_Village",
      TRUE ~ gsub(" ", "_", Delivery.strategy.type)
    ),
    # Calculate cost per dog (but don't filter based on this)
    cost_per_dog = ifelse(vaccinated_dogs > 0, total_cost / vaccinated_dogs, NA_real_)
  )

# ========================================================================================
# STEP 2: AGGREGATE TO WARD-YEAR LEVEL (PRIMARY UNIT OF ANALYSIS)
# ========================================================================================

cat("\n\nSTEP 2: AGGREGATING TO WARD-YEAR LEVEL\n")
cat("=======================================\n")

# Ward-year level data for Musoma (primary unit of analysis)
# Using same aggregation approach as per_DogCost1 with na.rm = TRUE
musoma_ward_year <- musoma_data %>%
  group_by(ward, strategy, vaccination_year) %>%
  summarise(
    n_monthly_obs = n(),
    total_cost = sum(total_cost, na.rm = TRUE),
    total_dogs = sum(vaccinated_dogs, na.rm = TRUE),
    n_quarters = n_distinct(quarter),
    quarters_active = paste(sort(unique(quarter)), collapse = ", "),
    delivery_methods = paste(sort(unique(delivery_simplified)), collapse = ", "),
    
    # Cost components (aggregated)
    vaccination_cost_total = sum(vaccination_cost, na.rm = TRUE),
    fuel_total = sum(fuel_VX + fuel_NVX, na.rm = TRUE),
    rental_total = sum(rental_cost + rental_NVX, na.rm = TRUE),
    labour_total = sum(labour_PR_VX + labour_PR_NVX, na.rm = TRUE),
    labour_VX = sum(labour_PR_VX, na.rm = TRUE),
    labour_NVX = sum(labour_PR_NVX, na.rm = TRUE),
    travel_total = sum(Travel_PR_VX + Travel_PR_NVX, na.rm = TRUE),
    perdiem_total = sum(perdiem_PR_VX + perdiem_PR_NVX, na.rm = TRUE),
    HH_cost_total = sum(HH_cost, na.rm = TRUE),
    storage_total = sum(proportional_storage_cost + proport_zeepot_cost, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  mutate(
    cost_per_dog = ifelse(total_dogs > 0, total_cost / total_dogs, NA_real_),
    
    # Cost components per dog
    vaccination_cost_per_dog = vaccination_cost_total / total_dogs,
    fuel_per_dog = fuel_total / total_dogs,
    rental_per_dog = rental_total / total_dogs,
    labour_per_dog = labour_total / total_dogs,
    labour_VX_perDog = labour_VX / total_dogs,
    labour_NVX_perDog = labour_NVX / total_dogs,
    travel_per_dog = travel_total / total_dogs,
    perdiem_per_dog = perdiem_total / total_dogs,
    HH_cost_per_dog = HH_cost_total / total_dogs,
    storage_per_dog = storage_total / total_dogs
  )
# NO FILTERING at the end - keep all ward-year combinations like per_DogCost1

# Same for other districts
other_districts_ward_year <- other_districts_data %>%
  group_by(district, ward, strategy, vaccination_year) %>%
  summarise(
    n_monthly_obs = n(),
    total_cost = sum(total_cost, na.rm = TRUE),
    total_dogs = sum(vaccinated_dogs, na.rm = TRUE),
    n_quarters = n_distinct(quarter),
    .groups = 'drop'
  ) %>%
  mutate(
    cost_per_dog = ifelse(total_dogs > 0, total_cost / total_dogs, NA_real_)
  )

# ========================================================================================
# STEP 3: BASIC DESCRIPTIVE STATISTICS
# ========================================================================================

cat("\n\nSTEP 3: DESCRIPTIVE STATISTICS\n")
cat("===============================\n")

# Musoma summary statistics
musoma_summary <- musoma_ward_year %>%
  group_by(strategy, ward) %>%
  summarise(
    n_ward_years = n(),
    n_unique_wards = n_distinct(ward),
    total_cost_all = sum(total_cost, na.rm = TRUE),
    total_dogs_all = sum(total_dogs, na.rm = TRUE),
    overall_cost_per_dog = total_cost_all / total_dogs_all,
    mean_cost_per_dog = mean(cost_per_dog, na.rm = TRUE),
    median_cost_per_dog = median(cost_per_dog, na.rm = TRUE),
    sd_cost_per_dog = sd(cost_per_dog, na.rm = TRUE),
    min_cost_per_dog = min(cost_per_dog, na.rm = TRUE),
    max_cost_per_dog = max(cost_per_dog, na.rm = TRUE),
    q25_cost_per_dog = quantile(cost_per_dog, 0.25, na.rm = TRUE),
    q75_cost_per_dog = quantile(cost_per_dog, 0.75, na.rm = TRUE),
    .groups = 'drop'
  )

# Summary by strategy only (aggregated across wards)
musoma_strategy_summary <- musoma_ward_year %>%
  group_by(strategy) %>%
  summarise(
    mean_cost_per_dog = mean(cost_per_dog, na.rm = TRUE),
    median_cost_per_dog = median(cost_per_dog, na.rm = TRUE),
    sd_cost_per_dog = sd(cost_per_dog, na.rm = TRUE),
    min_cost_per_dog = min(cost_per_dog, na.rm = TRUE),
    max_cost_per_dog = max(cost_per_dog, na.rm = TRUE),
    .groups = 'drop'
  )
cat("\nMUSOMA STRATEGY-LEVEL SUMMARY:\n")
print(musoma_strategy_summary)

# Other districts summary
other_districts_summary <- other_districts_ward_year %>%
  group_by(strategy, ward) %>%
  summarise(
    n_ward_years = n(),
    n_unique_wards = n_distinct(ward),
    total_cost_all = sum(total_cost, na.rm = TRUE),
    total_dogs_all = sum(total_dogs, na.rm = TRUE),
    overall_cost_per_dog = total_cost_all / total_dogs_all,
    mean_cost_per_dog = mean(cost_per_dog, na.rm = TRUE),
    median_cost_per_dog = median(cost_per_dog, na.rm = TRUE),
    sd_cost_per_dog = sd(cost_per_dog, na.rm = TRUE),
    min_cost_per_dog = min(cost_per_dog, na.rm = TRUE),
    max_cost_per_dog = max(cost_per_dog, na.rm = TRUE),
    q25_cost_per_dog = quantile(cost_per_dog, 0.25, na.rm = TRUE),
    q75_cost_per_dog = quantile(cost_per_dog, 0.75, na.rm = TRUE),
    .groups = 'drop'
  )

other_strategy_summary <- other_districts_ward_year %>%
  group_by(strategy) %>%
  summarise(
    mean_cost_per_dog = mean(cost_per_dog, na.rm = TRUE),
    median_cost_per_dog = median(cost_per_dog, na.rm = TRUE),
    sd_cost_per_dog = sd(cost_per_dog, na.rm = TRUE),
    min_cost_per_dog = min(cost_per_dog, na.rm = TRUE),
    max_cost_per_dog = max(cost_per_dog, na.rm = TRUE),
    .groups = 'drop'
  )
cat("\nOTHER DISTRICTS STRATEGY-LEVEL SUMMARY:\n")
print(other_strategy_summary)

all_districts <- rbind(musoma_summary, other_districts_summary)

cat("MUSOMA SUMMARY (by strategy):\n")
print(musoma_summary)

cat("\nOTHER DISTRICTS SUMMARY (by strategy):\n")
print(other_districts_summary)

# ========================================================================================
# STEP 4: OUTLIER IDENTIFICATION
# ========================================================================================

cat("\n\nSTEP 4: OUTLIER IDENTIFICATION\n")
cat("===============================\n")

# Function to identify outliers
identify_outliers <- function(data, title) {
  Q1 <- quantile(data$cost_per_dog, 0.25, na.rm = TRUE)
  Q3 <- quantile(data$cost_per_dog, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  outliers <- data %>%
    filter(cost_per_dog < lower_bound | cost_per_dog > upper_bound) %>%
    mutate(
      outlier_type = case_when(
        cost_per_dog < lower_bound ~ "Low outlier",
        cost_per_dog > upper_bound ~ "High outlier",
        TRUE ~ "Normal"
      )
    ) %>%
    arrange(desc(cost_per_dog))
  
  cat(title, ":\n")
  cat("Total ward-year observations:", nrow(data), "\n")
  cat("Outlier observations:", nrow(outliers), "(", round((nrow(outliers)/nrow(data))*100, 1), "%)\n")
  cat("Cost per dog range: $", round(min(data$cost_per_dog, na.rm = TRUE), 2), " to $", 
      round(max(data$cost_per_dog, na.rm = TRUE), 2), "\n")
  cat("IQR bounds: $", round(lower_bound, 2), " to $", round(upper_bound, 2), "\n\n")
  
  return(outliers)
}

# Identify outliers
musoma_outliers <- identify_outliers(musoma_ward_year, "MUSOMA OUTLIERS")
other_outliers <- identify_outliers(other_districts_ward_year, "OTHER DISTRICTS OUTLIERS")

# Show top Musoma outliers
if(nrow(musoma_outliers) > 0) {
  cat("TOP 10 MUSOMA OUTLIERS:\n")
  print(musoma_outliers %>% 
          dplyr::select(ward, strategy, vaccination_year, cost_per_dog, total_cost, total_dogs, 
                        quarters_active, delivery_methods, outlier_type) %>%
          head(10))
}

# Show top other district outliers
if(nrow(other_outliers) > 0) {
  cat("TOP OTHER DISTRICT OUTLIERS:\n")
  print(other_outliers %>% 
          dplyr::select(ward, strategy, vaccination_year, cost_per_dog, total_cost, total_dogs, 
                        outlier_type) %>%
          head(10))
}
# ========================================================================================
# STEP 5: WARD-LEVEL PATTERNS
# ========================================================================================

cat("\n\nSTEP 5: WARD-LEVEL PATTERNS\n")
cat("============================\n")

# Ward performance across years
ward_performance <- musoma_ward_year %>%
  group_by(ward) %>%
  summarise(
    n_ward_years = n(),
    strategies_used = paste(sort(unique(strategy)), collapse = ", "),
    years_active = paste(sort(unique(vaccination_year)), collapse = ", "),
    avg_cost_per_dog = mean(cost_per_dog, na.rm = TRUE),
    median_cost_per_dog = median(cost_per_dog, na.rm = TRUE),
    min_cost_per_dog = min(cost_per_dog, na.rm = TRUE),
    max_cost_per_dog = max(cost_per_dog, na.rm = TRUE),
    cost_variability = max_cost_per_dog - min_cost_per_dog,
    total_dogs_all_years = sum(total_dogs, na.rm = TRUE),
    total_cost_all_years = sum(total_cost, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_cost_per_dog))

cat("WARD PERFORMANCE ACROSS YEARS:\n")
print(ward_performance)

# Which wards appear most in outliers?
if(nrow(musoma_outliers) > 0) {
  ward_outlier_patterns <- musoma_outliers %>%
    count(ward, outlier_type, name = "n_outlier_years") %>%
    arrange(desc(n_outlier_years))
  
  cat("\nWARDS WITH MOST OUTLIER YEARS:\n")
  print(ward_outlier_patterns)
  
  # Which years have most outliers?
  year_outlier_patterns <- musoma_outliers %>%
    count(vaccination_year, outlier_type, name = "n_outlier_wards") %>%
    arrange(vaccination_year)
  
  cat("\nYEARS WITH MOST OUTLIER WARD-YEARS:\n")
  print(year_outlier_patterns)
  
  # Which strategies in outliers?
  strategy_outlier_patterns <- musoma_outliers %>%
    count(strategy, outlier_type, name = "n_outlier_observations") %>%
    arrange(desc(n_outlier_observations))
  
  cat("\nSTRATEGIES IN OUTLIER OBSERVATIONS:\n")
  print(strategy_outlier_patterns)
}

# ========================================================================================
# STEP 6: TEMPORAL PATTERNS
# ========================================================================================

cat("\n\nSTEP 6: TEMPORAL PATTERNS\n")
cat("==========================\n")

# Year-by-year comparison
yearly_comparison <- musoma_ward_year %>%
  group_by(vaccination_year, strategy) %>%
  summarise(
    n_ward_years = n(),
    n_wards = n_distinct(ward),
    total_cost = sum(total_cost, na.rm = TRUE),
    total_dogs = sum(total_dogs, na.rm = TRUE),
    overall_cost_per_dog = total_cost / total_dogs,
    avg_cost_per_dog = mean(cost_per_dog, na.rm = TRUE),
    median_cost_per_dog = median(cost_per_dog, na.rm = TRUE),
    min_cost_per_dog = min(cost_per_dog, na.rm = TRUE),
    max_cost_per_dog = max(cost_per_dog, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(vaccination_year, strategy)

cat("YEAR-BY-YEAR COMPARISON:\n")
print(yearly_comparison)

# Quarter patterns in outliers
if(nrow(musoma_outliers) > 0) {
  # Extract quarter information from outliers
  outlier_quarter_patterns <- musoma_outliers %>%
    separate_rows(quarters_active, sep = ", ") %>%
    count(quarters_active, outlier_type, name = "n_outlier_observations") %>%
    arrange(quarters_active, desc(n_outlier_observations))
  
  cat("\nQUARTER PATTERNS IN OUTLIERS:\n")
  print(outlier_quarter_patterns)
}

# ========================================================================================
# STEP 7: COST COMPONENT ANALYSIS FOR OUTLIERS
# ========================================================================================

cat("\n\nSTEP 7: COST COMPONENT ANALYSIS\n")
cat("================================\n")

if(nrow(musoma_outliers) > 0) {
  # Analyze cost components for top outliers
  outlier_components <- musoma_outliers %>%
    dplyr::select(ward, strategy, vaccination_year, cost_per_dog, total_cost, total_dogs,
                  vaccination_cost_per_dog, fuel_per_dog, rental_per_dog, labour_per_dog, labour_VX_perDog,
                  labour_NVX_perDog, travel_per_dog, perdiem_per_dog, HH_cost_per_dog, storage_per_dog) %>%
    head(10)
  
  cat("COST COMPONENT BREAKDOWN FOR TOP 10 OUTLIERS:\n")
  print(outlier_components)
  
  # Which cost components drive high costs?
  component_analysis <- musoma_outliers %>%
    summarise(
      avg_vaccination_cost = mean(vaccination_cost_per_dog, na.rm = TRUE),
      avg_fuel_cost = mean(fuel_per_dog, na.rm = TRUE),
      avg_rental_cost = mean(rental_per_dog, na.rm = TRUE),
      avg_labour_cost = mean(labour_per_dog, na.rm = TRUE),
      avg_labour_VX = mean(labour_VX_perDog, na.rm = TRUE),
      avg_labour_NVX = mean(labour_NVX_perDog, na.rm = TRUE),
      avg_travel_cost = mean(travel_per_dog, na.rm = TRUE),
      avg_perdiem_cost = mean(perdiem_per_dog, na.rm = TRUE),
      avg_HH_cost = mean(HH_cost_per_dog, na.rm = TRUE),
      avg_storage_cost = mean(storage_per_dog, na.rm = TRUE)
    ) %>%
    pivot_longer(everything(), names_to = "component", values_to = "avg_cost_per_dog") %>%
    arrange(desc(avg_cost_per_dog))
  
  cat("\nAVERAGE COST COMPONENTS IN OUTLIERS:\n")
  print(component_analysis)
}

# Calculate normal non-vaccination labor costs (excluding outliers)
normal_labor_costs <- musoma_ward_year %>%
  filter(ward != "Mwigobero") %>%
  summarise(avg_labor_per_dog = mean(labour_NVX_perDog, na.rm = TRUE))

# costs components for other districts
other_districts_ward_year_detailed <- other_districts_data %>%
  group_by(district, ward, strategy, vaccination_year) %>%
  summarise(
    n_monthly_obs = n(),
    total_cost = sum(total_cost, na.rm = TRUE),
    total_dogs = sum(vaccinated_dogs, na.rm = TRUE),
    n_quarters = n_distinct(quarter),
    quarters_active = paste(sort(unique(quarter)), collapse = ", "),
    delivery_methods = paste(sort(unique(delivery_simplified)), collapse = ", "),
    
    # Cost components (aggregated) - same as Musoma analysis
    vaccination_cost_total = sum(vaccination_cost, na.rm = TRUE),
    fuel_total = sum(fuel_VX + fuel_NVX, na.rm = TRUE),
    rental_total = sum(rental_cost + rental_NVX, na.rm = TRUE),
    labour_total = sum(labour_PR_VX + labour_PR_NVX, na.rm = TRUE),
    labour_VX = sum(labour_PR_VX, na.rm = TRUE),
    labour_NVX = sum(labour_PR_NVX, na.rm = TRUE),
    travel_total = sum(Travel_PR_VX + Travel_PR_NVX, na.rm = TRUE),
    perdiem_total = sum(perdiem_PR_VX + perdiem_PR_NVX, na.rm = TRUE),
    HH_cost_total = sum(HH_cost, na.rm = TRUE),
    storage_total = sum(proportional_storage_cost + proport_zeepot_cost, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  mutate(
    cost_per_dog = ifelse(total_dogs > 0, total_cost / total_dogs, NA_real_),
    
    # Cost components per dog
    vaccination_cost_per_dog = vaccination_cost_total / total_dogs,
    fuel_per_dog = fuel_total / total_dogs,
    rental_per_dog = rental_total / total_dogs,
    labour_per_dog = labour_total / total_dogs,
    labour_VX_perDog = labour_VX / total_dogs,
    labour_NVX_perDog = labour_NVX / total_dogs,
    travel_per_dog = travel_total / total_dogs,
    perdiem_per_dog = perdiem_total / total_dogs,
    HH_cost_per_dog = HH_cost_total / total_dogs,
    storage_per_dog = storage_total / total_dogs
  ) %>%
  filter(total_dogs > 0, is.finite(cost_per_dog))

# Apply the same outlier identification function
other_districts_outliers <- identify_outliers(other_districts_ward_year_detailed, "OTHER DISTRICTS OUTLIERS (DETAILED)")

# Show top outliers from other districts
if(nrow(other_districts_outliers) > 0) {
  cat("TOP 15 OTHER DISTRICTS OUTLIERS:\n")
  print(other_districts_outliers %>% 
          dplyr::select(district, ward, strategy, vaccination_year, cost_per_dog, total_cost, total_dogs, 
                        quarters_active, delivery_methods, outlier_type) %>%
          head(15))
  
  # District patterns in outliers
  district_outlier_patterns <- other_districts_outliers %>%
    count(district, outlier_type, name = "n_outlier_years") %>%
    arrange(desc(n_outlier_years))
  
  cat("\nDISTRICTS WITH MOST OUTLIER YEARS:\n")
  print(district_outlier_patterns)
  
  
  # Strategy patterns in outliers
  strategy_outlier_patterns_other <- other_districts_outliers %>%
    count(strategy, outlier_type, name = "n_outlier_observations") %>%
    arrange(desc(n_outlier_observations))
  
  cat("\nSTRATEGIES IN OTHER DISTRICTS OUTLIER OBSERVATIONS:\n")
  print(strategy_outlier_patterns_other)
}


if(nrow(other_districts_outliers) > 0) {
  # Analyze cost components for top outliers in other districts
  other_outlier_components <- other_districts_outliers %>%
    dplyr::select(district, ward, strategy, vaccination_year, cost_per_dog, total_cost, total_dogs,
                  vaccination_cost_per_dog, fuel_per_dog, rental_per_dog, labour_per_dog, 
                  labour_VX_perDog, labour_NVX_perDog, travel_per_dog, perdiem_per_dog, 
                  HH_cost_per_dog, storage_per_dog, outlier_type) %>%
    head(15)
  
  cat("COST COMPONENT BREAKDOWN FOR TOP 15 OTHER DISTRICTS OUTLIERS:\n")
  print(other_outlier_components)
  
  # Which cost components drive high costs in other districts?
  other_component_analysis <- other_districts_outliers %>%
    summarise(
      avg_vaccination_cost = mean(vaccination_cost_per_dog, na.rm = TRUE),
      avg_fuel_cost = mean(fuel_per_dog, na.rm = TRUE),
      avg_rental_cost = mean(rental_per_dog, na.rm = TRUE),
      avg_labour_cost = mean(labour_per_dog, na.rm = TRUE),
      avg_labour_VX = mean(labour_VX_perDog, na.rm = TRUE),
      avg_labour_NVX = mean(labour_NVX_perDog, na.rm = TRUE),
      avg_travel_cost = mean(travel_per_dog, na.rm = TRUE),
      avg_perdiem_cost = mean(perdiem_per_dog, na.rm = TRUE),
      avg_HH_cost = mean(HH_cost_per_dog, na.rm = TRUE),
      avg_storage_cost = mean(storage_per_dog, na.rm = TRUE)
    ) %>%
    pivot_longer(everything(), names_to = "component", values_to = "avg_cost_per_dog") %>%
    arrange(desc(avg_cost_per_dog))
  
  cat("\nAVERAGE COST COMPONENTS IN OTHER DISTRICTS OUTLIERS:\n")
  print(other_component_analysis)
  
  # Compare outlier component costs between Musoma and Other Districts
  if(exists("component_analysis")) {
    comparison_components <- bind_rows(
      component_analysis %>% mutate(location = "Musoma"),
      other_component_analysis %>% mutate(location = "Other_Districts")
    ) %>%
      pivot_wider(names_from = location, values_from = avg_cost_per_dog) %>%
      mutate(
        difference = Other_Districts - Musoma,
        ratio = Other_Districts / Musoma
      ) %>%
      arrange(desc(abs(difference)))
    
    cat("\nCOMPARISON OF OUTLIER COMPONENTS: MUSOMA vs OTHER DISTRICTS:\n")
    print(comparison_components)
  }
}
# ========================================================================================
# STEP 8: DELIVERY METHOD PATTERNS
# ========================================================================================

cat("\n\nSTEP 8: DELIVERY METHOD PATTERNS\n")
cat("=================================\n")

# Delivery method summary
delivery_summary <- musoma_ward_year %>%
  separate_rows(delivery_methods, sep = ", ") %>%
  group_by(delivery_methods, strategy) %>%
  summarise(
    n_observations = n(),
    avg_cost_per_dog = mean(cost_per_dog, na.rm = TRUE),
    median_cost_per_dog = median(cost_per_dog, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(strategy, desc(avg_cost_per_dog))

cat("DELIVERY METHOD SUMMARY:\n")
print(delivery_summary)

# 1. Mwigobero ward only - by delivery method and strategy
mwigobero_delivery_summary <- musoma_ward_year %>%
  filter(ward == "Mwigobero") %>%
  separate_rows(delivery_methods, sep = ", ") %>%
  group_by(delivery_methods, strategy) %>%
  summarise(
    n_observations = n(),
    avg_cost_per_dog = mean(cost_per_dog, na.rm = TRUE),
    median_cost_per_dog = median(cost_per_dog, na.rm = TRUE),
    min_cost_per_dog = min(cost_per_dog, na.rm = TRUE),
    max_cost_per_dog = max(cost_per_dog, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(ward_group = "Mwigobero Only") %>%
  arrange(strategy, desc(avg_cost_per_dog))

# 2. All other Musoma wards (excluding Mwigobero) - by delivery method and strategy
non_mwigobero_delivery_summary <- musoma_ward_year %>%
  filter(ward != "Mwigobero") %>%
  separate_rows(delivery_methods, sep = ", ") %>%
  group_by(delivery_methods, strategy) %>%
  summarise(
    n_observations = n(),
    avg_cost_per_dog = mean(cost_per_dog, na.rm = TRUE),
    median_cost_per_dog = median(cost_per_dog, na.rm = TRUE),
    min_cost_per_dog = min(cost_per_dog, na.rm = TRUE),
    max_cost_per_dog = max(cost_per_dog, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(ward_group = "Excluding Mwigobero") %>%
  arrange(strategy, desc(avg_cost_per_dog))


# Delivery methods in outliers
if(nrow(musoma_outliers) > 0) {
  outlier_delivery <- musoma_outliers %>%
    separate_rows(delivery_methods, sep = ", ") %>%
    count(delivery_methods, outlier_type, name = "n_outlier_observations") %>%
    arrange(desc(n_outlier_observations))
  
  cat("\nDELIVERY METHODS IN OUTLIERS:\n")
  print(outlier_delivery)
}

# ========================================================================================
# STEP 9: IMPACT ASSESSMENT
# ========================================================================================

cat("\n\nSTEP 9: IMPACT ASSESSMENT\n")
cat("==========================\n")

# Overall comparison
comparison_stats <- bind_rows(
  musoma_summary %>% mutate(dataset = "Musoma"),
  other_districts_summary %>% mutate(dataset = "Other Districts")
) %>%
  dplyr::select(dataset, strategy, n_ward_years, n_unique_wards, overall_cost_per_dog, 
                mean_cost_per_dog, median_cost_per_dog, min_cost_per_dog, max_cost_per_dog)

cat("OVERALL COMPARISON:\n")
print(comparison_stats)

# Calculate impact metrics
musoma_max <- max(musoma_ward_year$cost_per_dog, na.rm = TRUE)
musoma_median <- median(musoma_ward_year$cost_per_dog, na.rm = TRUE)
other_max <- max(other_districts_ward_year$cost_per_dog, na.rm = TRUE)
other_median <- median(other_districts_ward_year$cost_per_dog, na.rm = TRUE)

# Outlier impact
if(nrow(musoma_outliers) > 0) {
  outlier_impact <- musoma_outliers %>%
    summarise(
      n_outlier_observations = n(),
      total_outlier_cost = sum(total_cost, na.rm = TRUE),
      total_outlier_dogs = sum(total_dogs, na.rm = TRUE),
      avg_outlier_cost_per_dog = mean(cost_per_dog, na.rm = TRUE)
    )
  
  total_musoma_cost <- sum(musoma_ward_year$total_cost, na.rm = TRUE)
  total_musoma_dogs <- sum(musoma_ward_year$total_dogs, na.rm = TRUE)
  
  cat("\nOUTLIER IMPACT ANALYSIS:\n")
  cat("========================\n")
  cat("Outlier observations:", outlier_impact$n_outlier_observations, "out of", nrow(musoma_ward_year), 
      "(", round((outlier_impact$n_outlier_observations/nrow(musoma_ward_year))*100, 1), "%)\n")
  cat("Outliers represent:", round((outlier_impact$total_outlier_cost/total_musoma_cost)*100, 1), 
      "% of total Musoma costs\n")
  cat("Outliers represent:", round((outlier_impact$total_outlier_dogs/total_musoma_dogs)*100, 1), 
      "% of total Musoma dogs\n")
  cat("Average cost per dog in outliers: $", round(outlier_impact$avg_outlier_cost_per_dog, 2), "\n")
}


# Calculate actual figures for the 79% claim
continuous_with_mwigobero <- musoma_ward_year %>%
  filter(strategy == "continuous") %>%
  summarise(
    mean_cost_per_dog = mean(cost_per_dog, na.rm = TRUE),
    median_cost_per_dog = median(cost_per_dog, na.rm = TRUE)
  )

continuous_without_mwigobero <- musoma_ward_year %>%
  filter(strategy == "continuous", ward != "Mwigobero") %>%
  summarise(
    mean_cost_per_dog = mean(cost_per_dog, na.rm = TRUE),
    median_cost_per_dog = median(cost_per_dog, na.rm = TRUE)
  )

# Calculate the percentage increase
mean_increase <- ((continuous_with_mwigobero$mean_cost_per_dog - continuous_without_mwigobero$mean_cost_per_dog) / continuous_without_mwigobero$mean_cost_per_dog) * 100

median_increase <- ((continuous_with_mwigobero$median_cost_per_dog - continuous_without_mwigobero$median_cost_per_dog) / continuous_without_mwigobero$median_cost_per_dog) * 100




