
rm(list = ls())
library(effects)
library(tidyr)
library(ARTool)
library(clinfun)
library(ARTool)
library(MASS)
library(lme4)
library(car)

# load dataset
monthly_ward_costs <- read.csv("data/monthly_expanded_costs.csv")

#aggregate cost of different components annually
yrly_ward_costs <- monthly_ward_costs %>%
  group_by(district, ward, location, strategy, vaccination_year) %>%
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
    
    zeepot_cost = sum(proport_zeepot_cost, na.rm = TRUE),
    refrigeration_cost = sum(proportional_storage_cost, na.rm = TRUE),
    
    labour_NVX_cost = sum(labour_PR_NVX, na.rm = TRUE),
    perdiem_NVX_cost = sum(perdiem_PR_NVX, na.rm = TRUE),
    #personel_training = sum(training_NV, na.rm = TRUE),
    travel_NVX_cost = sum(Travel_PR_NVX, na.rm = TRUE),
    vehicle_rent_NVL = sum(rental_NVX, na.rm = TRUE),
    fuel_NVX_cost = sum(fuel_NVX, na.rm = TRUE),
    
    main_total = sum(total_cost, na.rm = TRUE),
    cost_per_dog = main_total/total_dogs
  )

yrly_ward_costs <- yrly_ward_costs %>%
  mutate(
    Vacc_delivery = rowSums(cbind(vaccine_bundle, labour_VX_cost, perdiem_VX_cost, travel_VX_cost,
                                  fuel_VX_cost, vehicle_rent_VL), na.rm = TRUE),
    vacc_storage = rowSums(cbind(zeepot_cost, refrigeration_cost), na.rm = TRUE),
    non_vacc_NV = rowSums(cbind(labour_NVX_cost, perdiem_NVX_cost, travel_NVX_cost,
                                vehicle_rent_NVL, fuel_NVX_cost), na.rm = TRUE),
    total_total = rowSums(cbind(Vacc_delivery, vacc_storage, non_vacc_NV, Household_costs), na.rm = TRUE),
    perDog_cost = total_total/total_dogs
  )


# Initial summary of ward-level costs by strategy
ward_summary <- yrly_ward_costs %>%
  group_by(district, ward, location, strategy, vaccination_year) %>%
  summarise(
    total_cost = sum(main_total, na.rm = TRUE),
    vaccine_bundle = sum(vaccine_bundle, na.rm = TRUE),
    fuel_VX_cost = sum(fuel_VX_cost, na.rm = TRUE),
    fuel_NVX_cost = sum(fuel_NVX_cost, na.rm = TRUE),
    refrigeration_cost = sum(vacc_storage, na.rm = TRUE),
    #zeepot_cost = sum(zeepot_cost, na.rm = TRUE),
    vehicle_rent_VL = sum(vehicle_rent_VL, na.rm = TRUE),
    vehicle_rent_NVL = sum(vehicle_rent_NVL, na.rm = TRUE),
    labour_VX_cost = sum(labour_VX_cost, na.rm = TRUE),
    labour_NVX_cost = sum(labour_NVX_cost, na.rm = TRUE),
    travel_VX_cost = sum(travel_VX_cost, na.rm = TRUE),
    travel_NVX_cost = sum(travel_NVX_cost, na.rm = TRUE),
    perdiem_VX_cost = sum(perdiem_VX_cost, na.rm = TRUE),
    perdiem_NVX_cost = sum(perdiem_NVX_cost, na.rm = TRUE),
    HH_trav = sum(HH_trav, na.rm = TRUE),
    HH_wait = sum(HH_wait, na.rm = TRUE),
    HH_costs = sum(Household_costs, na.rm = TRUE),
    delivery_costs = sum(Vacc_delivery, na.rm = TRUE),
    non_delivery_cost = sum(non_vacc_NV, na.rm = TRUE),
    total_dogs = sum(total_dogs, na.rm = TRUE),
    .groups = 'drop'
  )


key_summay <- ward_summary %>%
  group_by(strategy) %>%
  summarise(
    total_total = mean(total_cost),
    total_media = median(total_cost),
    vacc_delivery_costs = mean(delivery_costs),
    vacc_delivery_median = median(delivery_costs),
    household = mean(HH_costs),
    household_median = median(HH_costs),
    non_vacc_costs = mean(non_delivery_cost),
    non_vacc_median = median(non_delivery_cost)
  ); key_summay
# summarise costs components by strategy
strategy_summary <- ward_summary %>%
  group_by(strategy) %>%
  summarise(
    avg_total_cost = mean(total_cost, na.rm = TRUE),
    avg_vaccine_bundle = mean(vaccine_bundle, na.rm = TRUE),
    avg_fuel_VX = mean(fuel_VX_cost, na.rm = TRUE),
    avg_fuel_NVX = mean(fuel_NVX_cost, na.rm = TRUE),
    avg_refrigeration = mean(refrigeration_cost, na.rm = TRUE),
    #avg_zeepot = mean(zeepot_cost, na.rm = TRUE),
    avg_vehicle_rent_VX = mean(vehicle_rent_VL, na.rm = TRUE),
    avg_vehicle_rent_NVX = mean(vehicle_rent_NVL, na.rm = TRUE),
    avg_labour_VX = mean(labour_VX_cost, na.rm = TRUE),
    avg_labour_NVX = mean(labour_NVX_cost, na.rm = TRUE),
    avg_travel_VX = mean(travel_VX_cost, na.rm = TRUE),
    avg_travel_NVX = mean(travel_NVX_cost, na.rm = TRUE),
    avg_perdiem_VX = mean(perdiem_VX_cost, na.rm = TRUE),
    avg_perdiem_NVX = mean(perdiem_NVX_cost, na.rm = TRUE),
    avg_HH_travel = mean(HH_trav, na.rm = TRUE),
    avg_HH_wait = mean(HH_wait, na.rm = TRUE),
    avg_HH_costs = mean(HH_costs, na.rm = TRUE),
    avg_delivery_cost = mean(delivery_costs, na.rm = TRUE),
    avg_non_delivery_cost = mean(non_delivery_cost, na.rm = TRUE),
    avg_dogs_vaccinated = mean(total_dogs, na.rm = TRUE),
    cost_per_dog = mean(total_cost / total_dogs, na.rm = TRUE),
    .groups = "drop"
  )


# Create important aggregate cost categories
ward_summary <- ward_summary %>%
  mutate(
    # Vaccine-related costs
    vaccine_related_costs = vaccine_bundle + refrigeration_cost,
    
    # Travel costs (fuel and vehicle rental)
    travel_costs = fuel_VX_cost + fuel_NVX_cost + vehicle_rent_VL + vehicle_rent_NVL + 
      travel_VX_cost + travel_NVX_cost,
    
    # Personnel costs
    personnel_costs = labour_VX_cost + labour_NVX_cost + perdiem_VX_cost + perdiem_NVX_cost,
    
    # Household costs
    household_costs = HH_costs,
    
    # delivery costs
    #deliveryCosts = delivery_costs,
    
    # Provider costs (all costs except household costs)
    provider_costs = total_cost - household_costs,
    
    # total costs
    total_total = total_cost,
    
    # delivery costs
    vacc_delivery_costs = delivery_costs,
    
    # non vaccination delivery costs
    non_vacc_costs = non_delivery_cost
  )

# 2. Ward-Level Central Tendency and Variability Analysis
# Calculate central tendency and dispersion measures for total cost and components
ward_stats <- ward_summary %>%
  group_by(strategy) %>%
  summarise(
    # Number of wards
    ward_count = n(),
    
    # Total cost statistics
    mean_total_cost = mean(total_cost, na.rm = TRUE),
    median_total_cost = median(total_cost, na.rm = TRUE),
    sd_total_cost = sd(total_cost, na.rm = TRUE),
    cv_total_cost = sd_total_cost / mean_total_cost * 100,
    min_total_cost = min(total_cost, na.rm = TRUE),
    max_total_cost = max(total_cost, na.rm = TRUE),
    iqr_total_cost = IQR(total_cost, na.rm = TRUE),
    
    # Aggregate cost categories statistics
    across(
      c(vaccine_related_costs, travel_costs, personnel_costs, household_costs, provider_costs, vacc_delivery_costs, non_vacc_costs),
      list(
        mean = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        cv = ~sd(., na.rm = TRUE)/mean(., na.rm = TRUE)*100
      ),
      .names = "{.col}_{.fn}"
    ),
    
    # Total program cost by strategy
    total_program_cost = sum(total_cost, na.rm = TRUE),
    
    .groups = "drop"
  )

# Replace t-tests with Wilcoxon tests for all comparisons
wilcox_total <- wilcox.test(total_cost ~ strategy, data = ward_summary)
wilcox_vaccine <- wilcox.test(vaccine_related_costs ~ strategy, data = ward_summary)
wilcox_travel <- wilcox.test(travel_costs ~ strategy, data = ward_summary)
wilcox_personnel <- wilcox.test(personnel_costs ~ strategy, data = ward_summary)
wilcox_household <- wilcox.test(household_costs ~ strategy, data = ward_summary)
wilcox_provider <- wilcox.test(provider_costs ~ strategy, data = ward_summary)
wilcox_delivery <- wilcox.test(vacc_delivery_costs ~ strategy, data = ward_summary)
wilcox_non_delivery <- wilcox.test(non_vacc_costs ~ strategy, data = ward_summary)

# Add equivalent Welch t-tests (unequal variances assumed)
welch_total <- t.test(total_cost ~ strategy, data = ward_summary, var.equal = FALSE)
welch_vaccine <- t.test(vaccine_related_costs ~ strategy, data = ward_summary, var.equal = FALSE)
welch_travel <- t.test(travel_costs ~ strategy, data = ward_summary, var.equal = FALSE)
welch_personnel <- t.test(personnel_costs ~ strategy, data = ward_summary, var.equal = FALSE)
welch_household <- t.test(household_costs ~ strategy, data = ward_summary, var.equal = FALSE)
welch_provider <- t.test(provider_costs ~ strategy, data = ward_summary, var.equal = FALSE)
welch_delivery <- t.test(vacc_delivery_costs ~ strategy, data = ward_summary, var.equal = FALSE)
welch_non_delivery <- t.test(non_vacc_costs ~ strategy, data = ward_summary, var.equal = FALSE)

# Summarize test results
wilcox_results <- data.frame(
  cost_component = c("Total", "Vaccine-related", "Travel", "Personnel", "Household", "Provider", "delivery", "non_delivery"),
  statistic = c(wilcox_total$statistic, wilcox_vaccine$statistic, wilcox_travel$statistic, 
                wilcox_personnel$statistic, wilcox_household$statistic, wilcox_provider$statistic,
                wilcox_delivery$statistic, wilcox_non_delivery$statistic),
  p_value = c(wilcox_total$p.value, wilcox_vaccine$p.value, wilcox_travel$p.value, 
              wilcox_personnel$p.value, wilcox_household$p.value, wilcox_provider$p.value,
              wilcox_delivery$p.value, wilcox_non_delivery$p.value),
  significance = c(wilcox_total$p.value < 0.05, wilcox_vaccine$p.value < 0.05, 
                   wilcox_travel$p.value < 0.05, wilcox_personnel$p.value < 0.05,
                   wilcox_household$p.value < 0.05, wilcox_provider$p.value < 0.05,
                   wilcox_delivery$p.value < 0.05, wilcox_non_delivery$p.value < 0.05)
)

# Consider log-transforming cost data for regression analyses
ward_summary_log <- ward_summary %>%
  mutate(across(c(total_cost, vaccine_related_costs, travel_costs, personnel_costs, 
                  household_costs, provider_costs),
                ~log(. + 1),  # Add 1 to handle potential zeros
                .names = "log_{.col}"))

# Run regression models on log-transformed data
log_model1 <- lm(log_total_cost ~ strategy, data = ward_summary_log)
log_model2 <- lm(log_total_cost ~ strategy + location, data = ward_summary_log)
log_model3 <- lm(log_total_cost ~ strategy * location, data = ward_summary_log)


# Enhanced visualizations for non-normal data
ward_summary %>%
  mutate(strategy = fct_recode(strategy,
                               "Community led" = "continuous",
                               "Team led" = "pulsed")) %>%
  ggplot(aes(x = strategy, y = total_cost, fill = strategy)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2, alpha = 0.7) +
  scale_y_continuous(trans = "log10") +  # Log scale for better visualization
  scale_fill_discrete(name = "Trial arm") +  # Change legend title here
  labs(title = "Total Vaccination Costs by Strategy (Log Scale)",
       x = "Trial Arm", y = "Total Cost (log scale)") +
  theme_minimal()


# 3. Cost Component Analysis
# Calculate proportional contribution of each cost component to total cost
ward_summary <- ward_summary %>%
  mutate(
    # Basic cost components as percentage of total
    vaccine_bundle_pct = 100 * vaccine_bundle / total_cost,
    refrigeration_cost_pct = 100 * refrigeration_cost / total_cost,
    #zeepot_cost_pct = 100 * zeepot_cost / total_cost,
    fuel_VX_cost_pct = 100 * fuel_VX_cost / total_cost,
    fuel_NVX_cost_pct = 100 * fuel_NVX_cost / total_cost,
    vehicle_rent_VL_pct = 100 * vehicle_rent_VL / total_cost,
    vehicle_rent_NVL_pct = 100 * vehicle_rent_NVL / total_cost,
    labour_VX_cost_pct = 100 * labour_VX_cost / total_cost,
    labour_NVX_cost_pct = 100 * labour_NVX_cost / total_cost,
    travel_VX_cost_pct = 100 * travel_VX_cost / total_cost,
    travel_NVX_cost_pct = 100 * travel_NVX_cost / total_cost,
    perdiem_VX_cost_pct = 100 * perdiem_VX_cost / total_cost,
    perdiem_NVX_cost_pct = 100 * perdiem_NVX_cost / total_cost,
    HH_trav_pct = 100 * HH_trav / total_cost,
    HH_wait_pct = 100 * HH_wait / total_cost,
    
    # Aggregate cost categories as percentage of total
    vaccine_related_pct = 100 * vaccine_related_costs / total_cost,
    travel_pct = 100 * travel_costs / total_cost,
    personnel_pct = 100 * personnel_costs / total_cost,
    household_pct = 100 * household_costs / total_cost,
    provider_pct = 100 * provider_costs / total_cost
  )

# For component analysis, use proportions which may be more normally distributed
# But still check their distributions
shapiro.test(ward_summary$vaccine_related_pct)
shapiro.test(ward_summary$travel_pct)
shapiro.test(ward_summary$personnel_pct)
shapiro.test(ward_summary$household_pct)

# Compare cost component percentages between strategies
component_pct_comparison <- ward_summary %>%
  group_by(strategy) %>%
  summarise(
    across(ends_with("_pct"),
           list(mean = mean, median = median, sd = sd),
           na.rm = TRUE)
  )

# For comparing proportional components between strategies
wilcox_vaccine_pct <- wilcox.test(vaccine_related_pct ~ strategy, data = ward_summary)
wilcox_travel_pct <- wilcox.test(travel_pct ~ strategy, data = ward_summary)
wilcox_personnel_pct <- wilcox.test(personnel_pct ~ strategy, data = ward_summary)
wilcox_household_pct <- wilcox.test(household_pct ~ strategy, data = ward_summary)
provider_pct_test <- wilcox.test(provider_pct ~ strategy, data = ward_summary)

t_vaccine_pct <- t.test(vaccine_related_pct ~ strategy, data = ward_summary)
t_travel_pct <- t.test(travel_pct ~ strategy, data = ward_summary)
t_personnel_pct <- t.test(personnel_pct ~ strategy, data = ward_summary)
t_household_pct <- t.test(household_pct ~ strategy, data = ward_summary)
t_provider_pct <- t.test(provider_pct ~ strategy, data = ward_summary)

# 4. Location-Stratified Analysis
# Analysis by location and strategy
location_strategy_summary <- ward_summary %>%
  group_by(ward, location, strategy) %>%
  summarise(
    ward_count = n(),
    mean_total = mean(total_cost, na.rm = TRUE),
    median_total = median(total_cost, na.rm = TRUE),
    
    # Main cost categories
    mean_vaccine = mean(vaccine_related_costs, na.rm = TRUE),
    mean_travel = mean(travel_costs, na.rm = TRUE),
    mean_personnel = mean(personnel_costs, na.rm = TRUE),
    mean_household = mean(household_costs, na.rm = TRUE),
    mean_provider = mean(provider_costs, na.rm = TRUE),
    
    # Percentage breakdowns
    vaccine_pct = mean(vaccine_related_pct, na.rm = TRUE),
    travel_pct = mean(travel_pct, na.rm = TRUE),
    personnel_pct = mean(personnel_pct, na.rm = TRUE),
    household_pct = mean(household_pct, na.rm = TRUE),
    provider_pct = mean(provider_pct, na.rm = TRUE),
    .groups = 'drop'
  )

location_strategy_summary %>%
  group_by(location, strategy) %>%
  summarise(
    median_av = median(median_total),
    mean_total = mean(mean_total)
  )
# Compare cost component by location percentages between strategies
component_pct_location <- location_strategy_summary %>%
  group_by(strategy, location) %>%
  summarise(
    across(ends_with("_pct"),
           list(mean = mean, median = median, sd = sd),
           na.rm = TRUE)
  )

# Tests by location using the correct case
urban_test <- wilcox.test(total_cost ~ strategy, data = filter(ward_summary, location == "urban"))
rural_test <- wilcox.test(total_cost ~ strategy, data = filter(ward_summary, location == "rural"))

# Since the data is not normally distributed, we're using Wilcoxon tests instead of t-tests
print("Urban locations - strategy comparison (Wilcoxon test):")
print(urban_test)
print("Rural locations - strategy comparison (Wilcoxon test):")
print(rural_test)

# Non-parametric Aligned Rank Transform ANOVA
# Convert character variables to factors
ward_summary <- ward_summary %>%
  mutate(
    location = factor(location),
    strategy = factor(strategy)
  )
art_model1 <- art(total_cost ~ location * strategy, data = ward_summary)
anova(art_model1)

# 5. Cost Drivers Analysis
# Model 1: Cost component drivers (aggregated level)
component_model <- lm(total_cost ~ vaccine_related_costs + travel_costs + 
                        personnel_costs + household_costs, data = ward_summary)
summary(component_model)

# Model 2: Stepwise regression to identify key detailed drivers
full_model <- lm(total_cost ~ vaccine_bundle + refrigeration_cost + 
                   fuel_VX_cost + fuel_NVX_cost + vehicle_rent_VL + vehicle_rent_NVL + 
                   labour_VX_cost + labour_NVX_cost + travel_VX_cost + travel_NVX_cost + 
                   perdiem_VX_cost + perdiem_NVX_cost + HH_trav + HH_wait + 
                   strategy + location, data = ward_summary)

step_model <- stepAIC(full_model, direction = "both", trace = FALSE)
summary(step_model)

# Identify top cost drivers
top_coeffs <- coef(summary(step_model))
top_drivers <- rownames(top_coeffs)[order(abs(top_coeffs[,1]), decreasing = TRUE)][2:6]
print(paste("Top cost drivers:", paste(top_drivers, collapse = ", ")))


# 6. Cost Elasticity Analysis
# Calculate elasticity using log-log regression for major cost components
cost_components <- c("vaccine_related_costs", "travel_costs", "personnel_costs", "household_costs")
component_labels <- c("Vaccine", "Travel", "Personnel", "Household")

# Overall elasticity analysis
elasticity_summary <- map_dfr(1:length(cost_components), function(i) {
  # Handle potential zeros in household costs
  x_var <- if(cost_components[i] == "household_costs") {
    log(ward_summary[[cost_components[i]]] + 1)
  } else {
    log(ward_summary[[cost_components[i]]])
  }
  
  model <- lm(log(total_cost) ~ x_var, data = ward_summary)
  
  data.frame(
    component = component_labels[i],
    elasticity = coef(model)[2],
    r_squared = summary(model)$r.squared
  )
})

# Elasticity by strategy (streamlined)
elasticity_by_strategy <- ward_summary %>%
  group_by(strategy) %>%
  group_modify(~{
    map_dfr(1:length(cost_components), function(i) {
      x_var <- if(cost_components[i] == "household_costs") {
        log(.x[[cost_components[i]]] + 1)
      } else {
        log(.x[[cost_components[i]]])
      }
      
      model <- lm(log(total_cost) ~ x_var, data = .x)
      
      data.frame(
        component = component_labels[i],
        elasticity = coef(model)[2],
        r_squared = summary(model)$r.squared
      )
    })
  })

##########################################################################################
# DISTRICT LEVEL ANALYSIS
# 1. Basic district-level summary statistics
district_summary <- ward_summary %>%
  group_by(district, strategy) %>%
  summarise(
    ward_count = n(),
    mean_total_cost = mean(total_cost, na.rm = TRUE),
    median_total_cost = median(total_cost, na.rm = TRUE),
    iqr_total_cost = IQR(total_cost, na.rm = TRUE),
    median_vaccine_pct = median(vaccine_related_pct, na.rm = TRUE),
    median_personnel_pct = median(personnel_pct, na.rm = TRUE),
    median_household_pct = median(household_pct, na.rm = TRUE),
    .groups = 'drop'
  )

# 2. Test for strategy effects within each district (both Wilcoxon and Welch tests)
district_tests <- ward_summary %>%
  group_by(district) %>%
  summarise(
    # Wilcoxon rank-sum test
    wilcox_p_value = wilcox.test(total_cost ~ strategy)$p.value,
    wilcox_significant = wilcox_p_value < 0.05,
    
    # Welch t-test
    welch_p_value = t.test(total_cost ~ strategy, var.equal = FALSE)$p.value,
    welch_significant = welch_p_value < 0.05,
    
    # Agreement between tests
    tests_agree = wilcox_significant == welch_significant,
    
    .groups = 'drop'
  )

# 3. Calculate effect sizes (cost ratios) by district
district_effect_sizes <- ward_summary %>%
  group_by(district, strategy) %>%
  summarise(
    median_cost = median(total_cost, na.rm = TRUE),
    mean_cost = mean(total_cost, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = strategy,
    values_from = c(median_cost, mean_cost)
  ) %>%
  mutate(
    # Median-based ratios (for Wilcoxon interpretation)
    median_cost_ratio = median_cost_continuous / median_cost_pulsed,
    median_percent_difference = (median_cost_continuous - median_cost_pulsed) / median_cost_pulsed * 100,
    
    # Mean-based ratios (for Welch t-test interpretation)
    mean_cost_ratio = mean_cost_continuous / mean_cost_pulsed,
    mean_percent_difference = (mean_cost_continuous - mean_cost_pulsed) / mean_cost_pulsed * 100
  )

#############################################################
# ANALYSIS BY THE DELIVERY APPROACH
# First, create the simplified delivery strategy categories
delivery_categorized <- monthly_ward_costs %>%
  mutate(
    delivery_simplified = case_when(
      Delivery.strategy.type %in% c("House to House", "On Demand", "Visit for Other Reasons") ~ "Household Level",
      Delivery.strategy.type == "Central Point - Subvillage" ~ "Central Point - Subvillage",
      Delivery.strategy.type == "Central Point - Village" ~ "Central Point - Village",
      TRUE ~ Delivery.strategy.type  # Keep original for any other types
    )
  )

# Next, aggregate costs to the ward level by delivery strategy
delivery_ward_totals <- delivery_categorized %>%
  group_by(district, ward, vaccination_year, strategy, delivery_simplified) %>%
  summarise(
    total_cost = sum(total_cost, na.rm = TRUE),
    vaccine_related_costs = sum(vaccination_cost + proportional_storage_cost + proport_zeepot_cost, na.rm = TRUE),
    travel_costs = sum(fuel_VX + fuel_NVX + rental_cost + rental_NVX + Travel_PR_VX + Travel_PR_NVX, na.rm = TRUE),
    personnel_costs = sum(labour_PR_VX + labour_PR_NVX + perdiem_PR_VX + perdiem_PR_NVX, na.rm = TRUE),
    household_costs = sum(HH_cost, na.rm = TRUE),
    provider_costs = total_cost - household_costs,
    .groups = "drop"
  ) %>%
  mutate(
    vaccine_related_pct = 100 * vaccine_related_costs / total_cost,
    travel_pct = 100 * travel_costs / total_cost,
    personnel_pct = 100 * personnel_costs / total_cost,
    household_pct = 100 * household_costs / total_cost,
    provider_pct = 100 * provider_costs / total_cost
  )

# Now analyze by delivery strategy and trial arm
delivery_cost_summary <- delivery_ward_totals %>%
  group_by(strategy, delivery_simplified) %>%
  summarise(
    ward_count = n(),
    mean_total_cost = mean(total_cost, na.rm = TRUE),
    median_total_cost = median(total_cost, na.rm = TRUE),
    iqr_total_cost = IQR(total_cost, na.rm = TRUE),
    
    # Main cost categories
    median_vaccine_cost = median(vaccine_related_costs, na.rm = TRUE),
    median_personnel_cost = median(personnel_costs, na.rm = TRUE),
    median_travel_cost = median(travel_costs, na.rm = TRUE),
    median_household_cost = median(household_costs, na.rm = TRUE),
    
    # Proportional costs
    median_vaccine_pct = median(vaccine_related_pct, na.rm = TRUE),
    median_personnel_pct = median(personnel_pct, na.rm = TRUE),
    median_travel_pct = median(travel_pct, na.rm = TRUE),
    median_household_pct = median(household_pct, na.rm = TRUE),
    .groups = 'drop'
  )


# First, identify which delivery types have both strategies
testable_delivery <- delivery_ward_totals %>%
  group_by(delivery_simplified) %>%
  summarise(n_strategies = n_distinct(strategy)) %>%
  filter(n_strategies == 2) %>%
  pull(delivery_simplified)

# Then only test those
delivery_strategy_tests <- delivery_ward_totals %>%
  filter(delivery_simplified %in% testable_delivery) %>%
  group_by(delivery_simplified) %>%
  summarise(
    # Wilcoxon rank-sum test
    wilcox_p_value = wilcox.test(total_cost ~ strategy)$p.value,
    wilcox_significant = wilcox_p_value < 0.05,
    
    # Welch t-test
    welch_p_value = t.test(total_cost ~ strategy, var.equal = FALSE)$p.value,
    welch_significant = welch_p_value < 0.05,
    
    # Agreement between tests
    tests_agree = wilcox_significant == welch_significant,
    .groups = 'drop'
  )

# Ensure no grouping is carried over and factors are clean
delivery_ward_totals <- delivery_ward_totals %>%
  ungroup() %>%
  mutate(
    strategy = factor(strategy, levels = c("continuous", "pulsed")),
    delivery_simplified = factor(delivery_simplified, levels = c("Household Level", "Central Point - Subvillage", "Central Point - Village"))
  )
# Option: Filter to only delivery types that exist in both strategies
comparable_delivery <- delivery_ward_totals %>%
  group_by(delivery_simplified) %>%
  filter(n_distinct(strategy) == 2) %>%
  ungroup() %>%
  # Re-factor to drop unused levels after filtering
  mutate(
    delivery_simplified = droplevels(delivery_simplified)
  )
art_model_simple <- art(total_cost ~ strategy, data = comparable_delivery)
anova(art_model_simple)


# Enhanced effect sizes by delivery method (with both median and mean)
effect_by_delivery <- delivery_ward_totals %>%
  group_by(delivery_simplified, strategy) %>%
  summarise(
    median_cost = median(total_cost, na.rm = TRUE),
    mean_cost = mean(total_cost, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = strategy,
    values_from = c(median_cost, mean_cost)
  ) %>%
  mutate(
    # Median-based ratios (for Wilcoxon interpretation)
    median_cost_ratio = median_cost_continuous / median_cost_pulsed,
    median_percent_difference = (median_cost_continuous - median_cost_pulsed) / median_cost_pulsed * 100,
    
    # Mean-based ratios (for Welch t-test interpretation)
    mean_cost_ratio = mean_cost_continuous / mean_cost_pulsed,
    mean_percent_difference = (mean_cost_continuous - mean_cost_pulsed) / mean_cost_pulsed * 100
  )

###################################################################

# ANALYSIS OF COSTS OVER TIME


