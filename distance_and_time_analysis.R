

# analysis originating from "MDV_cost" summary script
# Load summarised dog vaccination, distancenand time data set
rm(list = ls())
# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
#vaccination_DF <- read.csv("output/vaccination_DF_summary.csv")
vaccination_DF <- read.csv("output/Per_unit_expandedMonthlyCost.csv")
JLFO_resource <- read.csv("output/JLFO_distance_time.csv")
RC_SLFO_resource <- read.csv("output/vaccinator_distance_time.csv")
#primary_vacc_data <- read.csv("output/primary_dog_data.csv")
yr_ward_unitCost <- read.csv("output/annual_ward_per_unit_costs.csv")

################################################################################
# calculate number of months in vaccination
# ward_vacc_months <- primary_vacc_data %>%
#   group_by(district, ward, vaccination_year, strategy) %>%
#   summarize(
#     months_used = n_distinct(month_name),
#     .groups = "drop"
#   )
# # AVERAGE NUMBER OF MONTHS IN ACTION
# ward_vacc_months %>%
#   group_by(strategy) %>%
#   summarise(
#     mean_months = mean(months_used, na.rm = TRUE),
#     median_months = median(months_used, na.rm = TRUE),
#     sd_months = sd(months_used),
#     ci_low_month = t.test(months_used)$conf.int[1],
#     ci_up_month = t.test(months_used)$conf.int[2])

# calculate annual ward vaccination days 
VaccDays <- vaccination_DF %>%
  group_by(district, ward, vaccination_year, strategy) %>%
  summarise(vaccinations_days = sum(all_vacc_days, na.rm = TRUE))

# annual vaccination DAYS per ward summary
VaccDays %>%
  group_by(strategy) %>%
  summarise(
    mean_days = mean(vaccinations_days, na.rm = TRUE),
    median_days = median(vaccinations_days, na.rm = TRUE),
    sd_days = sd(vaccinations_days),
    ci_low_days = t.test(vaccinations_days)$conf.int[1],
    ci_up_days = t.test(vaccinations_days)$conf.int[2])
  
#################################################################################

# TIME
# travel and vaccination hours per day
# Custom function to calculate confidence interval
calc_ci <- function(x) {
  t_test <- t.test(x, na.rm = TRUE)
  c(ci_lower = t_test$conf.int[1], ci_upper = t_test$conf.int[2])
}
# suma travel time for JLFO and SLFO
vaccination_DF <- vaccination_DF %>%
  mutate(travTime_row = coalesce(RC_SLFO_travTime, 0) + coalesce(JLFO_Trav_time, 0))

# summarise the sum of ward time and distance and calculate time and distance per day by arm of trial 
TimeDist_summary <- vaccination_DF %>%
  group_by(district, ward, strategy, vaccination_year) %>%
  summarise(
    DaysVacc = sum(all_vacc_days, na.rm = TRUE),
    totalDist = sum(trav_vacc_dist_PulseConti, na.rm = TRUE),
    travDist = sum(trav_dist_PulseConti, na.rm = TRUE),
    totalTime = sum(trav_vacc_time_PulseConti, na.rm = TRUE),
    travTime = sum(travTime_row, na.rm = TRUE),
    DayTotalDist = totalDist / DaysVacc,
    daytravDist = travDist / DaysVacc,
    DaytotalTime = totalTime / DaysVacc,
    DayTravTime = travTime / DaysVacc,
    TravTimePct = (DayTravTime / DaytotalTime) * 100,
    .groups = "drop"
  )

TimeDist_summary %>%
  group_by(strategy) %>%
  summarise(
    avg_time = mean(DaytotalTime),
    sd_time = sd(DaytotalTime),
    ci_low_time = t.test(DaytotalTime)$conf.int[1],
    ci_up_time = t.test(DaytotalTime)$conf.int[2],
    avg_trav = mean(DayTravTime),
    sd_trav = sd(DayTravTime),
    ci_low_trav = t.test(DayTravTime)$conf.int[1],
    ci_up_trav = t.test(DayTravTime)$conf.int[2],
    # Percentage
    avg_TravTimePct = (avg_trav / avg_time) * 100,
  )

# Perform Welch Two Sample t-test for vaccination time per year per ward 
TimePerDay <- t.test(DaytotalTime ~ strategy, data = TimeDist_summary, var.equal = FALSE)
print(TimePerDay)

# Recode strategy types
vaccination_recode <- vaccination_DF %>%
  mutate(
    Delivery.strategy.type = str_trim(Delivery.strategy.type),
    Delivery.strategy.type = case_when(
      Delivery.strategy.type %in% c("Visit for Other Reasons", "On Demand") ~ "House to House",
      TRUE ~ Delivery.strategy.type
    )
  )

# Calculate the average time allocated to delivery approach
annual_sum <- vaccination_recode %>%
  filter(strategy == "continuous") %>%
  group_by(district, ward, Delivery.strategy.type, vaccination_year) %>%
  summarise(
    yearly_total_time = sum(RC_SLFO_TravVacc_time, na.rm = TRUE),
    .groups = "drop"
  )

summary_test <- annual_sum %>%
  group_by(Delivery.strategy.type) %>%
  summarise(
    avg_total_time = mean(yearly_total_time, na.rm = TRUE),
    se_total_time  = sd(yearly_total_time, na.rm = TRUE) / sqrt(n()),
    t_val          = qt(0.975, df = n() - 1),
    lower_CI       = avg_total_time - t_val * se_total_time,
    upper_CI       = avg_total_time + t_val * se_total_time,
    .groups = "drop"
  ) %>%
  mutate(
    percent_contribution = (avg_total_time / sum(avg_total_time)) * 100
  )

################################################################################

# DISTANCE
distanc_summary <- TimeDist_summary %>%
  group_by(strategy) %>%
  summarise(
    avg_ann_distance = mean(totalDist),
    ci_low_dist = t.test(totalDist)$conf.int[1],
    ci_up_dist = t.test(totalDist)$conf.int[2],
    avg_ann_trav = mean(travDist),
    ci_low_trav = t.test(travDist)$conf.int[1],
    ci_up_trav = t.test(travDist)$conf.int[2],
    avg_day_distance = mean(DayTotalDist),
    sd_day_dista = sd(DayTotalDist),
    ci_low_day_dist = t.test(DayTotalDist)$conf.int[1],
    ci_up_day_dist = t.test(DayTotalDist)$conf.int[2],
    avg_day_trav_dist = mean(daytravDist),
    sd_day_trav_dist = sd(daytravDist),
    ci_low_day_trav_dist = t.test(daytravDist)$conf.int[1],
    ci_up_day_trav_dist = t.test(daytravDist)$conf.int[2],
  )
# calculate the percent of travel distance to total distance
distanc_summary %>%
  group_by(strategy) %>%
  summarise(
    trav_percent = (avg_ann_trav / avg_ann_distance) * 100
  )
# Perform Welch Two Sample t-test for variation distance per year per ward 
Dist_PerDay <- t.test(DayTotalDist ~ strategy, data = TimeDist_summary, var.equal = FALSE)
print(Dist_PerDay)
##############################################################################

# ANALYZE TIME AND DISTANCE METRICS BY VACCINATION STRATEGY

# 1. Ward-level analysis of time and distance metrics with safeguards
time_distance_ward <- yr_ward_unitCost %>%
  group_by(district, ward, strategy, vaccination_year) %>%
  summarize(
    total_dogs = sum(vaccinated_dogs, na.rm = TRUE),
    
    # Time metrics 
    vaccination_time = sum(trav_vacc_time_PulseConti, na.rm = TRUE),
    # convert hours into minutes for per dog metric
    vaccination_time_per_dog = ifelse(total_dogs > 0, 
                                      (vaccination_time / total_dogs) * 60, 
                                      NA_real_),
    
    household_time = sum(HH_time, na.rm = TRUE),
    # convert hours into minutes for per dog metric
    household_time_per_dog = ifelse(total_dogs > 0, 
                                    (household_time / total_dogs) * 60, 
                                    NA_real_),
    
    total_time = vaccination_time + household_time,
    # convert hours into minutes for per dog metric
    total_time_per_dog = ifelse(total_dogs > 0, 
                                (total_time / total_dogs) * 60, 
                                NA_real_),
    
    # Distance metrics
    travel_distance = sum(trav_dist_PulseConti, na.rm = TRUE),
    travel_distance_per_dog = ifelse(total_dogs > 0, 
                                     travel_distance / total_dogs, 
                                     NA_real_),
    
    vaccination_distance = sum(trav_vacc_dist_PulseConti, na.rm = TRUE),
    vaccination_distance_per_dog = ifelse(total_dogs > 0, 
                                          vaccination_distance / total_dogs, 
                                          NA_real_),
    
    # Efficiency metrics with safeguards
    total_days = sum(all_vacc_days, na.rm = TRUE),
    dogs_per_day = ifelse(total_days > 0, 
                          total_dogs / total_days, 
                          NA_real_),
    
    dogs_per_hour = ifelse(vaccination_time > 0, 
                           (total_dogs / (vaccination_time)), 
                           NA_real_),
    
    dogs_per_km = ifelse(vaccination_distance > 0, 
                         total_dogs / vaccination_distance, 
                         NA_real_),
    .groups = "drop"
  ) %>%
  # Remove any rows with infinite or NaN values
  filter_all(all_vars(is.na(.) | (!is.infinite(.) & !is.nan(.))))

# 2. Check data validity before analysis
variable_summary <- lapply(c("vaccination_time_per_dog", "household_time_per_dog", 
                             "total_time_per_dog", "travel_distance_per_dog",
                             "vaccination_distance_per_dog", "dogs_per_day", 
                             "dogs_per_hour", "dogs_per_km"), function(var) {
                               data.frame(
                                 metric = var,
                                 n_values = sum(!is.na(time_distance_ward[[var]])),
                                 n_unique = length(unique(time_distance_ward[[var]][!is.na(time_distance_ward[[var]])])),
                                 min_value = min(time_distance_ward[[var]], na.rm = TRUE),
                                 max_value = max(time_distance_ward[[var]], na.rm = TRUE),
                                 sd_value = sd(time_distance_ward[[var]], na.rm = TRUE)
                               )
                             })
variable_summary_table <- do.call(rbind, variable_summary)
print(variable_summary_table)

# 3. Strategy comparisons with robust statistical tests
time_comparisons <- lapply(c("vaccination_time_per_dog", "household_time_per_dog", 
                             "total_time_per_dog", "travel_distance_per_dog",
                             "vaccination_distance_per_dog", "dogs_per_day", 
                             "dogs_per_hour", "dogs_per_km"), function(var) {
                               
                               tryCatch({
                                 # Try t-test first
                                 formula <- as.formula(paste(var, "~ strategy"))
                                 test <- t.test(formula, data = time_distance_ward, var.equal = FALSE)
                                 
                                 # Calculate standard deviations
                                 community_sd <- sd(time_distance_ward[[var]][time_distance_ward$strategy == "continuous"], 
                                                    na.rm = TRUE)
                                 team_sd <- sd(time_distance_ward[[var]][time_distance_ward$strategy == "pulsed"], 
                                               na.rm = TRUE)
                                 
                                 data.frame(
                                   metric = var,
                                   test_type = "t-test",
                                   statistic = test$statistic,
                                   df = test$parameter,
                                   p_value = test$p.value,
                                   community_mean = test$estimate[1],
                                   community_sd = community_sd,
                                   team_mean = test$estimate[2],
                                   team_sd = team_sd,
                                   difference = test$estimate[1] - test$estimate[2],
                                   pct_diff = (test$estimate[1] / test$estimate[2] - 1) * 100
                                 )
                               }, error = function(e) {
                                 # If t-test fails, try non-parametric test
                                 wilcox_test <- wilcox.test(time_distance_ward[[var]] ~ time_distance_ward$strategy, 
                                                            na.rm = TRUE)
                                 
                                 # Calculate group means and standard deviations manually
                                 community_mean <- mean(time_distance_ward[[var]][time_distance_ward$strategy == "continuous"], 
                                                        na.rm = TRUE)
                                 team_mean <- mean(time_distance_ward[[var]][time_distance_ward$strategy == "pulsed"], 
                                                   na.rm = TRUE)
                                 
                                 community_sd <- sd(time_distance_ward[[var]][time_distance_ward$strategy == "continuous"], 
                                                    na.rm = TRUE)
                                 team_sd <- sd(time_distance_ward[[var]][time_distance_ward$strategy == "pulsed"], 
                                               na.rm = TRUE)
                                 
                                 data.frame(
                                   metric = var,
                                   test_type = "wilcoxon",
                                   statistic = wilcox_test$statistic,
                                   df = NA,
                                   p_value = wilcox_test$p.value,
                                   community_mean = community_mean,
                                   community_sd = community_sd,
                                   team_mean = team_mean,
                                   team_sd = team_sd,
                                   difference = community_mean - team_mean,
                                   pct_diff = (community_mean / team_mean - 1) * 100
                                 )
                               })
                             })
time_comparison_results <- do.call(rbind, time_comparisons)
print(time_comparison_results)

# 4. Rural-urban analysis of time and distance
time_distance_ward <- time_distance_ward %>%
  mutate(location = ifelse(district %in% c("Bunda Town Council", "Musoma Municipal Council", 
                                           "Tarime Town Council"), "urban", "rural"))

rural_urban_time <- time_distance_ward %>%
  group_by(location, strategy) %>%
  summarize(
    n_wards = n(),
    mean_vaccination_time = mean(vaccination_time_per_dog, na.rm = TRUE),
    median_vaccination_time = median(vaccination_time_per_dog, na.rm = TRUE),
    mean_household_time = mean(household_time_per_dog, na.rm = TRUE),
    median_household_time = median(household_time_per_dog, na.rm = TRUE),
    mean_total_time = mean(total_time_per_dog, na.rm = TRUE),
    median_total_time = median(total_time_per_dog, na.rm = TRUE),
    mean_travel_distance = mean(travel_distance_per_dog, na.rm = TRUE),
    median_travel_distance = median(travel_distance_per_dog, na.rm = TRUE),
    mean_vaccination_distance = mean(vaccination_distance_per_dog, na.rm = TRUE),
    median_vaccination_distance = median(vaccination_distance_per_dog, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(strategy = factor(strategy, 
                           levels = c("continuous", "pulsed"),
                           labels = c("Community-led", "Team-led")))

print(rural_urban_time)

# 5. Statistical tests for rural-urban differences
# For key metrics, test rural vs urban within each strategy
for(strat in c("continuous", "pulsed")) {
  cat("\n\nStrategy:", strat, "\n")
  
  for(var in c("vaccination_time_per_dog", "household_time_per_dog", "total_time_per_dog",
               "travel_distance_per_dog", "vaccination_distance_per_dog")) {
    
    # Subset data for this strategy
    strat_data <- time_distance_ward %>% filter(strategy == strat)
    
    # Try t-test
    tryCatch({
      test_result <- t.test(strat_data[[var]] ~ strat_data$location, var.equal = FALSE)
      cat(var, "- t-test: t =", round(test_result$statistic, 3), 
          ", df =", round(test_result$parameter, 1), 
          ", p =", round(test_result$p.value, 3), "\n")
    }, error = function(e) {
      # If t-test fails, try wilcoxon
      wilcox_result <- wilcox.test(strat_data[[var]] ~ strat_data$location)
      cat(var, "- Wilcoxon: W =", wilcox_result$statistic, 
          ", p =", round(wilcox_result$p.value, 3), "\n")
    })
  }
}

# 6. District-level analysis
district_time_distance <- time_distance_ward %>%
  group_by(district, strategy) %>%
  summarize(
    n_wards = n(),
    mean_vaccination_time = mean(vaccination_time_per_dog, na.rm = TRUE),
    mean_household_time = mean(household_time_per_dog, na.rm = TRUE),
    mean_total_time = mean(total_time_per_dog, na.rm = TRUE),
    mean_travel_distance = mean(travel_distance_per_dog, na.rm = TRUE),
    mean_vaccination_distance = mean(vaccination_distance_per_dog, na.rm = TRUE),
    mean_dogs_per_day = mean(dogs_per_day, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(strategy = factor(strategy, 
                           levels = c("continuous", "pulsed"),
                           labels = c("Community-led", "Team-led")))

# 7. Calculate Z-scores to identify outlier districts
# Calculate averages by strategy
strategy_time_avgs <- district_time_distance %>%
  group_by(strategy) %>%
  summarize(across(
    starts_with("mean_"),
    list(avg = mean, sd = sd),
    na.rm = TRUE
  ))

# Join strategy averages to district data
district_time_distance <- district_time_distance %>%
  left_join(strategy_time_avgs, by = "strategy") %>%
  mutate(
    # Calculate z-scores for time metrics
    z_vaccination_time = (mean_vaccination_time - mean_vaccination_time_avg) / mean_vaccination_time_sd,
    z_household_time = (mean_household_time - mean_household_time_avg) / mean_household_time_sd,
    z_total_time = (mean_total_time - mean_total_time_avg) / mean_total_time_sd,
    
    # Calculate z-scores for distance metrics
    z_travel_distance = (mean_travel_distance - mean_travel_distance_avg) / mean_travel_distance_sd,
    z_vaccination_distance = (mean_vaccination_distance - mean_vaccination_distance_avg) / mean_vaccination_distance_sd,
    
    # Efficiency z-score
    z_dogs_per_day = (mean_dogs_per_day - mean_dogs_per_day_avg) / mean_dogs_per_day_sd
  )

# 8. Identify high time/distance districts (z > 1)
high_time_districts <- district_time_distance %>%
  filter(z_total_time > 1) %>%
  arrange(strategy, desc(z_total_time))

high_distance_districts <- district_time_distance %>%
  filter(z_vaccination_distance > 1) %>%
  arrange(strategy, desc(z_vaccination_distance))

# 9. Create visualization plots

# Create abbreviated district names for better visualization
district_time_distance <- district_time_distance %>%
  mutate(district_short = case_when(
    district == "Bunda District Council" ~ "Bunda DC",
    district == "Bunda Town Council" ~ "Bunda TC",
    district == "Tarime Town Council" ~ "Tarime TC",
    district == "Tarime District Council" ~ "Tarime DC",
    district == "Musoma Municipal Council" ~ "Musoma MC",
    district == "Musoma District Council" ~ "Musoma DC",
    TRUE ~ district
  ))

# 9.1 Bar plot of time per dog by district and strategy
time_per_dog_plot <- ggplot(district_time_distance, 
                            aes(x = reorder(district_short, mean_total_time), 
                                y = mean_total_time, fill = strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Time Required per Dog by District and Strategy",
       x = "District", 
       y = "Average Time per Dog (minutes)",
       fill = "Strategy") +
  theme_minimal()

print(time_per_dog_plot)
ggsave("figure/time_per_dog.png", time_per_dog_plot, width = 10, height = 8)

# 9.2 Bar plot of distance per dog by district and strategy
distance_per_dog_plot <- ggplot(district_time_distance, 
                                aes(x = reorder(district_short, mean_vaccination_distance), 
                                    y = mean_vaccination_distance, fill = strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Distance Traveled per Dog by District and Strategy",
       x = "District", 
       y = "Average Distance per Dog (km)",
       fill = "Strategy") +
  theme_minimal()

print(distance_per_dog_plot)
ggsave("figure/distance_per_dog.png", distance_per_dog_plot, width = 10, height = 8)

# 9.4 Rural vs Urban comparison
rural_urban_plot <- ggplot(rural_urban_time, 
                           aes(x = location, y = mean_total_time, fill = strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Time per Dog in Rural vs Urban Areas",
       x = "Location", 
       y = "Average Time per Dog (minutes)",
       fill = "Strategy") +
  theme_minimal()

print(rural_urban_plot)
ggsave("figure/rural_urban_time.png", rural_urban_plot, width = 8, height = 6)





