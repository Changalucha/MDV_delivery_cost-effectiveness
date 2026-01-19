

# analysis originating from "MDV_cost" summary script
# Load summarised dog vaccination, distancenand time data set
rm(list = ls())
#load basic libraries
library(tidyverse)
require(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(reshape2)
library(car)
vaccination_DF <- read.csv("data/monthly_expanded_costs.csv", stringsAsFactors = FALSE) 

################################################################################

#assign rural and urban status for each ward
vaccination_DF <- mutate(vaccination_DF,
                                   location = ifelse(district %in% c("Bunda Town Council", "Musoma Municipal Council",
                                                                     "Tarime Town Council"), "urban", "rural"))
# summary of dogs vaccinated per day per ward
Dogs_summary <- vaccination_DF %>%
  group_by(district, strategy, location, vaccination_year, ward) %>%
  summarise(
    vaccDays = sum(all_vacc_days, na.rm = TRUE),
    totalDogs = sum(vaccinated_dogs, na.rm = TRUE),
    dogsPerDay = totalDogs/vaccDays, na.rm = TRUE)
# total dogs vaccinated
sum(Dogs_summary$totalDogs) # 187778

# dogs per year and strategy
Dogs_summary %>%
  group_by(strategy) %>%
  summarise(ann_dogs = sum(totalDogs))

# average dogs per year
Dogs_summary %>%
  group_by(strategy) %>%
  summarise(
    annual_ward_dogs = mean(totalDogs),
    medi_ward_dogs = median(totalDogs),
    sd_ward_dogs = sd(totalDogs),
    dogs_ci_lower = t.test(totalDogs)$conf.int[1],
    dogs_ci_upper = t.test(totalDogs)$conf.int[2],
  )

# Check distribution of data
ggplot(Dogs_summary, aes(x = totalDogs)) +
  geom_histogram(bins = 20) +
  facet_wrap(~ strategy)

ggplot(Dogs_summary, aes(x = strategy, y = totalDogs)) +
  geom_boxplot(fill = "lightblue", color = "black")

# Perform Welch Two Sample t-test for variation of annual dogs vaccinated per year per ward 
YearlyvaccDogs <- t.test(totalDogs ~ strategy, data = Dogs_summary, var.equal = FALSE)
print(YearlyvaccDogs)
shapiro.test(Dogs_summary$totalDogs) # just ignore as sample size is big

# Perform ANOVA test for variation of dogs per arm and strategy
anova_result <- aov(totalDogs ~ strategy * location, data = Dogs_summary)
# Print ANOVA table
summary(anova_result)

# Shapiro-Wilk normality test on residuals
shapiro.test(residuals(anova_result))

leveneTest(totalDogs ~ strategy * location, data = Dogs_summary)
# run non-parametric test
kruskal.test(totalDogs ~ interaction(strategy, location), data = Dogs_summary)

pairwise.wilcox.test(
  Dogs_summary$totalDogs,
  interaction(Dogs_summary$strategy, Dogs_summary$location),
  p.adjust.method = "bonferroni"
)

# summarise dogs vaccinated per day
Dogs_summary %>%
  group_by(strategy) %>%
  summarise(
    mean_day_dogs = mean(dogsPerDay, na.rm = TRUE),
    median_day_dosg = median(dogsPerDay, na.rm = TRUE),
    sd_totalDogs = sd(dogsPerDay),
    ci_lw_dogs = t.test(dogsPerDay)$conf.int[1],
    ci_up_dogs = t.test(dogsPerDay)$conf.int[2])

# Perform Welch Two Sample t-test of dogs vaccinated per day
DayvaccDogs <- t.test(dogsPerDay ~ strategy, data = Dogs_summary, var.equal = FALSE)
print(DayvaccDogs)

#summarise the sum of quarterly dogs
quartely_data <- vaccination_DF %>%
  group_by(district, ward, strategy, vaccination_year, quarter2) %>%
  summarize(num_dogs = sum(vaccinated_dogs))
# summarise % of vaccinated dogs by each quarter
quartely_BY_strategy <- quartely_data %>%
  group_by(quarter2, strategy) %>%
  summarize(tot_dogs = sum(num_dogs)) %>%
  group_by(strategy) %>%
  mutate(percentage = tot_dogs / sum(tot_dogs) * 100)

#summarise the sum of annual quarterly dogs
conti_quartely_data <- vaccination_DF %>%
  filter(strategy == "continuous") %>%
  group_by(district, ward, vaccination_year, quarter2, Delivery.strategy.type) %>%
  summarize(num_dogs = sum(vaccinated_dogs))

conti_quartely_data$quarter2 <- paste0("Q", conti_quartely_data$quarter2)

#summary of proportion of dogs by strategy and delivery strategy
dogs_BYquarter <- conti_quartely_data %>%
  filter(quarter2 %in% c("Q1", "Q2", "Q3", "Q4")) %>%
  group_by(quarter2) %>%
  summarize(
    dogs = sum(num_dogs),
    mean_vacc_dogs = mean(num_dogs[num_dogs > 0]),  # Exclude zeros from mean
    .groups = "drop"
  ) %>%
  mutate(percentage = dogs / sum(dogs) * 100)

dogs_BYdelivery <- conti_quartely_data %>%
  filter(quarter2 %in% c("Q2", "Q3", "Q4")) %>%
  group_by(Delivery.strategy.type) %>%
  summarize(
    dogs = sum(num_dogs),
    mean_vacc_dogs = mean(num_dogs[num_dogs > 0]),  # Exclude zeros from mean
    .groups = "drop"
  ) %>%
  mutate(percentage = dogs / sum(dogs) * 100)

######################################################################################










