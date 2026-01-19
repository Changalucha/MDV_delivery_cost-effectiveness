
rm(list = ls())
# Load required packages
library(dplyr)
library(tidyr)

# Load DLFO time data set
DLFO_time <- read.csv("data/DLFO_time.csv")
# summaries DLFO oversight time per week and year
DLFO_time_summary <- DLFO_time %>%
  filter(mode == "standard") %>%
  group_by(strategy) %>%
  summarise(meanHrsPerWeek = mean(hoursPerWeek),
            medianHrsPerWeek = median(hoursPerWeek),
            sdHrsPerWeek = sd(hoursPerWeek),
            meanWeeksPerYr = mean(weeksPerYr),
            medianWeeksPerYr = median(weeksPerYr),
            sdWeeksPerYr = sd(weeksPerYr))
