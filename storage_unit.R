
rm(list = ls())
# Load required packages
library(dplyr)
library(tidyr)

#storage cost
#LOAD STORAGE DATA
storage_data <- read.csv("data/storage_data.csv")
# add to pulsed space a freezer compartment, assume half of the ice pack are used for pulsed arm = 0.1
storage_data$pulsed_space <- storage_data$pulsed_space + 0.1
#summaries mean storage space
storageCost <- storage_data %>%
  summarise(pulse_space = mean(pulsed_space),
            sdPulse = sd(pulsed_space),
            conti_space = mean(continuous_space),
            sdConti = sd(continuous_space))
