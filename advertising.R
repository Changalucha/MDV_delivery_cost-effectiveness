
rm(list = ls())
# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)

your_data <- read.csv("data/advert_time.csv")
summary(your_data$hours)

# Histogram for weeks per year
ggplot(your_data, aes(x = hours)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of advertising time", x = "hours", y = "Frequency")

ggplot(your_data, aes(x = transport, y = hours, fill = transport)) +
  geom_boxplot() +
  labs(title = "advertising time", x = "Means of transport", y = "Hours") +
  theme(legend.position = "bottom")


# Calculate mean only per given transport and village status
#result <- aggregate(hours ~ transport + village_status, data = your_data, FUN = mean)
result <- aggregate(hours ~ transport, data = your_data, FUN = mean)

# Group by village_status and transport, calculate mean and standard deviation
result_summary <- your_data %>%
  group_by(transport) %>%
  summarize(mean_hours = mean(hours),
            median_hours = median(hours),
            sd_hours = sd(hours))

# Print the result
print(result)
print(result_summary)

