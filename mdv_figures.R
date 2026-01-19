
#############################################
# Rabies Vaccination: Costs & Coverage Plots
#############################################

# ---- 0) Setup ----
rm(list = ls())

# Core tidyverse (dplyr, ggplot2, readr, tidyr, etc.)
library(tidyverse)
library(lubridate)
library(sf)
library(patchwork)
library(scales)
library(viridis)
library(grid)

# Ensure output dir exists
if (!dir.exists("figure")) dir.create("figure", recursive = TRUE)

# Helper: standard error
se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

# ---- 1) Load data (once) ----
# Use readr::read_csv (stringsAsFactors handled; faster, consistent types)
expanded_costs <- readr::read_csv("output/monthly_expanded_costs.csv", show_col_types = FALSE)
household_data <- readr::read_csv("output/household_survey_data.csv", show_col_types = FALSE)

# Coerce date if present (some CSVs save as char)
if (!inherits(expanded_costs$month_interval, "Date")) {
  expanded_costs <- expanded_costs %>%
    mutate(month_interval = as.Date(month_interval))
}

# ---- 1) Monthly vaccinated dogs & costs by delivery strategy type ----
# Harmonise strategy labels (used in plots below)
vaccination_costs_df <- expanded_costs %>%
  mutate(
    Delivery.strategy.type = case_when(
      Delivery.strategy.type %in% c("On Demand", "Visit for Other Reasons") ~ "House to House",
      TRUE ~ Delivery.strategy.type
    ),
    strategy = case_when(
      strategy == "continuous" ~ "Community-led",
      strategy == "pulsed"     ~ "Team-led",
      TRUE ~ strategy
    )
  )

facet_labels <- c("first_year" = "Year 1", "second_year" = "Year 2", "third_year" = "Year 3")

# Legend labels + colors
legend_labels <- c(
  "Central Point - Subvillage" = "Sub-village level",
  "Central Point - Village"    = "Village-level",
  "House to House"             = "House-to-House"
)
colors <- setNames(viridis::viridis(3),
                   c("Central Point - Subvillage", "Central Point - Village", "House to House"))

strategy_colors <- c("Community-led" = "#1f77b4", "Team-led" = "#ff7f0e")

# First, determine the full date range for consistent x-axis
date_range <- vaccination_costs_df %>%
  summarise(
    min_date = min(month_interval, na.rm = TRUE),
    max_date = max(month_interval, na.rm = TRUE)
  )

# Community-led monthly vaccinated dogs (stacked by delivery type)
plot_continuous <- vaccination_costs_df %>%
  filter(strategy == "Community-led") %>%
  ggplot(aes(x = month_interval, y = vaccinated_dogs, fill = Delivery.strategy.type)) +
  geom_col() +
  labs(x = NULL, y = "Dogs Community-led", fill = "Delivery Strategy Type") +
  scale_fill_manual(values = colors, labels = legend_labels) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 8, face = "bold"),
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.y = element_text(size = 8, vjust = 0.5),
    plot.margin = unit(c(0, 0, -0.5, 0), "cm")
  ) +
  facet_wrap(~ vaccination_year, scales = "free_x",
             labeller = labeller(vaccination_year = facet_labels))

# Team-led monthly vaccinated dogs (stacked by delivery type)
plot_pulsed <- vaccination_costs_df %>%
  filter(strategy == "Team-led") %>%
  ggplot(aes(x = month_interval, y = vaccinated_dogs, fill = Delivery.strategy.type)) +
  geom_col() +
  labs(x = NULL, y = "Dogs Team-led", fill = "Delivery Strategy Type") +
  scale_fill_manual(values = colors, labels = legend_labels) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 8, face = "bold"),
    legend.position = "none",
    legend.title = element_blank(),
    axis.text.y = element_text(size = 8, vjust = 0.5),
    plot.margin = unit(c(0, 0, -0.5, 0), "cm"),
    strip.text = element_blank()
  ) +
  facet_wrap(~ vaccination_year, scales = "free_x")

# Monthly total costs (by strategy) - add faceting to match alignment with top plots
plot_costs <- vaccination_costs_df %>%
  ggplot(aes(x = month_interval, y = total_cost, fill = strategy)) +
  geom_col(position = "dodge") +
  labs(x = "Months", y = "Cost ($)", fill = "Strategy") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
  scale_fill_manual(values = strategy_colors) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 8, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8, vjust = 0.5),
    strip.text = element_blank()
  ) +
  facet_wrap(~ vaccination_year, scales = "free_x")

# Combine and print
combined_plot <- plot_continuous / plot_pulsed / plot_costs +
  patchwork::plot_layout(ncol = 1, heights = c(1, 1, 1))

print(combined_plot)
ggsave(filename = "figure/Cost_Vax_combo.pdf", plot = combined_plot,
       units = "cm", dpi = 300, width = 25, height = 20)
ggsave(filename = "figure/combined_plot2.jpg", plot = combined_plot,
       units = "cm", dpi = 300, width = 25, height = 20)


# ---- 5) Household survey: total vs vaccinated dogs over months ----
# Month bucketing helper
create_month_intervals <- function(dates) floor_date(as.Date(dates), unit = "month")

household_data <- household_data %>%
  mutate(month_interval = create_month_intervals(date))

HHS_summary <- household_data %>%
  group_by(district, ward, strategy, vaccination_year, month_interval, Household.file.id) %>%
  summarise(
    total_dogs        = first(num.dogs.household + num.dogs.household.puppy),
    vaccinated_dogs   = first(num.dogs.vacc.owner.recall + num.puppies.vacc.owner.recall),
    unvaccinated_dogs = first(num.dogs.not.vacc.owner.recall + num.puppies.not.vacc.owner.recall),
    .groups = "drop"
  ) %>%
  group_by(district, ward, strategy, vaccination_year, month_interval) %>%
  summarise(
    total_dogs        = sum(total_dogs, na.rm = TRUE),
    vaccinated_dogs   = sum(vaccinated_dogs, na.rm = TRUE),
    unvaccinated_dogs = sum(unvaccinated_dogs, na.rm = TRUE),
    .groups = "drop"
  )

# Use a proper month index per vaccination year (no 30-day approximation)
plot_data <- HHS_summary %>%
  group_by(vaccination_year) %>%
  arrange(month_interval, .by_group = TRUE) %>%
  mutate(
    month_num = dense_rank(month_interval),
    vaccination_year = factor(vaccination_year,
                              levels = c("first_year", "second_year", "third_year"),
                              labels = c("Year 1", "Year 2", "Year 3")),
    strategy = factor(strategy,
                      levels = c("continuous", "pulsed"),
                      labels = c("Community led", "Team led"))
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(total_dogs, vaccinated_dogs),
    names_to = "dog_type",
    values_to = "count"
  )

HHS_dogs_plot <- ggplot(plot_data, aes(x = month_num, y = count, fill = dog_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(strategy ~ vaccination_year, scales = "free_x") +
  scale_fill_manual(
    values = c("total_dogs" = "#2171B5", "vaccinated_dogs" = "#4DAF4A"),
    labels = c("Total Dogs", "Vaccinated Dogs")
  ) +
  scale_x_continuous(
    breaks = 1:max(plot_data$month_num, na.rm = TRUE),
    expand = c(0, 0)
  ) +
  labs(x = "Month", y = "Number of Dogs", fill = NULL) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text.y = element_text(angle = 90),
    panel.grid.major.x = element_blank()
  )

print(HHS_dogs_plot)
ggsave(filename = "figure/HHS_dogs1.pdf", plot = HHS_dogs_plot, units = "cm", dpi = 300, width = 25, height = 20)
ggsave(filename = "figure/HHS_dogs1.jpg", plot = HHS_dogs_plot, units = "cm", dpi = 300, width = 25, height = 20)

#############################################
# End of script
#############################################





