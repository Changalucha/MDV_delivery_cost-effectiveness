
# === Load Required Libraries ===
rm(list = ls())
library(tidyverse)
library(lme4)
library(broom)
library(ggplot2)
library(scales)
library(ggrepel)
library(patchwork)
library(dplyr)

# Load the data
monthly_ward_costs <- read.csv("data/monthly_expanded_costs.csv")

# === DATA PREPARATION ===
yrly_ward_costs <- monthly_ward_costs %>%
  group_by(district, ward, strategy, vaccination_year) %>%
  summarise(
    total_dogs = sum(vaccinated_dogs, na.rm = TRUE),
    total_total = sum(total_cost, na.rm = TRUE)
  )

monthly_ward_costs <- monthly_ward_costs %>% drop_na(total_cost, vaccinated_dogs, strategy, district, ward)
yrly_ward_costs <- yrly_ward_costs %>% drop_na(total_total, total_dogs, strategy, district)

# === REGRESSION-BASED ICER ANALYSIS ===
get_adjusted_ICER <- function(data, n_boot = 1000, seed = 123) {
  set.seed(seed)
  
  # Ensure district is a factor
  data$district <- factor(data$district)
  
  # Fit base models
  cost_model <- lm(total_total ~ strategy + district, data = data)
  effect_model <- lm(total_dogs ~ strategy + district, data = data)
  
  # Prepare prediction data
  newdata <- data.frame(strategy = c("continuous", "pulsed"))
  newdata$district <- factor(levels(data$district)[1], levels = levels(data$district))
  
  # Point estimates
  predicted_costs <- predict(cost_model, newdata = newdata)
  predicted_effects <- predict(effect_model, newdata = newdata)
  
  adj_cost_diff <- predicted_costs[1] - predicted_costs[2]
  adj_effect_diff <- predicted_effects[1] - predicted_effects[2]
  adjusted_ICER <- adj_cost_diff / adj_effect_diff
  
  # --- Bootstrap procedure ---
  boot_icers <- replicate(n_boot, {
    boot_data <- data[sample(nrow(data), replace = TRUE), ]
    boot_data$district <- factor(boot_data$district)
    
    tryCatch({
      cost_model_b <- lm(total_total ~ strategy + district, data = boot_data)
      effect_model_b <- lm(total_dogs ~ strategy + district, data = boot_data)
      
      cost_pred <- predict(cost_model_b, newdata = newdata)
      effect_pred <- predict(effect_model_b, newdata = newdata)
      
      cost_diff <- cost_pred[1] - cost_pred[2]
      effect_diff <- effect_pred[1] - effect_pred[2]
      
      if (effect_diff != 0) {
        return(cost_diff / effect_diff)
      } else {
        return(NA)
      }
    }, error = function(e) return(NA))
  })
  
  boot_icers <- na.omit(boot_icers)
  icer_ci <- quantile(boot_icers, probs = c(0.025, 0.975), na.rm = TRUE)
  
  # Return as a data frame
  result_df <- data.frame(
    adjusted_ICER = adjusted_ICER,
    lower_95_CI = icer_ci[1],
    upper_95_CI = icer_ci[2],
    n_bootstrap = length(boot_icers)
  )
  
  return(result_df)
}

# Run the adjusted ICER analysis
adjusted_ICER_result <- get_adjusted_ICER(yrly_ward_costs)
print(adjusted_ICER_result)


# Debug the bootstrap data first
debug_bootstrap_data <- function(icer_results) {
  print("=== DEBUGGING BOOTSTRAP DATA ===")
  print("Structure of bootstrap_data:")
  print(str(icer_results$bootstrap_data))
  print("Column names:")
  print(colnames(icer_results$bootstrap_data))
  print("First few rows:")
  print(head(icer_results$bootstrap_data))
}

# 1. Define the fixed bootstrap function
get_adjusted_ICER_with_bootstrap_fixed <- function(data, n_boot = 1000, seed = 123) {
  set.seed(seed)
  
  # Ensure district is a factor
  data$district <- factor(data$district)
  
  # Fit base models
  cost_model <- lm(total_total ~ strategy + district, data = data)
  effect_model <- lm(total_dogs ~ strategy + district, data = data)
  
  # Prepare prediction data
  newdata <- data.frame(strategy = c("continuous", "pulsed"))
  newdata$district <- factor(levels(data$district)[1], levels = levels(data$district))
  
  # Point estimates
  predicted_costs <- predict(cost_model, newdata = newdata)
  predicted_effects <- predict(effect_model, newdata = newdata)
  
  adj_cost_diff <- predicted_costs[1] - predicted_costs[2]
  adj_effect_diff <- predicted_effects[1] - predicted_effects[2]
  adjusted_ICER <- adj_cost_diff / adj_effect_diff
  
  # Bootstrap procedure
  boot_cost_diffs <- numeric(n_boot)
  boot_effect_diffs <- numeric(n_boot)
  
  for(i in 1:n_boot) {
    boot_data <- data[sample(nrow(data), replace = TRUE), ]
    boot_data$district <- factor(boot_data$district)
    
    tryCatch({
      cost_model_b <- lm(total_total ~ strategy + district, data = boot_data)
      effect_model_b <- lm(total_dogs ~ strategy + district, data = boot_data)
      
      cost_pred <- predict(cost_model_b, newdata = newdata)
      effect_pred <- predict(effect_model_b, newdata = newdata)
      
      boot_cost_diffs[i] <- cost_pred[1] - cost_pred[2]
      boot_effect_diffs[i] <- effect_pred[1] - effect_pred[2]
    }, error = function(e) {
      boot_cost_diffs[i] <- NA
      boot_effect_diffs[i] <- NA
    })
  }
  
  # Create bootstrap dataframe
  boot_df <- data.frame(
    cost_diff = boot_cost_diffs,
    effect_diff = boot_effect_diffs
  )
  boot_df <- boot_df[complete.cases(boot_df), ]
  
  # Calculate ICERs
  boot_icers <- ifelse(boot_df$effect_diff != 0, 
                       boot_df$cost_diff / boot_df$effect_diff, 
                       NA)
  boot_icers <- na.omit(boot_icers)
  icer_ci <- quantile(boot_icers, probs = c(0.025, 0.975), na.rm = TRUE)
  
  # Return results
  result <- list(
    summary = data.frame(
      adjusted_ICER = adjusted_ICER,
      incremental_cost = adj_cost_diff,
      incremental_effect = adj_effect_diff,
      lower_95_CI = icer_ci[1],
      upper_95_CI = icer_ci[2],
      n_bootstrap = nrow(boot_df)
    ),
    bootstrap_data = boot_df,
    bootstrap_icers = boot_icers
  )
  
  return(result)
}

# 2. Define the plotting function
plot_ce_plane <- function(icer_results, willingness_to_pay = c(50, 100, 200)) {
  
  boot_data <- icer_results$bootstrap_data
  summary_data <- icer_results$summary
  
  p <- ggplot(boot_data, aes(x = effect_diff, y = cost_diff)) +
    geom_point(alpha = 0.3, size = 0.8, color = "steelblue") +
    
    # Add point estimate
    geom_point(data = summary_data, 
               aes(x = incremental_effect, y = incremental_cost),
               color = "red", size = 5, shape = 18) +
    
    # Add origin lines
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7, linewidth = 1.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7, linewidth = 1.5) +
    
    # Add WTP threshold lines
    # geom_abline(slope = willingness_to_pay, intercept = 0, 
    #             linetype = "dotted", alpha = 0.6, 
    #             color = c("orange", "purple", "darkgreen")) +
    
    labs(
      title = "",
      subtitle = paste0("ICER = $", round(summary_data$adjusted_ICER, 2), 
                        " per additional dog vaccinated"),
      x = "Incremental Effectiveness (Additional Dogs Vaccinated)",
      y = "Incremental Cost ($)",
      # caption = paste0("WTP thresholds: $", paste(willingness_to_pay, collapse = ", $"), 
      #                  " per dog | Red diamond = Point estimate")
    ) +
    
    theme_minimal() +
    scale_x_continuous(labels = scales::comma_format()) +
    scale_y_continuous(labels = scales::dollar_format())
  
  return(p)
}

# 3. Run the analysis
print("Creating ICER results with bootstrap data...")
icer_results_fixed <- get_adjusted_ICER_with_bootstrap_fixed(yrly_ward_costs)

# 4. Print results
print("=== ICER SUMMARY ===")
print(icer_results_fixed$summary)

# 5. Create and display the plot
print("Creating cost-effectiveness plane plot...")
ce_plane_plot_fixed <- plot_ce_plane(icer_results_fixed)
print(ce_plane_plot_fixed)

# Save the plots
ggsave("figure/ICER_plane.png", ce_plane_plot_fixed, width = 8, height = 6, dpi = 400, bg = "white")


# === MARGINAL COST ANALYSIS WITH FIXED EFFECTS ===
# === Prepare Data (following methods statement) ===
monthly_ward_costs <- monthly_ward_costs %>%
  drop_na(total_cost, vaccinated_dogs, strategy, district, ward) %>%
  mutate(
    # Create binary strategy variable (1 = community-led, 0 = team-led)
    strategy_bin = ifelse(strategy == "continuous", 1, 0),
    
    # Calculate cost per vaccinated dog (outcome variable per methods)
    cost_per_vax = total_cost / vaccinated_dogs,
    
    # Log transformations for different model forms
    log_cost_per_vax = log(cost_per_vax),
    log_vaccinated_dogs = log(vaccinated_dogs + 1),  # +1 to avoid log(0)
    
    # Ensure ward is a factor for random effects
    ward = as.factor(ward)
  ) %>%
  # Optional: Remove extreme outliers if necessary
  filter(cost_per_vax < 15)

# === MODEL 1: LINEAR MODEL ===
# Cost per dog ~ vaccination volume + strategy + interaction
# Direct interpretation: marginal cost in dollars
model_linear_marginal <- lmer(
  cost_per_vax ~ vaccinated_dogs * strategy_bin + (1 | ward),
  data = monthly_ward_costs
)

# === MODEL 2: LOG-LINEAR MODEL ===
# Log(cost per dog) ~ vaccination volume + strategy + interaction  
# Semi-elasticity: % change in cost per additional dog vaccinated
model_log_linear_marginal <- lmer(
  log_cost_per_vax ~ vaccinated_dogs * strategy_bin + (1 | ward),
  data = monthly_ward_costs
)

# === MODEL 3: LOG-LOG MODEL ===
# Log(cost per dog) ~ log(vaccination volume) + strategy + interaction
# Elasticity: % change in cost per 1% change in vaccination volume
model_log_log_marginal <- lmer(
  log_cost_per_vax ~ log_vaccinated_dogs * strategy_bin + (1 | ward),
  data = monthly_ward_costs
)

# === COMPARE MODEL FIT USING AIC ===
aic_comparison <- AIC(model_linear_marginal, model_log_linear_marginal, model_log_log_marginal)
print("=== AIC Comparison for Marginal Cost Models ===")
print(aic_comparison)

# Add model names for clarity
aic_comparison$Model <- c("Linear", "Log-Linear", "Log-Log")
aic_comparison <- aic_comparison[order(aic_comparison$AIC), ]
print("\n=== AIC Results (Best to Worst) ===")
print(aic_comparison)

# Calculate AIC differences from best model
best_aic <- min(aic_comparison$AIC)
aic_comparison$Delta_AIC <- aic_comparison$AIC - best_aic
print("\n=== AIC Differences from Best Model ===")
print(aic_comparison)

# === DISPLAY SUMMARIES OF ALL MODELS ===
print("\n=== LINEAR MODEL SUMMARY ===")
print(summary(model_linear_marginal))

print("\n=== LOG-LINEAR MODEL SUMMARY ===")
print(summary(model_log_linear_marginal))

print("\n=== LOG-LOG MODEL SUMMARY ===")
print(summary(model_log_log_marginal))

# === MARGINAL COST CALCULATION (LINEAR MODEL) ===
# Calculate marginal costs at mean vaccination level  
model_total_cost <- lmer(total_cost ~ vaccinated_dogs * strategy_bin + (1 | ward), 
                         data = monthly_ward_costs)

# Extract marginal costs (coefficients for vaccinated_dogs)
total_coefs <- fixef(model_total_cost)
team_marginal_total <- total_coefs["vaccinated_dogs"]
community_marginal_total <- total_coefs["vaccinated_dogs"] + total_coefs["vaccinated_dogs:strategy_bin"]

print(paste("Team-led marginal cost: $", round(team_marginal_total, 2)))
print(paste("Community-led marginal cost: $", round(community_marginal_total, 2)))

# ============================
# VISUALIZATION: MARGINAL COST PLOTS
# ============================
# === Generate Predictions for Plotting ===
monthly_ward_costs <- monthly_ward_costs %>%
  mutate(
    pred_log_cost = predict(model_log_log_marginal),
    pred_cost = exp(pred_log_cost)
  )

# === PLOT 1: All Districts - General Plot ===
marginal_model_plot <- ggplot(monthly_ward_costs, aes(x = vaccinated_dogs, y = cost_per_vax)) +
  geom_point(aes(color = factor(strategy_bin), shape = factor(strategy_bin)),
             alpha = 0.4) +
  geom_line(aes(y = pred_cost, color = factor(strategy_bin)),
            size = 1.2) +
  scale_color_manual(
    values = c("0" = "red", "1" = "blue"),
    labels = c("Team-led", "Community-led"),
    name = "Strategy"
  ) +
  scale_shape_manual(
    values = c("0" = 1, "1" = 3),
    labels = c("Team-led", "Community-led"),
    name = "Strategy"
  ) +
  labs(
    x = "Vaccinations delivered",
    y = "Cost per vaccination ($)",
    title = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 14)
  )

print(marginal_model_plot)

# === PLOT 2: Single District - Smoother Curve ===
# Filter for a representative district
#rep_district <- monthly_ward_costs$district[1]  # Or set manually
rep_district <- "Musoma Municipal Council"
district_data <- monthly_ward_costs %>%
  filter(district == rep_district, cost_per_vax < 15)

# Fit simplified model for smoother curve (fixed effects only)
model_simple <- lm(log(cost_per_vax) ~ log(vaccinated_dogs) * strategy_bin, data = district_data)

# Generate predictions
district_data <- district_data %>%
  mutate(
    pred_log = predict(model_simple),
    pred_cost = exp(pred_log)
  )

# Create single district plot
single_district_plot <- ggplot(district_data, aes(x = vaccinated_dogs, y = cost_per_vax)) +
  geom_point(aes(color = factor(strategy_bin)), alpha = 0.5) +
  geom_line(aes(y = pred_cost, color = factor(strategy_bin)), size = 1.2) +
  scale_color_manual(
    values = c("0" = "red", "1" = "blue"),
    labels = c("Team-led", "Community-led"),
    name = "Strategy"
  ) +
  labs(
    x = "Vaccinated dogs",
    y = "Cost per dog vaccinated ($)",
    title = paste("Log-Log Trend (Smooth) —", rep_district)
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12)
  )

print(single_district_plot)

# === SAVE PLOTS ===
ggsave("figure/loglog_cost_curve.png", marginal_model_plot, width = 8, height = 6, dpi = 400, bg = "white")
ggsave("figure/loglog_cost_curve_single_district.png", single_district_plot, width = 8, height = 6, dpi = 400, bg = "white")


# ============================
# TABLES FOR MARGINAL COST ANALYSIS
# ============================
library(broom.mixed)
library(knitr)
library(kableExtra)
# ============================
# TABLE 1: DETAILED RESULTS (LOG-LOG MODEL ONLY)
# ============================

# Extract coefficients from best model
log_log_results <- tidy(model_log_log_marginal, conf.int = TRUE) %>%
  filter(effect == "fixed") %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "log_vaccinated_dogs" ~ "Log(Vaccinations)",
      term == "strategy_bin" ~ "Strategy (Community-led)",
      term == "log_vaccinated_dogs:strategy_bin" ~ "Log(Vaccinations) × Community-led",
      TRUE ~ term
    ),
    # Round all numeric columns
    across(where(is.numeric), ~round(.x, 3))
  ) %>%
  dplyr::select(term, estimate, std.error, conf.low, conf.high, statistic)

# Create manuscript-ready table
kable(log_log_results,
      caption = "Table Y: Log-log mixed-effects model results for cost per dog vaccinated",
      col.names = c("Parameter", "Coefficient", "SE", "95% CI Lower", "95% CI Upper", "t-value"),
      digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  footnote(general = c("Model: Log(Cost per dog) ~ Log(Vaccinations) + Strategy + Log(Vaccinations) × Strategy + (1|Ward)",
                       "n = 1,980 observations across 110 wards",
                       "Negative coefficients indicate economies of scale",
                       "t-values > |1.96| approximately significant at p < 0.05"),
           general_title = "Notes:")

# ============================
# ALTERNATIVE: COMBINED SUMMARY TABLE
# ============================

# Create a comprehensive summary table (alternative to separate tables)
combined_table <- data.frame(
  Model = c("Linear", "Log-Linear", "Log-Log"),
  AIC = c(
    round(AIC(model_linear_marginal), 0),
    round(AIC(model_log_linear_marginal), 0),
    round(AIC(model_log_log_marginal), 0)
  ),
  Team_led_effect = c(
    paste0("$", round(fixef(model_linear_marginal)["vaccinated_dogs"], 3)),
    round(fixef(model_log_linear_marginal)["vaccinated_dogs"], 4),
    round(abs(fixef(model_log_log_marginal)["log_vaccinated_dogs"]), 3)
  ),
  Community_led_effect = c(
    paste0("$", round(fixef(model_linear_marginal)["vaccinated_dogs"] + 
                        fixef(model_linear_marginal)["vaccinated_dogs:strategy_bin"], 3)),
    round(fixef(model_log_linear_marginal)["vaccinated_dogs"] + 
            fixef(model_log_linear_marginal)["vaccinated_dogs:strategy_bin"], 4),
    round(abs(fixef(model_log_log_marginal)["log_vaccinated_dogs"] + 
                fixef(model_log_log_marginal)["log_vaccinated_dogs:strategy_bin"]), 3)
  )
)

kable(combined_table,
      caption = "Table: Marginal cost analysis results across model specifications",
      col.names = c("Model", "AIC", "Team-led Effect", "Community-led Effect")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  pack_rows("Model Comparison", 1, 3) %>%
  footnote(general = c("Linear: Direct marginal cost per additional dog ($)",
                       "Log-linear: Semi-elasticity (coefficient)",  
                       "Log-log: Cost elasticity (absolute value shown)",
                       "Best model highlighted by lowest AIC"),
           general_title = "Notes:")

print("✓ All tables created successfully!")






