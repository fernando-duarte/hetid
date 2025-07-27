# Analyze Variance Bounds and Their Implications
# Statistical analysis of computed variance bounds for identification

# Load required packages and settings
source(here::here("scripts/utils/common_settings.R"))
load_visualization_packages()

# Load variance bounds results
variance_bounds_results <- readRDS(file.path(OUTPUT_DIR, "temp/variance_bounds/variance_bounds_results.rds"))
variance_bounds_df <- variance_bounds_results$variance_bounds_df

cli_h1("Analyzing Variance Bounds and Their Implications")

# Extract components for analysis
maturities <- variance_bounds_df$Maturity
variance_bounds <- variance_bounds_df$Variance_Bound
c_hat_values <- variance_bounds_df$c_hat
k_hat_values <- variance_bounds_df$k_hat

cli_h2("Variance Bounds Visualization")

# Create comprehensive plots
output_dir <- file.path(OUTPUT_DIR, "temp/variance_bounds")

# Plot 1: Variance bounds across maturities
p1 <- ggplot(variance_bounds_df, aes(x = Maturity, y = Variance_Bound)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  labs(
    title = "Theoretical Variance Bounds Across Maturities",
    subtitle = "Upper bounds for forecast error variance",
    x = "Maturity (years)",
    y = "Variance Bound"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(file.path(output_dir, "variance_bounds_by_maturity.png"),
  p1,
  width = 10, height = 6, dpi = 300
)
ggsave(file.path(output_dir, "variance_bounds_by_maturity.svg"),
  p1,
  width = 10, height = 6
)

# Plot 2: Components decomposition
components_long <- variance_bounds_df %>%
  select(Maturity, c_hat, k_hat) %>%
  pivot_longer(cols = c(c_hat, k_hat), names_to = "Component", values_to = "Value")

p2 <- ggplot(components_long, aes(x = Maturity, y = Value, color = Component)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("c_hat" = "red", "k_hat" = "green"),
    labels = c("ĉ (Supremum)", "k̂ (Fourth Moment)")
  ) +
  labs(
    title = "Variance Bound Components",
    subtitle = "ĉ and k̂ estimators across maturities",
    x = "Maturity (years)",
    y = "Component Value",
    color = "Component"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

ggsave(file.path(output_dir, "variance_bound_components.png"),
  p2,
  width = 10, height = 6, dpi = 300
)
ggsave(file.path(output_dir, "variance_bound_components.svg"),
  p2,
  width = 10, height = 6
)

# Plot 3: Log-scale analysis for better visualization
p3 <- ggplot(variance_bounds_df, aes(x = Maturity)) +
  geom_line(aes(y = log(Variance_Bound)), color = "blue", linewidth = 1.2) +
  geom_point(aes(y = log(Variance_Bound)), color = "darkblue", size = 3) +
  labs(
    title = "Log Variance Bounds Across Maturities",
    subtitle = "Log-scale visualization for trend analysis",
    x = "Maturity (years)",
    y = "Log(Variance Bound)"
  ) +
  theme_minimal()

ggsave(file.path(output_dir, "log_variance_bounds.png"),
  p3,
  width = 10, height = 6, dpi = 300
)
ggsave(file.path(output_dir, "log_variance_bounds.svg"),
  p3,
  width = 10, height = 6
)

cli_h2("Statistical Properties Analysis")

# Analyze growth rates
growth_rates <- diff(log(variance_bounds[variance_bounds > 0]))
if (length(growth_rates) > 0) {
  cli_ul(c(
    paste("Mean log growth rate:", round(mean(growth_rates, na.rm = TRUE), 4)),
    paste("Std dev of growth rates:", round(sd(growth_rates, na.rm = TRUE), 4)),
    paste("Min growth rate:", round(min(growth_rates, na.rm = TRUE), 4)),
    paste("Max growth rate:", round(max(growth_rates, na.rm = TRUE), 4))
  ))
}

# Test for trend in variance bounds
if (length(variance_bounds) > 2) {
  trend_test <- cor.test(maturities, variance_bounds, method = "spearman")
  cli_alert_info("Spearman correlation with maturity: {.val {round(trend_test$estimate, 3)}} (p = {.val {format.pval(trend_test$p.value)}})")
}

cli_h2("Identification Implications")

# Analyze bounds relative to typical forecast error variances
# Use rule of thumb: forecast errors typically have variance 0.001-0.01 for yields
typical_forecast_var_low <- 0.001
typical_forecast_var_high <- 0.01

binding_analysis <- variance_bounds_df %>%
  mutate(
    potentially_binding_low = Variance_Bound < typical_forecast_var_low,
    potentially_binding_high = Variance_Bound < typical_forecast_var_high,
    tightness_ratio_low = Variance_Bound / typical_forecast_var_low,
    tightness_ratio_high = Variance_Bound / typical_forecast_var_high
  )

cli_h3("Binding Constraints Analysis")

n_binding_low <- sum(binding_analysis$potentially_binding_low, na.rm = TRUE)
n_binding_high <- sum(binding_analysis$potentially_binding_high, na.rm = TRUE)

cli_ul(c(
  paste("Maturities with tight bounds (< 0.001):", n_binding_low, "out of", nrow(binding_analysis)),
  paste("Maturities with moderate bounds (< 0.01):", n_binding_high, "out of", nrow(binding_analysis))
))

# Create tightness analysis table
tightness_table <- binding_analysis %>%
  select(Maturity, Variance_Bound, tightness_ratio_low, tightness_ratio_high) %>%
  gt() %>%
  tab_header(
    title = "Variance Bound Tightness Analysis",
    subtitle = "Ratio of bounds to typical forecast error variances"
  ) %>%
  fmt_number(
    columns = c(Variance_Bound, tightness_ratio_low, tightness_ratio_high),
    decimals = 4
  ) %>%
  cols_label(
    Maturity = "Maturity",
    Variance_Bound = "Variance Bound",
    tightness_ratio_low = "Ratio (vs 0.001)",
    tightness_ratio_high = "Ratio (vs 0.01)"
  ) %>%
  tab_style(
    style = cell_fill(color = "lightcoral"),
    locations = cells_body(
      columns = tightness_ratio_low,
      rows = tightness_ratio_low < 1
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "lightyellow"),
    locations = cells_body(
      columns = tightness_ratio_high,
      rows = tightness_ratio_high < 1
    )
  )

print(tightness_table)

cli_h2("Cross-Maturity Relationships")

# Analyze relationships between adjacent maturities
if (length(variance_bounds) > 1) {
  # Compute ratios between consecutive maturities
  vb_ratios <- variance_bounds[-1] / variance_bounds[-length(variance_bounds)]
  vb_ratios <- vb_ratios[is.finite(vb_ratios)]

  if (length(vb_ratios) > 0) {
    cli_h3("Consecutive Maturity Ratios")
    cli_ul(c(
      paste("Mean ratio (VB_i+1 / VB_i):", round(mean(vb_ratios, na.rm = TRUE), 3)),
      paste("Median ratio:", round(median(vb_ratios, na.rm = TRUE), 3)),
      paste("Min ratio:", round(min(vb_ratios, na.rm = TRUE), 3)),
      paste("Max ratio:", round(max(vb_ratios, na.rm = TRUE), 3))
    ))
  }
}

# Analyze component dominance
cli_h3("Component Dominance Analysis")

# Which component drives the variance bound more?
c_hat_rank <- rank(c_hat_values)
k_hat_rank <- rank(k_hat_values)
vb_rank <- rank(variance_bounds)

cor_c_rank <- cor(c_hat_rank, vb_rank, use = "complete.obs")
cor_k_rank <- cor(k_hat_rank, vb_rank, use = "complete.obs")

cli_ul(c(
  paste("Rank correlation (ĉ with VB):", round(cor_c_rank, 3)),
  paste("Rank correlation (k̂ with VB):", round(cor_k_rank, 3))
))

if (abs(cor_c_rank) > abs(cor_k_rank)) {
  cli_alert_info("ĉ component appears to drive variance bound rankings more than k̂")
} else {
  cli_alert_info("k̂ component appears to drive variance bound rankings more than ĉ")
}

# Save analysis results
analysis_results <- list(
  binding_analysis = binding_analysis,
  growth_rates = if (exists("growth_rates")) growth_rates else NULL,
  trend_test = if (exists("trend_test")) trend_test else NULL,
  component_correlations = list(
    c_hat_rank_cor = cor_c_rank,
    k_hat_rank_cor = cor_k_rank
  ),
  consecutive_ratios = if (exists("vb_ratios")) vb_ratios else NULL
)

saveRDS(analysis_results, file.path(output_dir, "variance_bounds_analysis.rds"))

cli_alert_success("Variance bounds analysis completed!")
cli_alert_info("Plots and analysis saved to: {.path {output_dir}}")
