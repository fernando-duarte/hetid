# =============================================================================
# ACM Data Visualization and Summary Statistics
# =============================================================================
# This script visualizes the ACM (Adrian, Crump, and Moench) term structure
# data and produces comprehensive summary statistics.

# -----------------------------------------------------------------------------
# USER PARAMETERS
# -----------------------------------------------------------------------------
save_plots <- TRUE # Whether to save plots
output_dir <- "scripts/output/tests/acm_plots" # Directory for plots
date_range <- NULL # c("2000-01-01", "2023-12-31")  # Date range to analyze (NULL for all)

# -----------------------------------------------------------------------------
# SETUP
# -----------------------------------------------------------------------------
library(hetid)
library(ggplot2)
library(tidyr)
library(dplyr)

if (save_plots && !dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# LOAD DATA
# -----------------------------------------------------------------------------
cat("========================================\n")
cat("ACM Term Structure Data Analysis\n")
cat("========================================\n\n")

# Extract all ACM data
cat("Loading ACM data...\n")
acm_data <- extract_acm_data(
  data_types = c("yields", "term_premia", "risk_neutral"),
  start_date = date_range[1],
  end_date = date_range[2]
)

cat(sprintf(
  "Data loaded: %d observations from %s to %s\n\n",
  nrow(acm_data),
  min(acm_data$date),
  max(acm_data$date)
))

# -----------------------------------------------------------------------------
# SUMMARY STATISTICS
# -----------------------------------------------------------------------------
cat("SUMMARY STATISTICS\n")
cat("==================\n\n")

# Separate data types
yields <- acm_data[, grep("^y\\d+$", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]
risk_neutral <- acm_data[, grep("^rn", names(acm_data))]

# Summary for yields
cat("Yields (y1-y10):\n")
yields_summary <- data.frame(
  Maturity = 1:10,
  Mean = round(colMeans(yields, na.rm = TRUE), 3),
  SD = round(apply(yields, 2, sd, na.rm = TRUE), 3),
  Min = round(apply(yields, 2, min, na.rm = TRUE), 3),
  Max = round(apply(yields, 2, max, na.rm = TRUE), 3),
  Current = round(as.numeric(yields[nrow(yields), ]), 3)
)
print(yields_summary)

cat("\nTerm Premia (tp1-tp10):\n")
tp_summary <- data.frame(
  Maturity = 1:10,
  Mean = round(colMeans(term_premia, na.rm = TRUE), 3),
  SD = round(apply(term_premia, 2, sd, na.rm = TRUE), 3),
  Min = round(apply(term_premia, 2, min, na.rm = TRUE), 3),
  Max = round(apply(term_premia, 2, max, na.rm = TRUE), 3),
  Current = round(as.numeric(term_premia[nrow(term_premia), ]), 3)
)
print(tp_summary)

# Correlation structure
cat("\nCorrelation across maturities (yields):\n")
yield_cor <- cor(yields, use = "complete.obs")
cat("Average correlation:", round(mean(yield_cor[upper.tri(yield_cor)]), 3), "\n")
cat("Min correlation:", round(min(yield_cor[upper.tri(yield_cor)]), 3), "\n")
cat("Max correlation:", round(max(yield_cor[upper.tri(yield_cor)]), 3), "\n")

# -----------------------------------------------------------------------------
# VISUALIZATION 1: Yield Curves Over Time
# -----------------------------------------------------------------------------
cat("\n\nCreating visualizations...\n")

# Prepare data for plotting
yields_long <- acm_data %>%
  select(date, starts_with("y")) %>%
  select(-contains("ry")) %>%
  pivot_longer(cols = -date, names_to = "maturity", values_to = "yield") %>%
  mutate(maturity = as.numeric(gsub("y", "", maturity)))

# Select specific dates for yield curve snapshot
snapshot_dates <- c(
  min(acm_data$date),
  "2008-12-01", # Financial crisis
  "2020-03-01", # COVID
  max(acm_data$date)
)
snapshot_dates <- snapshot_dates[snapshot_dates %in% acm_data$date]

yields_snapshot <- yields_long %>%
  filter(date %in% snapshot_dates)

p_yield_curves <- ggplot(yields_snapshot, aes(
  x = maturity, y = yield,
  color = as.factor(date)
)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Yield Curves at Different Points in Time",
    x = "Maturity (years)",
    y = "Yield (%)",
    color = "Date"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_yield_curves)

# -----------------------------------------------------------------------------
# VISUALIZATION 2: Term Structure Evolution
# -----------------------------------------------------------------------------
# Create 3D-style plot showing evolution
selected_maturities <- c(1, 2, 5, 10)
yields_evolution <- yields_long %>%
  filter(maturity %in% selected_maturities) %>%
  mutate(maturity = factor(maturity, levels = selected_maturities))

p_evolution <- ggplot(yields_evolution, aes(
  x = date, y = yield,
  color = maturity
)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(end = 0.9) +
  labs(
    title = "Evolution of Yields by Maturity",
    x = "Date",
    y = "Yield (%)",
    color = "Maturity\n(years)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(p_evolution)

# -----------------------------------------------------------------------------
# VISUALIZATION 3: Term Premia Analysis
# -----------------------------------------------------------------------------
tp_long <- acm_data %>%
  select(date, starts_with("tp")) %>%
  pivot_longer(cols = -date, names_to = "maturity", values_to = "term_premium") %>%
  mutate(maturity = as.numeric(gsub("tp", "", maturity)))

# Average term premium by maturity
tp_avg <- tp_long %>%
  group_by(maturity) %>%
  summarise(
    mean_tp = mean(term_premium, na.rm = TRUE),
    sd_tp = sd(term_premium, na.rm = TRUE)
  )

p_tp_profile <- ggplot(tp_avg, aes(x = maturity)) +
  geom_ribbon(aes(ymin = mean_tp - sd_tp, ymax = mean_tp + sd_tp),
    alpha = 0.3, fill = "steelblue"
  ) +
  geom_line(aes(y = mean_tp), linewidth = 1.5, color = "darkblue") +
  geom_point(aes(y = mean_tp), size = 3, color = "darkblue") +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Average Term Premium Profile (±1 SD band)",
    x = "Maturity (years)",
    y = "Term Premium (%)"
  ) +
  theme_minimal()

print(p_tp_profile)

# -----------------------------------------------------------------------------
# VISUALIZATION 4: Yield vs Term Premium Scatter
# -----------------------------------------------------------------------------
# For 5-year maturity
scatter_data <- data.frame(
  date = acm_data$date,
  yield = acm_data$y5,
  term_premium = acm_data$tp5
)

p_scatter <- ggplot(scatter_data, aes(x = yield, y = term_premium)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "5-Year Yield vs Term Premium",
    x = "5-Year Yield (%)",
    y = "5-Year Term Premium (%)"
  ) +
  theme_minimal()

print(p_scatter)

# Correlation
correlation <- cor(scatter_data$yield, scatter_data$term_premium,
  use = "complete.obs"
)
cat(sprintf("\nCorrelation between 5Y yield and term premium: %.3f\n", correlation))

# -----------------------------------------------------------------------------
# VISUALIZATION 5: Slope of Yield Curve
# -----------------------------------------------------------------------------
# Define slope as 10Y - 2Y
slope_data <- data.frame(
  date = acm_data$date,
  slope = acm_data$y10 - acm_data$y2
)

p_slope <- ggplot(slope_data, aes(x = date, y = slope)) +
  geom_line(linewidth = 1, color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = pmin(slope, 0), ymax = 0),
    fill = "red", alpha = 0.3
  ) +
  labs(
    title = "Yield Curve Slope (10Y - 2Y)",
    x = "Date",
    y = "Slope (%)",
    caption = "Red shading indicates yield curve inversion"
  ) +
  theme_minimal()

print(p_slope)

# Inversion statistics
inversions <- sum(slope_data$slope < 0, na.rm = TRUE)
total_obs <- sum(!is.na(slope_data$slope))
cat(sprintf(
  "\nYield curve inversions: %d out of %d observations (%.1f%%)\n",
  inversions, total_obs, inversions / total_obs * 100
))

# -----------------------------------------------------------------------------
# SAVE PLOTS
# -----------------------------------------------------------------------------
if (save_plots) {
  cat("\nSaving plots...\n")
  ggsave(file.path(output_dir, "yield_curves_snapshot.pdf"),
    p_yield_curves,
    width = 10, height = 6
  )
  ggsave(file.path(output_dir, "yield_evolution.pdf"),
    p_evolution,
    width = 12, height = 6
  )
  ggsave(file.path(output_dir, "term_premium_profile.pdf"),
    p_tp_profile,
    width = 8, height = 6
  )
  ggsave(file.path(output_dir, "yield_tp_scatter.pdf"),
    p_scatter,
    width = 8, height = 6
  )
  ggsave(file.path(output_dir, "yield_curve_slope.pdf"),
    p_slope,
    width = 12, height = 6
  )
  cat(sprintf("Plots saved to %s\n", output_dir))
}

cat("\nACM data analysis complete!\n")
