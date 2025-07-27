# Compute Theoretical Variance Bounds
# Calculate variance bounds for forecast error variance across all maturities

# Load required packages and settings
source(here::here("scripts/utils/common_settings.R"))
# Core packages (hetid, dplyr, tidyr, gt, DT, here, cli) loaded via common_settings.R

# Load processed data
data <- readRDS(file.path(OUTPUT_DIR, "temp/data.rds"))

# Convert to data frame if it's a list
if (is.list(data) && !is.data.frame(data)) {
  data <- as.data.frame(data)
}

cli_h1("Computing Theoretical Variance Bounds")

# Extract yields and term premia
yield_vars <- grep("^y\\d+$", names(data), value = TRUE)
tp_vars <- grep("^tp\\d+$", names(data), value = TRUE)

# Get yield maturities from variable names
maturities <- as.numeric(gsub("y", "", yield_vars))

# Ensure maturities are sorted
if (!all(maturities == sort(maturities))) {
  stop("Yield variables must be sorted by maturity")
}

cli_ul(c(
  paste("Maturities included:", paste(maturities, collapse = ", ")),
  paste("Date range:", paste(format(range(data$date), "%Y-%m-%d"), collapse = " to ")),
  paste("Number of observations:", nrow(data))
))

# Extract yields and term premia data frames
yields_df <- data[, yield_vars]
tp_df <- data[, tp_vars]

cli_alert_info("Computing variance bounds for all maturities...")

# Initialize storage for results
variance_bounds <- numeric(length(maturities))
c_hat_values <- numeric(length(maturities))
k_hat_values <- numeric(length(maturities))
names(variance_bounds) <- paste0("maturity_", maturities)
names(c_hat_values) <- paste0("maturity_", maturities)
names(k_hat_values) <- paste0("maturity_", maturities)

# Compute variance bounds for each maturity
for (idx in seq_along(maturities)) {
  mat <- maturities[idx]

  cli_alert("Computing variance bound for maturity {.val {mat}}...")

  # Compute individual components
  c_hat_values[idx] <- compute_c_hat(yields_df, tp_df, i = mat)
  k_hat_values[idx] <- compute_k_hat(yields_df, tp_df, i = mat)

  # Compute variance bound: (1/4) * c_hat * k_hat
  variance_bounds[idx] <- compute_variance_bound(yields_df, tp_df, i = mat)

  cli_alert_success("Maturity {.val {mat}}: VB = {.val {round(variance_bounds[idx], 6)}}")
}

# Create comprehensive results data frame
variance_bounds_df <- data.frame(
  Maturity = maturities,
  c_hat = c_hat_values,
  k_hat = k_hat_values,
  Variance_Bound = variance_bounds,
  stringsAsFactors = FALSE
)

cli_h2("Variance Bounds Summary")

# Create formatted table
variance_bounds_table <- variance_bounds_df %>%
  gt() %>%
  tab_header(
    title = "Theoretical Variance Bounds",
    subtitle = "Components and bounds across maturities"
  ) %>%
  fmt_number(
    columns = c(c_hat, k_hat, Variance_Bound),
    decimals = 6
  ) %>%
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(columns = Maturity)
  ) %>%
  tab_style(
    style = cell_fill(color = "lightyellow"),
    locations = cells_body(columns = Variance_Bound)
  ) %>%
  cols_label(
    Maturity = "Maturity (years)",
    c_hat = "ĉ",
    k_hat = "k̂",
    Variance_Bound = "Variance Bound"
  )

print(variance_bounds_table)

# Summary statistics for variance bounds
cli_h2("Variance Bounds Statistics")

vb_stats <- data.frame(
  Statistic = c("Mean", "Median", "Min", "Max", "Std Dev", "Range"),
  Value = c(
    mean(variance_bounds, na.rm = TRUE),
    median(variance_bounds, na.rm = TRUE),
    min(variance_bounds, na.rm = TRUE),
    max(variance_bounds, na.rm = TRUE),
    sd(variance_bounds, na.rm = TRUE),
    max(variance_bounds, na.rm = TRUE) - min(variance_bounds, na.rm = TRUE)
  )
) %>%
  gt() %>%
  tab_header(title = "Variance Bounds Summary Statistics") %>%
  fmt_number(columns = Value, decimals = 6)

print(vb_stats)

# Analyze patterns across maturities
cli_h2("Maturity Structure Analysis")

# Check monotonicity
increases <- sum(diff(variance_bounds) > 0, na.rm = TRUE)
decreases <- sum(diff(variance_bounds) < 0, na.rm = TRUE)
total_changes <- increases + decreases

cli_ul(c(
  paste("Increases across maturities:", increases, "out of", total_changes),
  paste("Decreases across maturities:", decreases, "out of", total_changes),
  paste("Monotonicity ratio:", round(increases / total_changes, 3))
))

# Correlation analysis between components
cli_h2("Component Correlations")

cor_c_k <- cor(c_hat_values, k_hat_values, use = "complete.obs")
cor_c_vb <- cor(c_hat_values, variance_bounds, use = "complete.obs")
cor_k_vb <- cor(k_hat_values, variance_bounds, use = "complete.obs")

cor_df <- data.frame(
  Component_1 = c("ĉ", "ĉ", "k̂"),
  Component_2 = c("k̂", "Variance Bound", "Variance Bound"),
  Correlation = c(cor_c_k, cor_c_vb, cor_k_vb)
) %>%
  gt() %>%
  tab_header(title = "Component Correlations") %>%
  fmt_number(columns = Correlation, decimals = 3) %>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = Correlation,
      rows = abs(Correlation) > 0.7
    )
  )

print(cor_df)

# Relative contribution analysis
cli_h2("Relative Contributions to Variance Bounds")

# Normalize components to see relative contributions
c_hat_normalized <- c_hat_values / max(c_hat_values, na.rm = TRUE)
k_hat_normalized <- k_hat_values / max(k_hat_values, na.rm = TRUE)

contrib_df <- data.frame(
  Maturity = maturities,
  c_hat_normalized = c_hat_normalized,
  k_hat_normalized = k_hat_normalized,
  c_hat_contribution = c_hat_values / variance_bounds,
  k_hat_contribution = k_hat_values / variance_bounds
) %>%
  gt() %>%
  tab_header(
    title = "Normalized Components and Contributions",
    subtitle = "Relative importance of ĉ and k̂ components"
  ) %>%
  fmt_number(
    columns = -Maturity,
    decimals = 3
  ) %>%
  cols_label(
    c_hat_normalized = "ĉ (normalized)",
    k_hat_normalized = "k̂ (normalized)",
    c_hat_contribution = "ĉ / VB",
    k_hat_contribution = "k̂ / VB"
  )

print(contrib_df)

# Create output directory
output_dir <- file.path(OUTPUT_DIR, "temp/variance_bounds")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save detailed results
variance_bounds_results <- list(
  variance_bounds_df = variance_bounds_df,
  summary_statistics = list(
    mean = mean(variance_bounds, na.rm = TRUE),
    median = median(variance_bounds, na.rm = TRUE),
    min = min(variance_bounds, na.rm = TRUE),
    max = max(variance_bounds, na.rm = TRUE),
    sd = sd(variance_bounds, na.rm = TRUE)
  ),
  correlations = list(
    c_hat_k_hat = cor_c_k,
    c_hat_variance_bound = cor_c_vb,
    k_hat_variance_bound = cor_k_vb
  ),
  monotonicity = list(
    increases = increases,
    decreases = decreases,
    total_changes = total_changes,
    monotonicity_ratio = increases / total_changes
  )
)

saveRDS(variance_bounds_results, file.path(output_dir, "variance_bounds_results.rds"))

# Save CSV for external use
write.csv(variance_bounds_df, file.path(output_dir, "variance_bounds.csv"), row.names = FALSE)

cli_h2("Economic Interpretation")

# Provide economic interpretation
max_vb_idx <- which.max(variance_bounds)
min_vb_idx <- which.min(variance_bounds[variance_bounds > 0]) # Exclude maturity 1 if it's 0

cli_ul(c(
  paste(
    "Highest variance bound:", round(variance_bounds[max_vb_idx], 6),
    "at maturity", maturities[max_vb_idx]
  ),
  paste(
    "Lowest positive variance bound:", round(variance_bounds[min_vb_idx], 6),
    "at maturity", maturities[min_vb_idx]
  ),
  paste("Variance bound range:", round(max(variance_bounds) - min(variance_bounds[variance_bounds > 0]), 6))
))

if (variance_bounds[1] == 0) {
  cli_alert_info("Variance bound for maturity 1 is zero by construction (no forecast error)")
}

cli_alert_success("Variance bounds computation completed successfully!")
cli_alert_info("Results saved to: {.path {output_dir}}")
