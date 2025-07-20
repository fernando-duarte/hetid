# =============================================================================
# Visualization Gallery
# =============================================================================
# This script loads previously saved results and creates a comprehensive set
# of visualizations for the heteroskedasticity identification analysis.

# -----------------------------------------------------------------------------
# USER PARAMETERS - MODIFY THESE VALUES AS NEEDED
# -----------------------------------------------------------------------------
results_file <- "scripts/output/01_basic_analysis/basic_analysis_results.rds" # Path to saved results from 01_basic_analysis.R
hetero_file <- "scripts/output/02_heteroskedasticity/heteroskedasticity_test_results.rds" # Path from 02_heteroskedasticity_tests.R
opt_file <- "scripts/output/03_optimization/optimization_results.rds" # Path from 03_optimization_analysis.R
save_plots <- TRUE # Whether to save plots as PDF files
output_dir <- "scripts/output/04_visualization" # Directory for plot files
plot_width <- 10 # Width of saved plots in inches
plot_height <- 6 # Height of saved plots in inches

# -----------------------------------------------------------------------------
# SETUP
# -----------------------------------------------------------------------------
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

if (!dir.exists(output_dir) && save_plots) {
  dir.create(output_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# LOAD RESULTS
# -----------------------------------------------------------------------------
cat("========================================\n")
cat("Visualization Gallery\n")
cat("========================================\n\n")

# Check if result files exist
if (!file.exists(results_file)) {
  stop("Results file not found. Please run 01_basic_analysis.R first.")
}

cat("Loading analysis results...\n")
results <- readRDS(results_file)

# Load additional results if available
has_hetero <- file.exists(hetero_file)
has_opt <- file.exists(opt_file)

if (has_hetero) {
  hetero_results <- readRDS(hetero_file)
  cat("  Loaded heteroskedasticity test results\n")
}

if (has_opt) {
  opt_results <- readRDS(opt_file)
  cat("  Loaded optimization results\n")
}

# Extract parameters
J <- results$parameters$J
tau <- results$parameters$tau
I <- ncol(results$root1_matrix)

# -----------------------------------------------------------------------------
# 1. ROOT COMPARISON PLOTS
# -----------------------------------------------------------------------------
cat("\nCreating root comparison plots...\n")

# Prepare data
df_roots <- expand.grid(PC = 1:J, Maturity = 1:I)
df_roots$Root1 <- as.vector(t(results$root1_matrix))
df_roots$Root2 <- as.vector(t(results$root2_matrix))
df_roots$PC <- factor(df_roots$PC)

# Combined roots plot
p_roots_combined <- ggplot(df_roots, aes(x = Maturity)) +
  geom_line(aes(y = Root1, color = PC, linetype = "First Root"),
    linewidth = 1
  ) +
  geom_line(aes(y = Root2, color = PC, linetype = "Second Root"),
    linewidth = 1
  ) +
  geom_point(aes(y = Root1, color = PC), size = 2) +
  geom_point(aes(y = Root2, color = PC), size = 2, shape = 17) +
  scale_x_continuous(breaks = 1:I) +
  scale_linetype_manual(values = c(
    "First Root" = "solid",
    "Second Root" = "dashed"
  )) +
  labs(
    title = sprintf("Both Gamma1 Roots by PC and Maturity (tau = %.2f)", tau),
    x = "Maturity (years)",
    y = expression(gamma[1]),
    color = "PC",
    linetype = "Root"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_roots_combined)

# Root distance plot
df_roots$Distance <- abs(df_roots$Root1 - df_roots$Root2)

p_distance <- ggplot(df_roots, aes(x = Maturity, y = Distance, color = PC)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:I) +
  labs(
    title = "Distance Between Roots by PC and Maturity",
    x = "Maturity (years)",
    y = "Root Distance",
    color = "PC"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_distance)

# -----------------------------------------------------------------------------
# 2. R-SQUARED VISUALIZATION
# -----------------------------------------------------------------------------
cat("Creating R-squared plots...\n")

# W2 R-squared by maturity
df_rsq <- data.frame(
  Maturity = 1:I,
  R_squared = results$W2_rsquared
)

p_rsq <- ggplot(df_rsq, aes(x = Maturity, y = R_squared)) +
  geom_line(linewidth = 1.2, color = "darkgreen") +
  geom_point(size = 3, color = "darkgreen") +
  geom_hline(
    yintercept = results$W1_rsquared,
    linetype = "dashed", color = "darkred"
  ) +
  scale_x_continuous(breaks = 1:I) +
  scale_y_continuous(limits = c(0, max(c(
    results$W1_rsquared,
    results$W2_rsquared
  )) * 1.1)) +
  labs(
    title = "R-squared Values for Reduced Form Regressions",
    x = "Maturity (years)",
    y = expression(R^2),
    caption = sprintf("Red dashed line: W1 R² = %.3f", results$W1_rsquared)
  ) +
  theme_minimal()

print(p_rsq)

# -----------------------------------------------------------------------------
# 3. HETEROSKEDASTICITY VISUALIZATION (if available)
# -----------------------------------------------------------------------------
if (has_hetero) {
  cat("Creating heteroskedasticity visualization...\n")

  # Combine test results with correlations
  df_hetero <- data.frame(
    Maturity = 1:I,
    p_value = hetero_results$heteroskedasticity_tests[, "p_value"],
    avg_corr = hetero_results$summary$avg_corr_by_maturity
  )

  # Dual axis plot
  p_hetero_dual <- ggplot(df_hetero, aes(x = Maturity)) +
    geom_bar(aes(y = -log10(p_value)),
      stat = "identity",
      fill = "steelblue", alpha = 0.7
    ) +
    geom_line(aes(y = avg_corr * max(-log10(p_value)) / max(avg_corr)),
      linewidth = 1.2, color = "darkred"
    ) +
    geom_point(aes(y = avg_corr * max(-log10(p_value)) / max(avg_corr)),
      size = 3, color = "darkred"
    ) +
    scale_x_continuous(breaks = 1:I) +
    scale_y_continuous(
      name = "-log10(p-value)",
      sec.axis = sec_axis(
        ~ . * max(df_hetero$avg_corr) /
          max(-log10(df_hetero$p_value)),
        name = "Average |Correlation|"
      )
    ) +
    labs(
      title = "Heteroskedasticity Tests and Average Correlations",
      x = "Maturity (years)"
    ) +
    theme_minimal() +
    theme(
      axis.title.y = element_text(color = "steelblue"),
      axis.title.y.right = element_text(color = "darkred")
    )

  print(p_hetero_dual)
}

# -----------------------------------------------------------------------------
# 4. OPTIMIZATION VISUALIZATION (if available)
# -----------------------------------------------------------------------------
if (has_opt && !is.null(opt_results$best_maturity)) {
  cat("Creating optimization visualization...\n")

  # Weight evolution plot
  opt_table <- opt_results$optimization_table
  weight_evolution <- opt_table %>%
    select(maturity, starts_with("weight_pc")) %>%
    pivot_longer(
      cols = starts_with("weight_pc"),
      names_to = "PC",
      values_to = "Weight"
    ) %>%
    mutate(PC = gsub("weight_pc", "", PC))

  p_weight_evolution <- ggplot(
    weight_evolution,
    aes(x = maturity, y = Weight, color = PC)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_vline(
      xintercept = opt_results$best_maturity,
      linetype = "dashed", alpha = 0.5
    ) +
    scale_x_continuous(breaks = 1:I) +
    labs(
      title = "Evolution of Optimal PC Weights by Maturity",
      x = "Maturity (years)",
      y = "Optimal Weight",
      caption = sprintf(
        "Vertical line: Best maturity = %d",
        opt_results$best_maturity
      )
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p_weight_evolution)
}

# -----------------------------------------------------------------------------
# 5. SUMMARY DASHBOARD
# -----------------------------------------------------------------------------
cat("Creating summary dashboard...\n")

# Create text summary
summary_text <- paste0(
  "Analysis Summary\n",
  "================\n",
  sprintf("Principal Components: %d\n", J),
  sprintf("Tau parameter: %.2f\n", tau),
  sprintf("Observations: %d\n", results$n_observations),
  sprintf("W1 R-squared: %.3f\n", results$W1_rsquared),
  sprintf("Avg W2 R-squared: %.3f\n", mean(results$W2_rsquared))
)

if (has_hetero) {
  summary_text <- paste0(
    summary_text,
    sprintf("\nHeteroskedasticity:\n"),
    sprintf(
      "  Significant at 5%%: %d/%d\n",
      hetero_results$summary$n_sig_05, I
    )
  )
}

if (has_opt && !is.null(opt_results$best_maturity)) {
  summary_text <- paste0(
    summary_text,
    sprintf("\nOptimization:\n"),
    sprintf("  Best maturity: %d\n", opt_results$best_maturity),
    sprintf(
      "  Min root distance: %.3f\n",
      min(opt_results$optimization_table$root_distance, na.rm = TRUE)
    )
  )
}

# Create summary plot
p_summary <- ggplot() +
  annotate("text",
    x = 0.5, y = 0.5, label = summary_text,
    size = 4, hjust = 0.5, vjust = 0.5, family = "mono"
  ) +
  theme_void() +
  labs(title = "Analysis Summary") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

# -----------------------------------------------------------------------------
# SAVE PLOTS
# -----------------------------------------------------------------------------
if (save_plots) {
  cat(sprintf("\nSaving plots to %s...\n", output_dir))

  ggsave(file.path(output_dir, "roots_combined.pdf"),
    p_roots_combined,
    width = plot_width, height = plot_height
  )
  ggsave(file.path(output_dir, "root_distances.pdf"),
    p_distance,
    width = plot_width, height = plot_height
  )
  ggsave(file.path(output_dir, "r_squared_values.pdf"),
    p_rsq,
    width = plot_width, height = plot_height
  )

  if (has_hetero) {
    ggsave(file.path(output_dir, "heteroskedasticity_dual.pdf"),
      p_hetero_dual,
      width = plot_width, height = plot_height
    )
  }

  if (has_opt && !is.null(opt_results$best_maturity)) {
    ggsave(file.path(output_dir, "weight_evolution.pdf"),
      p_weight_evolution,
      width = plot_width, height = plot_height
    )
  }

  ggsave(file.path(output_dir, "summary_dashboard.pdf"),
    p_summary,
    width = 8, height = 6
  )

  cat("All plots saved successfully!\n")
}

cat("\nVisualization complete!\n")
