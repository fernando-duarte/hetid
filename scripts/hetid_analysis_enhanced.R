#!/usr/bin/env Rscript

# Enhanced analysis script with heteroskedasticity tests and correlations
# This script can be run interactively in RStudio using "Source" or "Run All"

# Check if hetid package is installed, if not, install it
if (!requireNamespace("hetid", quietly = TRUE)) {
  cat("Installing hetid package...\n")

  # Check if we're in the package directory
  if (file.exists("DESCRIPTION") && file.exists("R/hetid-package.R")) {
    # We're in the package directory, install from here
    if (!requireNamespace("devtools", quietly = TRUE)) {
      install.packages("devtools")
    }
    devtools::install(".", force = TRUE, quiet = FALSE)
  } else {
    # Try to install from GitHub
    if (!requireNamespace("pak", quietly = TRUE)) {
      install.packages("pak")
    }
    pak::pak("fernando-duarte/hetid")
  }
}

# Load required packages
library(hetid)

# Install and load visualization packages if needed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  cat("Installing ggplot2 for visualizations...\n")
  install.packages("ggplot2")
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  cat("Installing tidyr for data manipulation...\n")
  install.packages("tidyr")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  cat("Installing dplyr for data manipulation...\n")
  install.packages("dplyr")
}

library(ggplot2)
library(tidyr)
library(dplyr)

# Set user parameters
# Check if running interactively
if (interactive()) {
  cat("\n========================================\n")
  cat("INTERACTIVE PARAMETER SELECTION\n")
  cat("========================================\n\n")

  # Get J parameter
  J_input <- readline(prompt = "Enter number of PCs to use (1-6) [default: 4]: ")
  if (J_input == "") {
    J <- 4
  } else {
    J <- as.integer(J_input)
  }

  # Get tau parameter
  tau_input <- readline(prompt = "Enter tau parameter (0-1) [default: 0.5]: ")
  if (tau_input == "") {
    tau <- 0.5
  } else {
    tau <- as.numeric(tau_input)
  }
} else {
  # Default values for non-interactive mode
  J <- 4 # Number of PCs to use (can be 1-6)
  tau <- 0.5 # Parameter between 0 and 1
}

# Validate inputs
if (!J %in% 1:6) {
  stop("J must be between 1 and 6")
}
if (tau < 0 || tau > 1) {
  stop("tau must be between 0 and 1")
}

cat("========================================\n")
cat("Enhanced Heteroskedasticity Identification Analysis\n")
cat("========================================\n")
cat(sprintf("Number of PCs (J): %d\n", J))
cat(sprintf("Tau parameter: %.3f\n", tau))
cat("\n")

# Load variables data (for PCs and consumption growth)
data("variables")

# Download and load ACM data if needed
if (!file.exists(system.file("extdata", "acm_term_premia.csv", package = "hetid"))) {
  cat("Downloading ACM data...\n")
  download_term_premia(quiet = FALSE)
}

# Extract ACM data
cat("Loading ACM data...\n")
acm_data <- extract_acm_data(
  data_types = c("yields", "term_premia"),
  frequency = "quarterly"
)
yields_data <- acm_data[, grep("^y", names(acm_data))]
tp_data <- acm_data[, grep("^tp", names(acm_data))]

# Determine maximum maturity I
# Note: ACM data has maturities 1-10, but n_hat(i) needs y(i+1),
# so we can only compute SDF innovations for maturities 1-9
I <- 9
cat(sprintf("Maximum maturity (I): %d years\n", I))
cat("(Limited to 9 because SDF innovations need y(i+1))\n\n")

# Step 1: Compute W1 (reduced form residual for consumption growth)
cat("Computing W1 (consumption growth residuals)...\n")
res_y1 <- compute_w1_residuals(n_pcs = J)
W1 <- res_y1$residuals
cat(sprintf("  Number of observations: %d\n", length(W1)))
cat(sprintf("  R-squared: %.4f\n", res_y1$r_squared))
cat("\n")

# Step 2: Compute W2i for all maturities i=1,...,I
cat("Computing W2i (SDF innovation residuals) for all maturities...\n")
res_y2 <- compute_w2_residuals(
  yields = yields_data,
  term_premia = tp_data,
  maturities = 1:I,
  n_pcs = J,
  pcs = as.matrix(variables[, paste0("pc", 1:J)])
)

# Extract all W2i
W2_list <- res_y2$residuals

# Report R-squared for each maturity
cat("\nR-squared values for W2i regressions:\n")
for (i in 1:I) {
  cat(sprintf("  Maturity %2d: R² = %.4f\n", i, res_y2$r_squared[i]))
}
cat("\n")

# Step 3: Extract PCs
PC_data <- as.matrix(variables[, paste0("pc", 1:J)])

# Ensure all data has same length (use minimum)
n_obs <- min(length(W1), nrow(PC_data), length(W2_list[[1]]))
W1_aligned <- W1[1:n_obs]
PC_aligned <- PC_data[1:n_obs, ]

# ==============================================================================
# NEW SECTION 1: Test heteroskedasticity of W2i
# ==============================================================================
cat("========================================\n")
cat("HETEROSKEDASTICITY TESTS FOR W2i\n")
cat("========================================\n\n")

# Function to perform Breusch-Pagan test manually
breusch_pagan_test <- function(residuals, regressors) {
  n <- length(residuals)

  # Square the residuals
  res_squared <- residuals^2

  # Regress squared residuals on regressors
  aux_reg <- lm(res_squared ~ regressors)

  # Get R-squared from auxiliary regression
  r_squared <- summary(aux_reg)$r.squared

  # Test statistic: n * R²
  lm_stat <- n * r_squared

  # Degrees of freedom = number of regressors (excluding intercept)
  df <- ncol(as.matrix(regressors))

  # P-value from chi-squared distribution
  p_value <- 1 - pchisq(lm_stat, df)

  list(
    statistic = lm_stat,
    df = df,
    p.value = p_value,
    r.squared = r_squared
  )
}

# Store test results
hetero_test_results <- matrix(NA, nrow = I, ncol = 4)
colnames(hetero_test_results) <- c("LM_statistic", "df", "p_value", "R_squared")
rownames(hetero_test_results) <- paste0("Maturity_", 1:I)

# Test heteroskedasticity for each W2i
for (i in 1:I) {
  w2_i <- W2_list[[i]][1:n_obs]

  # Test using PCs as regressors
  test_result <- breusch_pagan_test(w2_i, PC_aligned)

  hetero_test_results[i, "LM_statistic"] <- test_result$statistic
  hetero_test_results[i, "df"] <- test_result$df
  hetero_test_results[i, "p_value"] <- test_result$p.value
  hetero_test_results[i, "R_squared"] <- test_result$r.squared

  cat(sprintf(
    "Maturity %2d: LM = %7.3f, p-value = %.4f %s\n",
    i,
    test_result$statistic,
    test_result$p.value,
    ifelse(test_result$p.value < 0.05, "***",
      ifelse(test_result$p.value < 0.10, "**", "")
    )
  ))
}

cat("\n*** indicates p < 0.05, ** indicates p < 0.10\n")
cat("\nInterpretation: Low p-values indicate significant heteroskedasticity\n\n")

# ==============================================================================
# NEW SECTION 2: Calculate |corr(PC_j, W2i²)| for all combinations
# ==============================================================================
cat("========================================\n")
cat("ABSOLUTE CORRELATIONS: |corr(PC_j, W2_i^2)|\n")
cat("========================================\n\n")

# Create correlation matrix
corr_matrix <- matrix(NA, nrow = J, ncol = I)
rownames(corr_matrix) <- paste0("PC", 1:J)
colnames(corr_matrix) <- paste0("Mat", 1:I)

# Calculate correlations
for (j in 1:J) {
  for (i in 1:I) {
    pc_j <- PC_aligned[, j]
    w2_i <- W2_list[[i]][1:n_obs]
    w2_i_squared <- w2_i^2

    # Calculate correlation
    correlation <- cor(pc_j, w2_i_squared, use = "complete.obs")

    # Store absolute value
    corr_matrix[j, i] <- abs(correlation)
  }
}

# Display correlation matrix
cat("Absolute correlations between PC_j and W2_i^2:\n\n")
print(round(corr_matrix, 4))

# Find strongest correlations
cat("\nStrongest correlations (top 10):\n")
corr_df <- expand.grid(PC = 1:J, Maturity = 1:I)
corr_df$AbsCorr <- as.vector(corr_matrix)
corr_df <- corr_df[order(corr_df$AbsCorr, decreasing = TRUE), ]
for (idx in 1:min(10, nrow(corr_df))) {
  cat(sprintf(
    "  PC%d - Maturity %2d: |corr| = %.4f\n",
    corr_df$PC[idx],
    corr_df$Maturity[idx],
    corr_df$AbsCorr[idx]
  ))
}

# ==============================================================================
# ORIGINAL SECTION: Compute roots for all combinations of j and i
# ==============================================================================
cat("\n========================================\n")
cat("GAMMA_1 ROOTS FOR ALL (j,i) COMBINATIONS\n")
cat("========================================\n\n")

# Create storage for results
results_matrix_root1 <- matrix(NA, nrow = J, ncol = I)
results_matrix_root2 <- matrix(NA, nrow = J, ncol = I)
rownames(results_matrix_root1) <- paste0("PC", 1:J)
colnames(results_matrix_root1) <- paste0("Mat", 1:I)
rownames(results_matrix_root2) <- paste0("PC", 1:J)
colnames(results_matrix_root2) <- paste0("Mat", 1:I)

# Loop through all combinations
for (j in 1:J) {
  for (i in 1:I) {
    # Get PC_j and W2_i
    pc_j <- PC_aligned[, j]
    w2_i <- W2_list[[i]][1:n_obs]

    # Solve quadratic
    result <- tryCatch(
      {
        solve_gamma_quadratic(
          pc_j = pc_j,
          w1 = W1_aligned,
          w2 = w2_i,
          tau = tau,
          use_t_minus_1 = TRUE
        )
      },
      error = function(e) {
        list(roots = c(NA, NA), error = e$message)
      }
    )

    # Store results
    if (!is.null(result$error)) {
      cat(sprintf("PC%d, Maturity %d: ERROR - %s\n", j, i, result$error))
    } else {
      results_matrix_root1[j, i] <- Re(result$roots[1])
      results_matrix_root2[j, i] <- Re(result$roots[2])

      # Display results
      if (is.complex(result$roots[1])) {
        cat(sprintf(
          "PC%d, Maturity %2d: gamma1(1) = %8.4f %+.4fi, gamma1(2) = %8.4f %+.4fi\n",
          j, i,
          Re(result$roots[1]), Im(result$roots[1]),
          Re(result$roots[2]), Im(result$roots[2])
        ))
      } else {
        cat(sprintf(
          "PC%d, Maturity %2d: gamma1(1) = %8.4f, gamma1(2) = %8.4f\n",
          j, i, result$roots[1], result$roots[2]
        ))
      }
    }
  }
  cat("\n")
}

# Display summary tables
cat("\n========================================\n")
cat("SUMMARY: Real parts of gamma1(1) (first root)\n")
cat("========================================\n")
print(round(results_matrix_root1, 4))

cat("\n========================================\n")
cat("SUMMARY: Real parts of gamma1(2) (second root)\n")
cat("========================================\n")
print(round(results_matrix_root2, 4))

# ==============================================================================
# NEW SECTION 3: Create enhanced visualization
# ==============================================================================
cat("\nCreating enhanced visualizations...\n")

# 1. Heteroskedasticity test p-values plot
hetero_df <- data.frame(
  Maturity = 1:I,
  p_value = hetero_test_results[, "p_value"]
)

p_hetero <- ggplot(hetero_df, aes(x = Maturity, y = -log10(p_value))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
  geom_hline(yintercept = -log10(0.10), linetype = "dashed", color = "orange") +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Heteroskedasticity Tests for W2i",
    x = "Maturity (years)",
    y = "-log10(p-value)",
    caption = "Red line: p=0.05, Orange line: p=0.10"
  ) +
  theme_minimal()

# 2. Correlation heatmap
corr_long <- expand.grid(PC = factor(1:J), Maturity = 1:I)
corr_long$AbsCorr <- as.vector(corr_matrix)

p_corr <- ggplot(corr_long, aes(x = Maturity, y = PC, fill = AbsCorr)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.3f", AbsCorr)), size = 3) +
  scale_fill_gradient(
    low = "white", high = "darkred",
    name = "|Correlation|",
    limits = c(0, max(corr_matrix, na.rm = TRUE))
  ) +
  scale_x_continuous(breaks = 1:I) +
  labs(
    title = expression(paste("Absolute Correlations: |corr(", PC[j], ", ", W[list(2, i)]^2, ")|")),
    x = "Maturity (years)",
    y = "Principal Component"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# 3. Original root plots
df_root1 <- as.data.frame(results_matrix_root1) %>%
  mutate(PC = factor(1:J)) %>%
  pivot_longer(cols = -PC, names_to = "Maturity", values_to = "Root1") %>%
  mutate(Maturity = as.numeric(gsub("Mat", "", Maturity)))

df_root2 <- as.data.frame(results_matrix_root2) %>%
  mutate(PC = factor(1:J)) %>%
  pivot_longer(cols = -PC, names_to = "Maturity", values_to = "Root2") %>%
  mutate(Maturity = as.numeric(gsub("Mat", "", Maturity)))

df_combined <- df_root1 %>%
  left_join(df_root2, by = c("PC", "Maturity"))

p1 <- ggplot(df_combined, aes(x = Maturity)) +
  geom_line(aes(y = Root1, color = PC), linewidth = 1.2) +
  geom_point(aes(y = Root1, color = PC), size = 2) +
  scale_x_continuous(breaks = 1:I) +
  labs(
    title = sprintf("First Root of Gamma1 Quadratic (tau = %.2f)", tau),
    x = "Maturity (years)",
    color = "PC"
  ) +
  ylab(expression(gamma[1]^{
    (1)
  })) +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 <- ggplot(df_combined, aes(x = Maturity)) +
  geom_line(aes(y = Root2, color = PC), linewidth = 1.2) +
  geom_point(aes(y = Root2, color = PC), size = 2) +
  scale_x_continuous(breaks = 1:I) +
  labs(
    title = sprintf("Second Root of Gamma1 Quadratic (tau = %.2f)", tau),
    x = "Maturity (years)",
    color = "PC"
  ) +
  ylab(expression(gamma[1]^{
    (2)
  })) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display all plots in RStudio
print(p_hetero)
print(p_corr)
print(p1)
print(p2)

# Ask about saving plots (only in interactive mode)
if (interactive()) {
  save_plots <- readline(prompt = "\nSave plots as PDF files? (y/n): ")
} else {
  save_plots <- "n"
}

if (tolower(save_plots) %in% c("y", "yes")) {
  output_dir <- "scripts/output/hetid_enhanced"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  ggsave(file.path(output_dir, "hetero_test_pvalues.pdf"), p_hetero, width = 8, height = 6)
  ggsave(file.path(output_dir, "correlation_heatmap.pdf"), p_corr, width = 10, height = 6)
  ggsave(file.path(output_dir, "gamma1_roots_plot1.pdf"), p1, width = 8, height = 6)
  ggsave(file.path(output_dir, "gamma1_roots_plot2.pdf"), p2, width = 8, height = 6)
  cat("\nPlots saved as PDF files in:", output_dir, "\n")
}

# ==============================================================================
# SUMMARY REPORT
# ==============================================================================
cat("\n========================================\n")
cat("ANALYSIS SUMMARY\n")
cat("========================================\n")

# Count significant heteroskedasticity tests
n_sig_05 <- sum(hetero_test_results[, "p_value"] < 0.05, na.rm = TRUE)
n_sig_10 <- sum(hetero_test_results[, "p_value"] < 0.10, na.rm = TRUE)

cat(sprintf("\nHeteroskedasticity tests:\n"))
cat(sprintf("  - Significant at 5%% level: %d out of %d maturities\n", n_sig_05, I))
cat(sprintf("  - Significant at 10%% level: %d out of %d maturities\n", n_sig_10, I))

# Report average absolute correlations
avg_corr_by_maturity <- colMeans(corr_matrix, na.rm = TRUE)
avg_corr_by_pc <- rowMeans(corr_matrix, na.rm = TRUE)

cat(sprintf("\nAverage |corr(PC_j, W2_i^2)|:\n"))
cat("  By PC:\n")
for (j in 1:J) {
  cat(sprintf("    PC%d: %.4f\n", j, avg_corr_by_pc[j]))
}
cat("  By Maturity:\n")
cat(sprintf("    Short (1-3 years): %.4f\n", mean(avg_corr_by_maturity[1:3])))
cat(sprintf("    Medium (4-7 years): %.4f\n", mean(avg_corr_by_maturity[4:7])))
cat(sprintf("    Long (8-10 years): %.4f\n", mean(avg_corr_by_maturity[8:10])))

cat("\nAnalysis complete!\n")

# ==============================================================================
# NEW SECTION 4: Optimize PC weights to minimize root distance
# ==============================================================================
cat("\n========================================\n")
cat("OPTIMIZING PC WEIGHTS TO MINIMIZE ROOT DISTANCE\n")
cat("========================================\n\n")

# Run optimization for all maturities
cat("Finding optimal linear combinations of PCs for each maturity...\n\n")

opt_results <- optimize_pc_weights_all_maturities(
  pc_matrix = PC_aligned,
  w1 = W1_aligned,
  w2_list = lapply(W2_list, function(w) w[1:n_obs]),
  tau = tau,
  use_t_minus_1 = TRUE,
  parallel = FALSE
)

# Display optimization results
cat("Optimization Results:\n")
cat("====================\n\n")

# Show results for each maturity
for (i in 1:I) {
  row <- opt_results[i, ]

  cat(sprintf("Maturity %2d:\n", i))

  if (row$is_complex) {
    cat("  Status: Complex roots (no real solution)\n")
  } else {
    cat(sprintf("  Root 1: %.4f\n", row$root1))
    cat(sprintf("  Root 2: %.4f\n", row$root2))
    cat(sprintf("  Root distance: %.4f\n", row$root_distance))
  }

  # Show weights
  cat("  Optimal weights: ")
  weight_cols <- grep("^weight_pc", names(row))
  weights <- as.numeric(row[weight_cols])
  for (j in 1:J) {
    cat(sprintf("PC%d=%.3f", j, weights[j]))
    if (j < J) cat(", ")
  }
  cat("\n")

  cat(sprintf(
    "  Convergence: %s\n\n",
    ifelse(row$convergence == 0, "Success", "Failed")
  ))
}

# Find best maturity (smallest root distance among real roots)
real_roots <- opt_results[!opt_results$is_complex, ]
if (nrow(real_roots) > 0) {
  best_idx <- which.min(real_roots$root_distance)
  best_maturity <- real_roots$maturity[best_idx]

  cat("\n========================================\n")
  cat("BEST LINEAR COMBINATION\n")
  cat("========================================\n\n")

  cat(sprintf("Maturity with smallest root distance: %d years\n", best_maturity))
  cat(sprintf("Root distance: %.4f\n", real_roots$root_distance[best_idx]))
  cat(sprintf(
    "Roots: gamma1(1) = %.4f, gamma1(2) = %.4f\n",
    real_roots$root1[best_idx], real_roots$root2[best_idx]
  ))

  cat("\nOptimal PC weights:\n")
  best_weights <- as.numeric(real_roots[best_idx, grep("^weight_pc", names(real_roots))])
  for (j in 1:J) {
    cat(sprintf("  PC%d: %.4f\n", j, best_weights[j]))
  }

  # Verify the linear combination has unit variance
  full_results <- attr(opt_results, "full_results")
  best_full <- full_results[[best_maturity]]
  cat(sprintf(
    "\nLinear combination variance: %.4f (should be 1.0)\n",
    var(best_full$linear_comb, na.rm = TRUE)
  ))
} else {
  cat("\nNo maturities produced real roots.\n")
}

# Create visualization for optimization results
plot_data <- opt_results[!opt_results$is_complex, ]

if (nrow(plot_data) > 0) {
  p_opt <- ggplot(plot_data, aes(x = maturity, y = root_distance)) +
    geom_line(linewidth = 1.2, color = "darkblue") +
    geom_point(size = 3, color = "darkblue") +
    geom_point(
      data = plot_data[which.min(plot_data$root_distance), ],
      size = 5, color = "red"
    ) +
    scale_x_continuous(breaks = 1:I) +
    labs(
      title = "Root Distance by Maturity (Optimized PC Weights)",
      x = "Maturity (years)",
      y = "Distance between roots",
      caption = "Red point indicates minimum distance"
    ) +
    theme_minimal()

  print(p_opt)

  if (tolower(save_plots) %in% c("y", "yes")) {
    ggsave(file.path(output_dir, "optimal_root_distances.pdf"), p_opt, width = 8, height = 6)
    cat(sprintf("Optimization plot saved in %s\n", output_dir))
  }
}

cat("\nEnhanced analysis with optimization complete!\n")
cat("\n========================================\n")
cat("To re-run with different parameters, simply source this script again.\n")
if (interactive()) {
  cat("All plots are displayed in the Plots pane.\n")
}
