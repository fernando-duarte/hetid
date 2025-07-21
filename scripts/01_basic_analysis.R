# =============================================================================
# Basic Heteroskedasticity Identification Analysis
# =============================================================================
# This script performs the core analysis of computing gamma1 roots for all
# combinations of principal components and maturities.

# -----------------------------------------------------------------------------
# USER PARAMETERS - MODIFY THESE VALUES AS NEEDED
# -----------------------------------------------------------------------------
J <- 4 # Number of principal components to use (1-6)
tau <- 0.5 # Quantile parameter (between 0 and 1)
save_plots <- TRUE # Whether to save plots as PDF files
output_dir <- "scripts/output/01_basic_analysis" # Output directory

# -----------------------------------------------------------------------------
# SETUP
# -----------------------------------------------------------------------------
# Load required packages
library(hetid)
library(ggplot2)
library(tidyr)
library(dplyr)

# Set output directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# ANALYSIS
# -----------------------------------------------------------------------------
cat("========================================\n")
cat("Basic Heteroskedasticity Identification Analysis\n")
cat("========================================\n")
cat(sprintf("Number of PCs (J): %d\n", J))
cat(sprintf("Tau parameter: %.3f\n", tau))
cat(sprintf("Output directory: %s\n\n", output_dir))

# Load data
cat("Loading data...\n")
data("variables")
acm_data <- extract_acm_data(
  data_types = c("yields", "term_premia"),
  frequency = "quarterly"
)
yields_data <- acm_data[, grep("^y", names(acm_data))]
tp_data <- acm_data[, grep("^tp", names(acm_data))]

# Maximum maturity (limited to 9 because SDF innovations need y(i+1))
I <- 9

# Compute W1 (consumption growth residuals)
cat("\nComputing W1 residuals...\n")
res_y1 <- compute_w1_residuals(n_pcs = J)
W1 <- res_y1$residuals
cat(sprintf("  Observations: %d\n", length(W1)))
cat(sprintf("  R-squared: %.4f\n", res_y1$r_squared))

# Compute W2i for all maturities
cat("\nComputing W2 residuals for all maturities...\n")
res_y2 <- compute_w2_residuals(
  yields = yields_data,
  term_premia = tp_data,
  maturities = 1:I,
  n_pcs = J,
  pcs = as.matrix(variables[, paste0("pc", 1:J)])
)
W2_list <- res_y2$residuals

# Report R-squared values
cat("\nR-squared for W2 regressions:\n")
for (i in 1:I) {
  cat(sprintf("  Maturity %d: %.4f\n", i, res_y2$r_squared[i]))
}

# Align data
PC_data <- as.matrix(variables[, paste0("pc", 1:J)])
n_obs <- min(length(W1), nrow(PC_data), length(W2_list[[1]]))
W1_aligned <- W1[1:n_obs]
PC_aligned <- PC_data[1:n_obs, ]

# -----------------------------------------------------------------------------
# COMPUTE ROOTS
# -----------------------------------------------------------------------------
cat("\n========================================\n")
cat("COMPUTING GAMMA1 ROOTS\n")
cat("========================================\n\n")

# Initialize results matrices
results_matrix_root1 <- matrix(NA, nrow = J, ncol = I)
results_matrix_root2 <- matrix(NA, nrow = J, ncol = I)
rownames(results_matrix_root1) <- paste0("PC", 1:J)
colnames(results_matrix_root1) <- paste0("Mat", 1:I)
rownames(results_matrix_root2) <- paste0("PC", 1:J)
colnames(results_matrix_root2) <- paste0("Mat", 1:I)

# Compute roots for all combinations
for (j in 1:J) {
  for (i in 1:I) {
    pc_j <- PC_aligned[, j]
    w2_i <- W2_list[[i]][1:n_obs]

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

    if (!is.null(result$error)) {
      cat(sprintf("PC%d, Maturity %d: ERROR - %s\n", j, i, result$error))
    } else {
      results_matrix_root1[j, i] <- Re(result$roots[1])
      results_matrix_root2[j, i] <- Re(result$roots[2])

      if (is.complex(result$roots[1])) {
        cat(sprintf(
          "PC%d, Maturity %d: gamma1(1) = %.4f %+.4fi, gamma1(2) = %.4f %+.4fi\n",
          j, i,
          Re(result$roots[1]), Im(result$roots[1]),
          Re(result$roots[2]), Im(result$roots[2])
        ))
      } else {
        cat(sprintf(
          "PC%d, Maturity %d: gamma1(1) = %.4f, gamma1(2) = %.4f\n",
          j, i, result$roots[1], result$roots[2]
        ))
      }
    }
  }
  cat("\n")
}

# Display summary tables
cat("========================================\n")
cat("SUMMARY: Real parts of gamma1(1)\n")
cat("========================================\n")
print(round(results_matrix_root1, 4))

cat("\n========================================\n")
cat("SUMMARY: Real parts of gamma1(2)\n")
cat("========================================\n")
print(round(results_matrix_root2, 4))

# -----------------------------------------------------------------------------
# VISUALIZATION
# -----------------------------------------------------------------------------
cat("\n\nCreating visualizations...\n")

# Reshape data for plotting
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

# Create plots
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

# Display plots
print(p1)
print(p2)

# Save plots if requested
if (save_plots) {
  ggsave(file.path(output_dir, "gamma1_root1.pdf"), p1, width = 8, height = 6)
  ggsave(file.path(output_dir, "gamma1_root2.pdf"), p2, width = 8, height = 6)
  cat(sprintf("\nPlots saved to %s\n", output_dir))
}

# -----------------------------------------------------------------------------
# SAVE RESULTS
# -----------------------------------------------------------------------------
# Save numerical results
results <- list(
  parameters = list(J = J, tau = tau),
  W1_rsquared = res_y1$r_squared,
  W2_rsquared = res_y2$r_squared,
  root1_matrix = results_matrix_root1,
  root2_matrix = results_matrix_root2,
  n_observations = n_obs
)

saveRDS(results, file.path(output_dir, "basic_analysis_results.rds"))
cat(sprintf(
  "\nNumerical results saved to %s\n",
  file.path(output_dir, "basic_analysis_results.rds")
))

cat("\nAnalysis complete!\n")
