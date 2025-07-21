# =============================================================================
# PC Weight Optimization Analysis
# =============================================================================
# This script finds optimal linear combinations of principal components that
# minimize the distance between gamma1 roots for each maturity.

# -----------------------------------------------------------------------------
# USER PARAMETERS - MODIFY THESE VALUES AS NEEDED
# -----------------------------------------------------------------------------
J <- 4 # Number of principal components to use (1-6)
tau <- 0.5 # Quantile parameter (between 0 and 1)
penalty_complex <- 1e6 # Penalty for complex roots in optimization
max_iterations <- 1000 # Maximum iterations for optimization
save_plots <- TRUE # Whether to save plots as PDF files
output_dir <- "scripts/output/03_optimization" # Directory for output files

# -----------------------------------------------------------------------------
# SETUP
# -----------------------------------------------------------------------------
library(hetid)
library(ggplot2)
library(tidyr)
library(dplyr)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# DATA PREPARATION
# -----------------------------------------------------------------------------
cat("========================================\n")
cat("PC Weight Optimization Analysis\n")
cat("========================================\n")
cat(sprintf("Number of PCs: %d\n", J))
cat(sprintf("Tau parameter: %.3f\n", tau))
cat(sprintf("Penalty for complex roots: %.0e\n\n", penalty_complex))

# Load and prepare data
data("variables")
acm_data <- extract_acm_data(
  data_types = c("yields", "term_premia"),
  frequency = "quarterly"
)
yields_data <- acm_data[, grep("^y", names(acm_data))]
tp_data <- acm_data[, grep("^tp", names(acm_data))]

I <- 9 # Maximum maturity

# Compute residuals
cat("Computing residuals...\n")
res_y1 <- compute_w1_residuals(n_pcs = J)
W1 <- res_y1$residuals

res_y2 <- compute_w2_residuals(
  yields = yields_data,
  term_premia = tp_data,
  maturities = 1:I,
  n_pcs = J,
  pcs = as.matrix(variables[, paste0("pc", 1:J)])
)
W2_list <- res_y2$residuals

# Align data
PC_data <- as.matrix(variables[, paste0("pc", 1:J)])
n_obs <- min(length(W1), nrow(PC_data), length(W2_list[[1]]))
W1_aligned <- W1[1:n_obs]
PC_aligned <- PC_data[1:n_obs, ]

# -----------------------------------------------------------------------------
# OPTIMIZATION FOR EACH MATURITY
# -----------------------------------------------------------------------------
cat("\n========================================\n")
cat("OPTIMIZING PC WEIGHTS BY MATURITY\n")
cat("========================================\n\n")

# Run optimization for all maturities
cat("Finding optimal linear combinations...\n")
cat("(This may take a few moments)\n\n")

opt_results <- optimize_pc_weights_all_maturities(
  pc_matrix = PC_aligned,
  w1 = W1_aligned,
  w2_list = lapply(W2_list, function(w) w[1:n_obs]),
  tau = tau,
  use_t_minus_1 = TRUE,
  parallel = FALSE
)

# Display results for each maturity
cat("Optimization Results by Maturity:\n")
cat("=================================\n\n")

for (i in 1:I) {
  row <- opt_results[i, ]
  cat(sprintf("Maturity %d:\n", i))

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

# -----------------------------------------------------------------------------
# FIND BEST LINEAR COMBINATION
# -----------------------------------------------------------------------------
real_roots <- opt_results[!opt_results$is_complex, ]

if (nrow(real_roots) > 0) {
  best_idx <- which.min(real_roots$root_distance)
  best_maturity <- real_roots$maturity[best_idx]

  cat("========================================\n")
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

  # Verify unit variance
  full_results <- attr(opt_results, "full_results")
  best_full <- full_results[[best_maturity]]
  cat(sprintf(
    "\nLinear combination variance: %.4f (should be 1.0)\n",
    var(best_full$linear_comb, na.rm = TRUE)
  ))
} else {
  cat("\nNo maturities produced real roots.\n")
  best_maturity <- NULL
}

# -----------------------------------------------------------------------------
# COMPARISON: INDIVIDUAL PCs VS OPTIMAL
# -----------------------------------------------------------------------------
if (!is.null(best_maturity)) {
  cat("\n========================================\n")
  cat("COMPARISON WITH INDIVIDUAL PCs\n")
  cat("========================================\n\n")

  # Compute roots for individual PCs at best maturity
  individual_results <- data.frame(
    PC = 1:J,
    root1 = NA,
    root2 = NA,
    root_distance = NA
  )

  w2_best <- W2_list[[best_maturity]][1:n_obs]

  for (j in 1:J) {
    result <- solve_gamma_quadratic(
      pc_j = PC_aligned[, j],
      w1 = W1_aligned,
      w2 = w2_best,
      tau = tau,
      use_t_minus_1 = TRUE
    )

    if (is.null(result$error) && !is.complex(result$roots[1])) {
      individual_results$root1[j] <- result$roots[1]
      individual_results$root2[j] <- result$roots[2]
      individual_results$root_distance[j] <- abs(result$roots[1] - result$roots[2])
    }
  }

  cat(sprintf("Results for Maturity %d:\n\n", best_maturity))
  cat("Individual PCs:\n")
  for (j in 1:J) {
    if (!is.na(individual_results$root_distance[j])) {
      cat(sprintf(
        "  PC%d: distance = %.4f (roots: %.4f, %.4f)\n",
        j, individual_results$root_distance[j],
        individual_results$root1[j], individual_results$root2[j]
      ))
    } else {
      cat(sprintf("  PC%d: complex roots\n", j))
    }
  }

  cat(sprintf(
    "\nOptimal combination: distance = %.4f (roots: %.4f, %.4f)\n",
    real_roots$root_distance[best_idx],
    real_roots$root1[best_idx],
    real_roots$root2[best_idx]
  ))

  # Calculate improvement
  min_individual <- min(individual_results$root_distance, na.rm = TRUE)
  if (!is.na(min_individual) && min_individual > 0) {
    improvement <- (min_individual - real_roots$root_distance[best_idx]) / min_individual * 100
    cat(sprintf("\nImprovement over best individual PC: %.1f%%\n", improvement))
  }
}

# -----------------------------------------------------------------------------
# VISUALIZATION
# -----------------------------------------------------------------------------
cat("\n\nCreating visualizations...\n")

# 1. Root distances by maturity
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
      title = sprintf("Root Distance by Maturity (Optimized PC Weights, tau = %.2f)", tau),
      x = "Maturity (years)",
      y = "Distance between roots",
      caption = "Red point indicates minimum distance"
    ) +
    theme_minimal()

  print(p_opt)

  # 2. Weight composition plot
  weight_data <- opt_results %>%
    select(maturity, starts_with("weight_pc")) %>%
    pivot_longer(
      cols = starts_with("weight_pc"),
      names_to = "PC",
      values_to = "Weight"
    ) %>%
    mutate(PC = factor(gsub("weight_pc", "PC", PC)))

  p_weights <- ggplot(weight_data, aes(x = maturity, y = Weight, fill = PC)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = 1:I) +
    labs(
      title = "Optimal PC Weights by Maturity",
      x = "Maturity (years)",
      y = "Weight",
      fill = "PC"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p_weights)

  # Save plots if requested
  if (save_plots) {
    ggsave(file.path(output_dir, "optimal_root_distances.pdf"),
      p_opt,
      width = 8, height = 6
    )
    ggsave(file.path(output_dir, "optimal_weights.pdf"),
      p_weights,
      width = 10, height = 6
    )
    cat(sprintf("\nPlots saved to %s\n", output_dir))
  }
}

# -----------------------------------------------------------------------------
# SAVE RESULTS
# -----------------------------------------------------------------------------
optimization_results <- list(
  parameters = list(
    J = J,
    tau = tau,
    penalty_complex = penalty_complex
  ),
  optimization_table = opt_results,
  best_maturity = best_maturity,
  best_weights = if (!is.null(best_maturity)) best_weights else NULL,
  full_results = attr(opt_results, "full_results")
)

saveRDS(
  optimization_results,
  file.path(output_dir, "optimization_results.rds")
)
cat(sprintf(
  "\nOptimization results saved to %s\n",
  file.path(output_dir, "optimization_results.rds")
))

cat("\nAnalysis complete!\n")
