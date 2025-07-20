# =============================================================================
# Test: optimize_pc_weights()
# =============================================================================
# This function finds optimal weights for a linear combination of PCs that
# minimizes the distance between the two roots of the gamma_1 quadratic equation.
# Complex roots are penalized to favor real solutions.

# Load package
library(hetid)

cat("Testing optimize_pc_weights() function\n")
cat("======================================\n\n")

# Prepare test data
data("variables")
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Compute residuals
res_y1 <- compute_reduced_form_residual_y1(n_pcs = 4)
W1 <- res_y1$residuals

res_y2 <- compute_reduced_form_residual_y2(
  yields = yields,
  term_premia = term_premia,
  maturities = c(3, 5, 7), # Multiple maturities for testing
  n_pcs = 4,
  variables_data = variables
)
W2_list <- res_y2$residuals

# Get PC matrix
PC_matrix <- as.matrix(variables[, paste0("pc", 1:4)])

# Align data
n_obs <- min(length(W1), nrow(PC_matrix), length(W2_list[[1]]))
W1_aligned <- W1[1:n_obs]
PC_aligned <- PC_matrix[1:n_obs, ]
W2_aligned <- W2_list[[2]][1:n_obs] # Use maturity 5

# Test 1: Basic optimization
cat("Basic optimization for maturity 5\n")
opt_result <- optimize_pc_weights(
  pc_matrix = PC_aligned,
  w1 = W1_aligned,
  w2 = W2_aligned,
  tau = 0.5,
  maturity = 5,
  use_t_minus_1 = TRUE
)

cat(sprintf(
  "  Convergence: %s\n",
  ifelse(opt_result$convergence == 0, "Success", "Failed")
))
cat(sprintf(
  "  Optimal weights: %s\n",
  paste(round(opt_result$optimal_weights, 3), collapse = ", ")
))
cat(sprintf("  Is complex: %s\n", opt_result$is_complex))

if (!opt_result$is_complex) {
  cat(sprintf("  First root: %.4f\n", opt_result$roots[1]))
  cat(sprintf("  Second root: %.4f\n", opt_result$roots[2]))
  cat(sprintf("  Root distance: %.4f\n", opt_result$root_distance))
}

# Test 2: Different initial weights
cat("\nEffect of initial weights\n")
initial_patterns <- list(
  "Equal" = rep(1 / sqrt(4), 4),
  "First PC" = c(1, 0, 0, 0),
  "Random" = runif(4)
)

for (pattern_name in names(initial_patterns)) {
  opt <- optimize_pc_weights(
    pc_matrix = PC_aligned,
    w1 = W1_aligned,
    w2 = W2_aligned,
    tau = 0.5,
    maturity = 5,
    use_t_minus_1 = TRUE,
    initial_weights = initial_patterns[[pattern_name]]
  )

  cat(sprintf(
    "  %s: distance = %.4f, convergence = %d\n",
    pattern_name,
    ifelse(opt$is_complex, NA, opt$root_distance),
    opt$convergence
  ))
}

# Test 3: Different tau values
cat("\nOptimization across tau values\n")
tau_values <- c(0.1, 0.3, 0.5, 0.7, 0.9)

for (tau in tau_values) {
  opt <- optimize_pc_weights(
    pc_matrix = PC_aligned,
    w1 = W1_aligned,
    w2 = W2_aligned,
    tau = tau,
    maturity = 5,
    use_t_minus_1 = TRUE
  )

  if (opt$is_complex) {
    cat(sprintf("  tau=%.1f: Complex roots (optimization failed)\n", tau))
  } else {
    cat(sprintf(
      "  tau=%.1f: min distance = %.4f\n",
      tau, opt$root_distance
    ))
  }
}

# Test 4: Compare with individual PCs
cat("\nComparison with individual PCs\n")
# First compute for individual PCs
individual_distances <- numeric(4)
for (j in 1:4) {
  weights_j <- rep(0, 4)
  weights_j[j] <- 1

  result <- solve_gamma_quadratic_lincomb(
    pc_matrix = PC_aligned,
    weights = weights_j,
    w1 = W1_aligned,
    w2 = W2_aligned,
    tau = 0.5,
    use_t_minus_1 = TRUE
  )

  if (!is.null(result$error) || is.complex(result$roots[1])) {
    individual_distances[j] <- NA
  } else {
    individual_distances[j] <- abs(result$roots[1] - result$roots[2])
  }
}

# Optimized result
opt_result <- optimize_pc_weights(
  pc_matrix = PC_aligned,
  w1 = W1_aligned,
  w2 = W2_aligned,
  tau = 0.5,
  maturity = 5,
  use_t_minus_1 = TRUE
)

cat("  Individual PC root distances:\n")
for (j in 1:4) {
  cat(sprintf("    PC%d: %.4f\n", j, individual_distances[j]))
}
cat(sprintf("  Optimized distance: %.4f\n", opt_result$root_distance))
cat(sprintf(
  "  Improvement: %.1f%%\n",
  (min(individual_distances, na.rm = TRUE) - opt_result$root_distance) /
    min(individual_distances, na.rm = TRUE) * 100
))

# Test 5: Optimization parameters
cat("\nDifferent optimization settings\n")
opt_settings <- list(
  "Default" = list(method = "Nelder-Mead", maxit = 1000),
  "More iterations" = list(method = "Nelder-Mead", maxit = 5000),
  "Low penalty" = list(penalty_complex = 1e3)
)

for (setting_name in names(opt_settings)) {
  opt <- do.call(optimize_pc_weights, c(
    list(
      pc_matrix = PC_aligned,
      w1 = W1_aligned,
      w2 = W2_aligned,
      tau = 0.5,
      maturity = 5,
      use_t_minus_1 = TRUE
    ),
    opt_settings[[setting_name]]
  ))

  cat(sprintf(
    "  %s: distance = %.4f, obj value = %.4f\n",
    setting_name,
    ifelse(opt$is_complex, NA, opt$root_distance),
    opt$objective_value
  ))
}

# Test 6: Verify unit variance of linear combination
cat("\nVerify properties of optimal solution\n")
opt_final <- optimize_pc_weights(
  pc_matrix = PC_aligned,
  w1 = W1_aligned,
  w2 = W2_aligned,
  tau = 0.5,
  maturity = 5,
  use_t_minus_1 = TRUE
)

if (!is.null(opt_final$linear_comb)) {
  lc_var <- var(opt_final$linear_comb, na.rm = TRUE)
  weights_norm <- sum(opt_final$optimal_weights^2)

  cat(sprintf("  Linear combination variance: %.4f (should be ~1)\n", lc_var))
  cat(sprintf("  Sum of squared weights: %.4f\n", weights_norm))
  cat(sprintf("  Objective function value: %.6f\n", opt_final$objective_value))
}

cat("\nTest complete!\n")
