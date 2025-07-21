# =============================================================================
# Test: solve_gamma_quadratic_lincomb()
# =============================================================================
# This function solves the quadratic equation for gamma_1 using a linear
# combination of principal components instead of a single PC. The weights
# are normalized to ensure unit variance of the linear combination.

# Load package
library(hetid)

cat("Testing solve_gamma_quadratic_lincomb() function\n")
cat("================================================\n\n")

# Prepare test data
data("variables")
acm_data <- extract_acm_data(
  data_types = c("yields", "term_premia"),
  frequency = "quarterly"
)
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Compute residuals
res_y1 <- compute_w1_residuals(n_pcs = 4)
W1 <- res_y1$residuals

res_y2 <- compute_w2_residuals(
  yields = yields,
  term_premia = term_premia,
  maturities = 5,
  n_pcs = 4,
  pcs = as.matrix(variables[, paste0("pc", 1:4)])
)
W2 <- res_y2$residuals[[1]]

# Get PC matrix
PC_matrix <- as.matrix(variables[, paste0("pc", 1:4)])

# Align all data
n_obs <- min(length(W1), length(W2), nrow(PC_matrix))
W1_aligned <- W1[1:n_obs]
W2_aligned <- W2[1:n_obs]
PC_aligned <- PC_matrix[1:n_obs, ]

# Test: Equal weights
cat("Linear combination with equal weights\n")
weights_equal <- rep(1 / sqrt(4), 4) # Normalized to unit variance
result_equal <- solve_gamma_quadratic_lincomb(
  pc_matrix = PC_aligned,
  weights = weights_equal,
  w1 = W1_aligned,
  w2 = W2_aligned,
  tau = 0.5,
  use_t_minus_1 = TRUE
)

if (!is.null(result_equal$error)) {
  cat(sprintf("  Error: %s\n", result_equal$error))
} else {
  cat(sprintf(
    "  Roots: [%.4f, %.4f]\n",
    Re(result_equal$roots[1]), Re(result_equal$roots[2])
  ))
  cat(sprintf(
    "  Normalized weights: %s\n",
    paste(round(result_equal$normalized_weights, 3), collapse = ", ")
  ))
  cat(sprintf("  Linear combination variance: %.4f\n", result_equal$variance))
}

# Test: Single PC (should match solve_gamma_quadratic)
cat("\nSingle PC weight (should match basic function)\n")
weights_single <- c(1, 0, 0, 0)
result_single <- solve_gamma_quadratic_lincomb(
  pc_matrix = PC_aligned,
  weights = weights_single,
  w1 = W1_aligned,
  w2 = W2_aligned,
  tau = 0.5,
  use_t_minus_1 = TRUE
)

# Compare with basic function
result_basic <- solve_gamma_quadratic(
  pc_j = PC_aligned[, 1],
  w1 = W1_aligned,
  w2 = W2_aligned,
  tau = 0.5,
  use_t_minus_1 = TRUE
)

cat(sprintf(
  "  Linear combination roots: [%.4f, %.4f]\n",
  Re(result_single$roots[1]), Re(result_single$roots[2])
))
cat(sprintf(
  "  Basic function roots: [%.4f, %.4f]\n",
  Re(result_basic$roots[1]), Re(result_basic$roots[2])
))
cat(sprintf(
  "  Match: %s\n",
  ifelse(abs(result_single$roots[1] - result_basic$roots[1]) < 0.001,
    "✓ Yes", "✗ No"
  )
))

# Test: Different weight patterns
cat("\nDifferent weight patterns\n")
weight_patterns <- list(
  "First two PCs" = c(1 / sqrt(2), 1 / sqrt(2), 0, 0),
  "Decreasing" = c(0.6, 0.4, 0.2, 0.1),
  "Last PC only" = c(0, 0, 0, 1),
  "Alternating" = c(0.7, -0.3, 0.5, -0.2)
)

for (pattern_name in names(weight_patterns)) {
  weights <- weight_patterns[[pattern_name]]
  result <- solve_gamma_quadratic_lincomb(
    pc_matrix = PC_aligned,
    weights = weights,
    w1 = W1_aligned,
    w2 = W2_aligned,
    tau = 0.5,
    use_t_minus_1 = TRUE
  )

  if (!is.null(result$error)) {
    cat(sprintf("  %s: Error\n", pattern_name))
  } else if (is.complex(result$roots[1])) {
    cat(sprintf("  %s: Complex roots\n", pattern_name))
  } else {
    cat(sprintf(
      "  %s: roots = [%.3f, %.3f], distance = %.3f\n",
      pattern_name,
      result$roots[1], result$roots[2],
      abs(result$roots[1] - result$roots[2])
    ))
  }
}

# Test: Verify normalization
cat("\nWeight normalization\n")
raw_weights <- c(2, 3, 1, 0.5) # Not normalized
result_raw <- solve_gamma_quadratic_lincomb(
  pc_matrix = PC_aligned,
  weights = raw_weights,
  w1 = W1_aligned,
  w2 = W2_aligned,
  tau = 0.5,
  use_t_minus_1 = TRUE
)

cat(sprintf("  Raw weights: %s\n", paste(round(raw_weights, 3), collapse = ", ")))
cat(sprintf(
  "  Normalized weights: %s\n",
  paste(round(result_raw$normalized_weights, 3), collapse = ", ")
))
cat(sprintf(
  "  Sum of squared normalized weights: %.4f\n",
  sum(result_raw$normalized_weights^2)
))
cat(sprintf(
  "  Linear combination variance: %.4f (should be ~1)\n",
  result_raw$variance
))

# Test: Effect of tau
cat("\nEffect of tau with fixed weights\n")
weights_fixed <- c(0.5, 0.5, 0.3, 0.3)
tau_values <- c(0.1, 0.3, 0.5, 0.7, 0.9)

for (tau in tau_values) {
  result <- solve_gamma_quadratic_lincomb(
    pc_matrix = PC_aligned,
    weights = weights_fixed,
    w1 = W1_aligned,
    w2 = W2_aligned,
    tau = tau,
    use_t_minus_1 = TRUE
  )

  if (!is.null(result$error)) {
    cat(sprintf("  tau=%.1f: Error\n", tau))
  } else if (is.complex(result$roots[1])) {
    cat(sprintf("  tau=%.1f: Complex roots\n", tau))
  } else {
    cat(sprintf(
      "  tau=%.1f: distance = %.3f\n",
      tau, abs(result$roots[1] - result$roots[2])
    ))
  }
}

# Test: Properties of linear combination
cat("\nLinear combination properties\n")
weights_test <- c(0.4, 0.3, 0.2, 0.1)
result_test <- solve_gamma_quadratic_lincomb(
  pc_matrix = PC_aligned,
  weights = weights_test,
  w1 = W1_aligned,
  w2 = W2_aligned,
  tau = 0.5,
  use_t_minus_1 = TRUE
)

if (!is.null(result_test$linear_comb)) {
  lc <- result_test$linear_comb
  cat(sprintf("  Linear combination statistics:\n"))
  cat(sprintf("    Mean: %.4f\n", mean(lc, na.rm = TRUE)))
  cat(sprintf("    SD: %.4f\n", sd(lc, na.rm = TRUE)))
  cat(sprintf(
    "    Correlation with PC1: %.3f\n",
    cor(lc, PC_aligned[, 1], use = "complete.obs")
  ))
}

cat("\nTest complete!\n")
