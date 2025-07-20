# =============================================================================
# Test: optimize_pc_weights_all_maturities()
# =============================================================================
# This function runs PC weight optimization across all specified maturities and
# returns a summary table. It can optionally use parallel processing for speed.

# Load package
library(hetid)

cat("Testing optimize_pc_weights_all_maturities() function\n")
cat("=====================================================\n\n")

# Prepare test data
data("variables")
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Compute residuals for all maturities
res_y1 <- compute_reduced_form_residual_y1(n_pcs = 4)
W1 <- res_y1$residuals

# Test with subset of maturities for speed
test_maturities <- c(2, 4, 6, 8)
res_y2 <- compute_reduced_form_residual_y2(
  yields = yields,
  term_premia = term_premia,
  maturities = test_maturities,
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
W2_list_aligned <- lapply(W2_list, function(w) w[1:n_obs])

# Test 1: Basic functionality (no parallel)
cat("Basic optimization across maturities\n")
opt_results <- optimize_pc_weights_all_maturities(
  pc_matrix = PC_aligned,
  w1 = W1_aligned,
  w2_list = W2_list_aligned,
  tau = 0.5,
  use_t_minus_1 = TRUE,
  parallel = FALSE
)

cat(sprintf("  Results for %d maturities\n", nrow(opt_results)))
cat("\n  Summary of results:\n")
print(opt_results[, c("maturity", "root_distance", "is_complex", "convergence")])

# Test 2: Extract full results
cat("\nAccessing full results\n")
full_results <- attr(opt_results, "full_results")
cat(sprintf(
  "  Full results stored: %s\n",
  ifelse(!is.null(full_results), "Yes", "No")
))
cat(sprintf("  Number of full results: %d\n", length(full_results)))

# Show optimal weights for first maturity
if (length(full_results) > 0) {
  first_result <- full_results[[1]]
  cat(sprintf(
    "\n  Optimal weights for maturity %d:\n",
    first_result$maturity
  ))
  for (j in 1:4) {
    cat(sprintf("    PC%d: %.4f\n", j, first_result$optimal_weights[j]))
  }
}

# Test 3: Find best maturity
cat("\nFinding best maturity\n")
real_roots <- opt_results[!opt_results$is_complex, ]
if (nrow(real_roots) > 0) {
  best_idx <- which.min(real_roots$root_distance)
  best_mat <- real_roots$maturity[best_idx]

  cat(sprintf("  Best maturity: %d\n", best_mat))
  cat(sprintf("  Root distance: %.4f\n", real_roots$root_distance[best_idx]))
  cat(sprintf(
    "  Roots: [%.4f, %.4f]\n",
    real_roots$root1[best_idx],
    real_roots$root2[best_idx]
  ))

  # Show weights
  weight_cols <- grep("^weight_pc", names(real_roots))
  best_weights <- as.numeric(real_roots[best_idx, weight_cols])
  cat("  Optimal weights:\n")
  for (j in 1:length(best_weights)) {
    cat(sprintf("    PC%d: %.4f\n", j, best_weights[j]))
  }
} else {
  cat("  No real root solutions found\n")
}

# Test 4: Different tau values
cat("\nEffect of tau on optimization\n")
tau_values <- c(0.3, 0.5, 0.7)
min_distances <- numeric(length(tau_values))

for (i in 1:length(tau_values)) {
  opt_tau <- optimize_pc_weights_all_maturities(
    pc_matrix = PC_aligned,
    w1 = W1_aligned,
    w2_list = W2_list_aligned,
    tau = tau_values[i],
    use_t_minus_1 = TRUE,
    parallel = FALSE
  )

  real_tau <- opt_tau[!opt_tau$is_complex, ]
  if (nrow(real_tau) > 0) {
    min_distances[i] <- min(real_tau$root_distance)
  } else {
    min_distances[i] <- NA
  }
}

cat("  Minimum root distances by tau:\n")
for (i in 1:length(tau_values)) {
  cat(sprintf(
    "    tau = %.1f: min distance = %.4f\n",
    tau_values[i], min_distances[i]
  ))
}

# Test 5: Convergence statistics
cat("\nConvergence statistics\n")
convergence_table <- table(opt_results$convergence)
cat("  Convergence codes:\n")
for (code in names(convergence_table)) {
  cat(sprintf(
    "    Code %s: %d maturities\n",
    code, convergence_table[code]
  ))
}
cat("  (0 = successful convergence)\n")

# Test 6: Complex vs real roots
cat("\nComplex vs real root solutions\n")
n_complex <- sum(opt_results$is_complex)
n_real <- sum(!opt_results$is_complex)
cat(sprintf(
  "  Complex root cases: %d (%.1f%%)\n",
  n_complex, n_complex / nrow(opt_results) * 100
))
cat(sprintf(
  "  Real root cases: %d (%.1f%%)\n",
  n_real, n_real / nrow(opt_results) * 100
))

# Show which maturities produce complex roots
if (n_complex > 0) {
  complex_mats <- opt_results$maturity[opt_results$is_complex]
  cat(sprintf(
    "  Maturities with complex roots: %s\n",
    paste(complex_mats, collapse = ", ")
  ))
}

# Note about parallel processing
cat("\nNote: This test used parallel = FALSE for compatibility.\n")
cat("Set parallel = TRUE and specify n_cores for faster processing\n")
cat("with large numbers of maturities.\n")

cat("\nTest complete!\n")
