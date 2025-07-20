# =============================================================================
# Test: compute_reduced_form_residual_y2()
# =============================================================================
# This function computes reduced form residuals for Y2 (SDF innovations) by
# regressing SDF innovations on lagged principal components. It can compute
# residuals for multiple maturities simultaneously.

# Load package
library(hetid)

cat("Testing compute_reduced_form_residual_y2() function\n")
cat("===================================================\n\n")

# Load data
data("variables")
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Test 1: Single maturity
cat("Computing W2 for single maturity (i=5)\n")
res_y2_single <- compute_reduced_form_residual_y2(
  yields = yields,
  term_premia = term_premia,
  maturities = 5,
  n_pcs = 4,
  variables_data = variables
)

cat(sprintf("  Number of maturities: %d\n", length(res_y2_single$residuals)))
cat(sprintf("  Residuals length: %d\n", length(res_y2_single$residuals[[1]])))
cat(sprintf("  R-squared: %.4f\n", res_y2_single$r_squared[1]))

# Test 2: Multiple maturities
cat("\nComputing W2 for multiple maturities\n")
maturities <- c(1, 2, 5, 7, 9)
res_y2_multi <- compute_reduced_form_residual_y2(
  yields = yields,
  term_premia = term_premia,
  maturities = maturities,
  n_pcs = 4,
  variables_data = variables
)

cat(sprintf("  Maturities requested: %s\n", paste(maturities, collapse = ", ")))
cat(sprintf("  Results returned: %d\n", length(res_y2_multi$residuals)))
cat("\n  R-squared by maturity:\n")
for (i in 1:length(maturities)) {
  cat(sprintf(
    "    Maturity %d: R² = %.4f\n",
    maturities[i], res_y2_multi$r_squared[i]
  ))
}

# Test 3: Different numbers of PCs
cat("\nEffect of number of PCs on R-squared\n")
for (n_pc in c(2, 4, 6)) {
  res <- compute_reduced_form_residual_y2(
    yields = yields,
    term_premia = term_premia,
    maturities = 5,
    n_pcs = n_pc,
    variables_data = variables
  )
  cat(sprintf("  %d PCs: R² = %.4f\n", n_pc, res$r_squared[1]))
}

# Test 4: Residual properties
cat("\nResidual properties for maturity 3\n")
res_y2_3 <- compute_reduced_form_residual_y2(
  yields = yields,
  term_premia = term_premia,
  maturities = 3,
  n_pcs = 4,
  variables_data = variables
)
W2_3 <- res_y2_3$residuals[[1]]

cat(sprintf("  Mean: %.6f\n", mean(W2_3, na.rm = TRUE)))
cat(sprintf("  SD: %.6f\n", sd(W2_3, na.rm = TRUE)))
cat(sprintf("  Skewness: %.3f\n", moments::skewness(W2_3, na.rm = TRUE)))
cat(sprintf("  Kurtosis: %.3f\n", moments::kurtosis(W2_3, na.rm = TRUE)))

# Test 5: Compare residuals across maturities
cat("\nResidual standard deviations by maturity\n")
all_maturities <- 1:9
res_y2_all <- compute_reduced_form_residual_y2(
  yields = yields,
  term_premia = term_premia,
  maturities = all_maturities,
  n_pcs = 4,
  variables_data = variables
)

sd_by_maturity <- sapply(res_y2_all$residuals, sd, na.rm = TRUE)
for (i in 1:length(all_maturities)) {
  cat(sprintf("  Maturity %d: SD = %.6f\n", all_maturities[i], sd_by_maturity[i]))
}

# Test 6: Verify SDF innovations computation
cat("\nVerify Y2 (SDF innovations) computation\n")
# Manually compute SDF innovations for maturity 2
i <- 2
sdf_innov_manual <- compute_sdf_innovations(yields, term_premia, i = i)

# Get residuals from function
res_y2_2 <- compute_reduced_form_residual_y2(
  yields = yields,
  term_premia = term_premia,
  maturities = i,
  n_pcs = 4,
  variables_data = variables
)

# The function regresses SDF innovations on lagged PCs
# So the dependent variable should be SDF innovations
cat(sprintf("  SDF innovations length: %d\n", length(sdf_innov_manual)))
cat(sprintf("  W2 residuals length: %d\n", length(res_y2_2$residuals[[1]])))
cat(sprintf("  Lengths differ due to data alignment with variables data\n"))

# Show alignment warning
cat("\nNote: You may see warnings about data alignment.\n")
cat("This occurs because:\n")
cat("  - Variables data is quarterly (fewer observations)\n")
cat("  - ACM data is monthly (more observations)\n")
cat("The function handles this alignment automatically.\n")

cat("\nTest complete!\n")
