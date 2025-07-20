# =============================================================================
# Test: compute_c_hat()
# =============================================================================
# This function computes the supremum estimator c_hat(i,t), which provides an
# upper bound for bond price changes. It's based on the maximum possible change
# in bond prices given the term structure.

# Load package
library(hetid)

cat("Testing compute_c_hat() function\n")
cat("================================\n\n")

# Load ACM data
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Test with default parameters (i = 5)
cat("Computing c_hat for i=5\n")
i <- 5
c_hat_5 <- compute_c_hat(yields, term_premia, i = i)

# Display results
cat(sprintf(
  "  Number of observations (should equal one): %d\n",
  length(c_hat_5)
))
cat(sprintf("  Value: %.4f\n", c_hat_5))

# Test for different maturities
cat("\nc_hat for different maturities\n")
c_means <- numeric(9)
for (i in 1:9) {
  c_means[i] <- compute_c_hat(yields, term_premia, i = i)
  cat(sprintf("  i=%d: mean c_hat = %.4f\n", i, c_means[i]))
}

# c_hat should be positive (it's a supremum of exponentials)
cat("\nc_hat should always be positive\n")
all_positive <- TRUE
for (i in 1:9) {
  c_hat_i <- compute_c_hat(yields, term_premia, i = i)
  if (any(c_hat_i <= 0, na.rm = TRUE)) {
    all_positive <- FALSE
    cat(sprintf("  WARNING: Negative values found for i=%d\n", i))
  }
}
cat(sprintf(
  "  All values positive: %s\n",
  ifelse(all_positive, "✓ Yes", "✗ No")
))

# c_hat should be below 1 for maturities > 1 (as bonds trade below par)
cat("\nTest: c_hat should be below one\n")
all_below_1 <- TRUE
for (i in 2:9) {
  c_hat_i <- compute_c_hat(yields, term_premia, i = i)
  if (any(c_hat_i > 1, na.rm = TRUE)) {
    all_below_1 <- FALSE
    cat(sprintf("  WARNING: Values above 1 found for i=%d\n", i))
  }
}
cat(sprintf(
  "  All values below 1: %s\n",
  ifelse(all_below_1, "✓ Yes", "✗ No")
))

# Compare with manual construction from n_hat
cat("\nConstruct c_hat manually from to n_hat\n")

n_hat_3 <- compute_n_hat(yields, term_premia, i = 3)
c_hat_3 <- compute_c_hat(yields, term_premia, i = 3)

cat(sprintf("  For i=3:\n"))
cat(sprintf("    Max n_hat(3): %.4f\n", max(n_hat_3, na.rm = TRUE)))
cat(sprintf("    Error c_hat(3) - e^( 2 * max(n_hat(3)) )\n"))
cat(sprintf(
  "Should be zero: %.4f\n",
  1e7 * abs(c_hat_3 - exp(2 * max(n_hat_3)))
))

n_hat_4 <- compute_n_hat(yields, term_premia, i = 4)
c_hat_4 <- compute_c_hat(yields, term_premia, i = 4)

cat(sprintf("  For i=4:\n"))
cat(sprintf("    Max n_hat(4): %.4f\n", max(n_hat_4, na.rm = TRUE)))
cat(sprintf("    Error c_hat(4) - e^( 2 * max(n_hat(4)) )\n"))
cat(sprintf(
  "Should be zero: %.4f\n",
  1e7 * abs(c_hat_4 - exp(2 * max(n_hat_4)))
))
