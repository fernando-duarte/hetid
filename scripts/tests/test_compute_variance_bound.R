# =============================================================================
# Test: compute_variance_bound()
# =============================================================================
# This function computes the variance bound for bond returns using the k_hat
# (fourth moment) and c_hat (supremum) estimators. It provides an upper bound
# on the variance of bond price changes.

# Load package
library(hetid)

cat("Testing compute_variance_bound() function\n")
cat("=========================================\n\n")

# Load ACM data
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Test with default parameters (i = 5)
cat("Computing variance bound for i=5\n")
var_bound_5 <- compute_variance_bound(yields, term_premia, i = 5)

# Display results
cat(sprintf("  Number of values (should be 1): %d\n", length(var_bound_5)))
cat(sprintf("  Value: %.6e\n", var_bound_5))

# Test for multiple maturities
cat("\nVariance bounds by maturity\n")
var_bounds <- numeric(9)
for (i in 1:9) {
  var_bound_i <- compute_variance_bound(yields, term_premia, i = i)
  var_bounds[i] <- var_bound_i
  cat(sprintf("  i=%d: variance bound = %.6e\n", i, var_bounds[i]))
}

# Variance bound should be zero for i=1
var_bound_1 <- compute_variance_bound(yields, term_premia, i = 1)
cat("\nVariance bound for i=1 should be zero\n")
if (var_bound_1 != 0) {
  cat(sprintf("  WARNING: Non-zero value %.6e for i=1\n", var_bound_1))
}
cat(sprintf(
  "  Value is zero for i=1: %s\n",
  ifelse(var_bound_1 == 0, "✓ Yes", "✗ No")
))

# Variance bounds should be positive for i>1
cat("\nAll variance bounds should be positive\n")
all_positive <- TRUE
for (i in 2:9) {
  var_bound_i <- compute_variance_bound(yields, term_premia, i = i)
  if (var_bound_i <= 0) {
    all_positive <- FALSE
    cat(sprintf("  WARNING: Non-positive value %.6e for i=%d\n", var_bound_i, i))
  }
}
cat(sprintf(
  "  All values positive: %s\n",
  ifelse(all_positive, "✓ Yes", "✗ No")
))

# Compare components
cat("\nRelationship to k_hat and c_hat\n")
i <- 3
k_hat_3 <- compute_k_hat(yields, term_premia, i = 3)
c_hat_3 <- compute_c_hat(yields, term_premia, i = 3)
var_bound_3 <- compute_variance_bound(yields, term_premia, i = 3)

# Variance bound = 0.25 * c_hat * k_hat
computed_bound <- 0.25 * c_hat_3 * k_hat_3
difference <- abs(var_bound_3 - computed_bound)

cat(sprintf("  For i=3:\n"))
cat(sprintf("    k_hat: %.6e\n", k_hat_3))
cat(sprintf("    c_hat: %.6f\n", c_hat_3))
cat(sprintf(
  "    Variance bound: %.6e\n",
  var_bound_3
))
cat(sprintf(
  "    Verification (0.25*c*k): %.6e\n",
  computed_bound
))
cat(sprintf("    Absolute difference: %.2e\n", difference))

# Check monotonicity with maturity
cat("\nMonotonicity with maturity\n")
# Variance bounds should generally increase with maturity
is_increasing <- all(diff(var_bounds) > 0)
cat(sprintf(
  "  Strictly increasing with maturity: %s\n",
  ifelse(is_increasing, "✓ Yes", "~ Mostly")
))

# Show the pattern
cat("  Pattern of variance bounds:\n")
for (i in 1:9) {
  cat(sprintf(
    "    i=%d: %.2e%s\n",
    i,
    var_bounds[i],
    if (i > 1 && var_bounds[i] > var_bounds[i - 1]) " ↑" else if (i > 1) " ↓" else ""
  ))
}

cat("  Pattern of std dev bounds in % = 100*sqrt(variance bounds) :\n")
for (i in 1:9) {
  cat(sprintf(
    "    i=%d: %.2e%s\n",
    i,
    sqrt(var_bounds[i]),
    if (i > 1 && var_bounds[i] > var_bounds[i - 1]) " ↑" else if (i > 1) " ↓" else ""
  ))
}

# Compare variance bounds to unconditional yield variances
cat("\nRatio of variance bound to unconditional yield variance\n")
cat("  (Shows how tight the bound is relative to actual yield volatility)\n")
for (i in 1:9) {
  # Get the yield for maturity i
  yield_col <- paste0("y", i)
  if (yield_col %in% names(yields)) {
    yield_i <- yields[[yield_col]]
    # Compute unconditional variance (yields are in percentage points)
    uncond_var <- var(yield_i, na.rm = TRUE)
    # Convert to decimal squared (since variance bound is in decimal units)
    uncond_var_decimal <- uncond_var / 10000 # (percent/100)^2

    # Compute ratio
    if (uncond_var_decimal > 0 && var_bounds[i] > 0) {
      ratio <- var_bounds[i] / uncond_var_decimal
      sd_ratio <- sqrt(ratio)
      cat(sprintf(
        paste0(
          "    i=%d: Bound/Var(y%d) = %.2e / %.2e = %.4f    |",
          "    sqrt(Bound/Var)(y%d) = %.4f\n"
        ),
        i, i, var_bounds[i], uncond_var_decimal, ratio, i, ssd_ratio
      ))
    } else if (var_bounds[i] == 0) {
      cat(sprintf("    i=%d: Bound = 0 (k_hat is zero for i=1)\n", i))
    }
  }
}

cat("\nTest complete!\n")
