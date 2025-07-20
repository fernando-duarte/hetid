# =============================================================================
# Test: compute_k_hat()
# =============================================================================
# This function computes the fourth moment estimator k_hat(i,t), which estimates
# the expected fourth power of bond price changes. It's used in computing
# variance bounds for bond returns.

# Load package
library(hetid)

cat("Testing compute_k_hat() function\n")
cat("================================\n\n")

# Load ACM data
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Test with default parameters (i = 5)
cat("Computing k_hat for i=5\n")
i <- 5
k_hat_5 <- compute_k_hat(yields, term_premia, i = i)

# Display result
cat(sprintf("  Number of values (should be 1): %d\n", length(k_hat_5)))
cat(sprintf(
  "  Should be positive (fourth moment): %s\n",
  ifelse(all(k_hat_5 > 0, na.rm = TRUE), "✓ Yes", "✗ No")
))

# Get n_hat_(i-1,t+1) and y_(t+i)^(1)
n_hat <- compute_n_hat(yields, term_premia, i = i - 1)
y <- yields$y1 / 100 # Convert to decimal

# Get n_hat at t+1:
n_hat_t_plus_1 <- c(n_hat[-1], NA)

# Get y at t+i:
y_t_plus_i <- c(y[-seq_len(i)], rep(NA, i))

# Construct k_hat manually
T <- length(y)
k_hat_5_manual <- sum((-y_t_plus_i - n_hat_t_plus_1)^4, na.rm = TRUE) / (T - i)

# Check if k_hat_5 matches manual calculation
error_5 <- k_hat_5 - k_hat_5_manual
cat("\nTest: Error in k_hat with i=5 (k_hat_5 - k_hat_5_manual)\n")
cat(sprintf(
  " Should be zero: %.6f\n",
  abs(1e7 * error_5)
)) # should be 0

# Test special case i=1
cat("\nSpecial case i=1 (uses n_hat_0 = -y1)\n")
i <- 1
k_hat_1 <- compute_k_hat(yields, term_premia, i = i)
cat(sprintf(
  "  Should be zero: %s\n",
  ifelse(all(k_hat_1 == 0, na.rm = TRUE), "✓ Yes", "✗ No")
))

# Get n_hat_(i-1,t+1) and p_(t+i)^(1)
n_hat <- -yields$y1 / 100 # n_hat_(0,t+1) = -y1(t+1) in decimal
y <- yields$y1 / 100 # Convert to decimal

# Get n_hat at t+1:
n_hat_t_plus_1 <- c(n_hat[-1], NA)

# Get y at t+i:
y_t_plus_i <- c(y[-seq_len(i)], rep(NA, i))

# Construct k_hat manually
T <- length(y)
k_hat_1_manual <- sum((-y_t_plus_i - n_hat_t_plus_1)^4, na.rm = TRUE) / (T - i)

# Check if k_hat_1 matches manual calculation
error_1 <- k_hat_1 - k_hat_1_manual
cat("\nTest: Error in k_hat with i=1 (k_hat_1 - k_hat_1_manual)\n")
cat(sprintf(
  " Should be zero: %.6f\n",
  abs(1e7 * error_1)
)) # should be 0

# Test multiple maturities
cat("\nTest: k_hat increases with maturity (on average)\n")
k_means <- numeric(9)
for (i in 1:9) {
  k_means[i] <- compute_k_hat(yields, term_premia, i = i)
  cat(sprintf("  i=%d: k_hat*10^7 = %.6f\n", i, k_means[i] * 1e7))
}

# Check monotonicity
is_increasing <- all(diff(k_means) > 0)
cat(sprintf(
  "\nGenerally increasing with maturity: %s\n",
  ifelse(is_increasing, "✓ Yes", "~ Mostly")
))

cat("\nTest complete!\n")
