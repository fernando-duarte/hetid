# =============================================================================
# Test: compute_n_hat()
# =============================================================================
# This function computes the expected log bond price estimator n_hat(i,t), which
# estimates E_t[p_(t+i)^(1)] = -E_t[y_(t+i)^(1)]. It uses yields and term premia
# to calculate: n_hat = i*y_i - (i+1)*y_(i+1) + (i+1)*TP_(i+1) - i*TP_i

# Load package
library(hetid)

cat("Testing compute_n_hat() function\n")
cat("================================\n\n")

# Load ACM data
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Test with default parameters (i = 5)
cat("Computing n_hat for i=5\n")
n_hat_5 <- compute_n_hat(yields, term_premia, i = 5)

# Display results
cat(sprintf("  Number of observations: %d\n", length(n_hat_5)))
cat(sprintf(
  "  First 10 values: %s\n",
  paste(round(n_hat_5[1:10], 4), collapse = ", ")
))
cat(sprintf("  Mean: %.4f\n", mean(n_hat_5, na.rm = TRUE)))
cat(sprintf("  SD: %.4f\n", sd(n_hat_5, na.rm = TRUE)))
cat(sprintf(
  "  Range: [%.4f, %.4f]\n\n",
  min(n_hat_5, na.rm = TRUE), max(n_hat_5, na.rm = TRUE)
))

# Test with different maturities
cat("Computing n_hat for multiple maturities\n")
maturities <- 1:9 # c(1, 2, 5, 9)
all_neg <- numeric(length(maturities))
for (i in maturities) {
  n_hat_i <- compute_n_hat(yields, term_premia, i = i)
  cat(sprintf(
    "  i=%d: mean=%.4f, sd=%.4f\n",
    i, mean(n_hat_i, na.rm = TRUE), sd(n_hat_i, na.rm = TRUE)
  ))
  all_neg[i] <- all(n_hat_i < 0, na.rm = TRUE)
}

cat(sprintf(
  "  n_hat should be negative as bonds trade below par (below 1)\n"
))
for (i in maturities) {
  cat(sprintf(
    "  i=%d: Are all negative: %s\n",
    i, ifelse(all_neg[i], "✓ Yes", "✗ No") # i = 1 has some correct positives
  ))
}
# Visual check
cat("\nVisual check: n_hat should be close to -y_i for small term premia\n")
i <- 3
n_hat_3 <- compute_n_hat(yields, term_premia, i = i) # n_hat_3 in decimal
y_3 <- yields$y3 / 100 # Convert to decimal
tp_3 <- term_premia$tp3 / 100

cat(sprintf("  For i=3, first observation:\n"))
cat(sprintf("    y_3 = %.4f%%\n", yields$y3[1]))
cat(sprintf("    n_hat_3 = %.4f\n", n_hat_3[1]))

n_hat_4 <- compute_n_hat(yields, term_premia, i = i + 1)
y_4 <- yields$y4 / 100
tp_4 <- term_premia$tp4 / 100
cat(sprintf(
  "  i*y_3 - (i+1)*y_4: %.4f\n",
  i * y_3[1] - (i + 1) * y_4[1]
))
cat(sprintf(
  " (i+1)*TP_(i+1) - i*TP_i: %.4f\n",
  (i + 1) * tp_4[1] - i * tp_3[1]
))

error_3 <- n_hat_3 - (i * y_3 - (i + 1) * y_4 + (i + 1) * tp_4 - i * tp_3)
cat(sprintf(
  "max(abs(n_hat_3 - (i*y_3 - (i+1)*y_4 + (i+1)*tp_4 - i*tp_3))): %.4f\n",
  max(abs(error_3))
)) # should be zero

# Compute n_hat(i-1,t+1), which estimates E_(t+1)[p_(t+i)^(1)]

# Get variables for maturity i-1
n_hat_2 <- compute_n_hat(yields, term_premia, i = i - 1)
y_2 <- yields$y2 / 100
tp_2 <- term_premia$tp2 / 100

error_2 <- n_hat_2 - ((i - 1) * y_2 - i * y_3 + i * tp_3 - (i - 1) * tp_2)
cat(sprintf(
  paste0(
    "Error in n_hat with i=2 (n_hat_2 - manually computed value):",
    "%.4f\n"
  ),
  max(abs(error_2))
)) # should be zero

# Get n_hat_2 at t+1:
n_hat_2_t_plus_1 <- c(n_hat_2[-1], NA)

# Get y3, tp3 at t+1:
y_3_t_plus_1 <- c(y_3[-1], NA) # y_3 values shifted forward by 1 period
tp_3_t_plus_1 <- c(tp_3[-1], NA)

# Get y2, tp2 at t+1:
y_2_t_plus_1 <- c(y_2[-1], NA)
tp_2_t_plus_1 <- c(tp_2[-1], NA)

error_2_t_plus_1 <- n_hat_2_t_plus_1 -
  ((i - 1) * y_2_t_plus_1 - i * y_3_t_plus_1 -
    (i - 1) * tp_2_t_plus_1 + i * tp_3_t_plus_1)

cat(sprintf(
  paste0(
    "Error in n_hat with i=2 (n_hat_2 - manually computed value):",
    "%.4f\n"
  ),
  max(abs(error_2_t_plus_1), na.rm = TRUE)
)) # should be zero

# Test with i = 1
i <- 1
n_hat_1 <- compute_n_hat(yields, term_premia, i = i)
y_1 <- yields$y1 / 100
tp_1 <- term_premia$tp1 / 100

error_1 <- n_hat_1 - (i * y_1 - (i + 1) * y_2 + (i + 1) * tp_2 - i * tp_1)
cat(sprintf(
  "max(abs(n_hat_3 - (i*y_3 - manually computed value))): %.4f\n",
  max(abs(error_1))
)) # should be zero

cat("\nTest complete!\n")
