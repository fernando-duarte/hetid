# =============================================================================
# Test: compute_sdf_innovations()
# =============================================================================
# This function computes SDF innovations, which are the adjusted bond price
# changes that account for convexity effects. It multiplies the price changes
# by an exponential adjustment factor and includes second-order terms.

# Load package
library(hetid)

cat("Testing compute_sdf_innovations() function\n")
cat("==========================================\n\n")

# Load ACM data
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Test with default parameters (i = 5)
cat("Computing SDF innovations for i=5\n")
sdf_innov_5 <- compute_sdf_innovations(yields, term_premia, i = 5)

# Display results
cat(sprintf("  Number of observations: %d\n", length(sdf_innov_5)))
cat(sprintf(
  "  First 5 values: %s\n",
  paste(round(sdf_innov_5[1:5], 6), collapse = ", ")
))
cat(sprintf("  Mean: %.6f (should be near 0)\n", mean(sdf_innov_5, na.rm = TRUE)))
cat(sprintf("  SD: %.6f\n", sd(sdf_innov_5, na.rm = TRUE)))

# Test for different maturities
cat("\nSDF innovations by maturity\n")
innov_stats <- data.frame(
  maturity = 1:9,
  mean = numeric(9),
  sd = numeric(9),
  min = numeric(9),
  max = numeric(9)
)

for (i in 1:9) {
  sdf_innov_i <- compute_sdf_innovations(yields, term_premia, i = i)
  innov_stats$mean[i] <- mean(sdf_innov_i, na.rm = TRUE)
  innov_stats$sd[i] <- sd(sdf_innov_i, na.rm = TRUE)
  innov_stats$min[i] <- min(sdf_innov_i, na.rm = TRUE)
  innov_stats$max[i] <- max(sdf_innov_i, na.rm = TRUE)
}

print(round(innov_stats, 6))

# Compare with SDF news
cat("\nRelationship to SDF news\n")
i <- 4
sdf_news_4 <- compute_sdf_news(yields, term_premia, i = 4)
sdf_innov_4 <- compute_sdf_innovations(yields, term_premia, i = 4)

# Align lengths (both should be n-1)
cat(sprintf("  Length of SDF news: %d\n", length(sdf_news_4)))
cat(sprintf("  Length of SDF innovations: %d\n", length(sdf_innov_4)))

# Correlation
correlation <- cor(sdf_news_4, sdf_innov_4, use = "complete.obs")
cat(sprintf("  Correlation between news and innovations: %.3f\n", correlation))
cat(sprintf("  (High correlation expected, innovations include adjustment terms)\n"))

# Check the adjustment factor effect
cat("\nConvexity adjustment effect\n")
n_hat_4 <- compute_n_hat(yields, term_premia, i = 4)
exp_n_hat <- exp(n_hat_4)
cat(sprintf("  Mean exp(n_hat): %.4f\n", mean(exp_n_hat, na.rm = TRUE)))
cat(sprintf("  This multiplies the price changes in innovations\n"))

# Distribution properties
cat("\nDistribution properties\n")
sdf_innov_7 <- compute_sdf_innovations(yields, term_premia, i = 7)

# Should have fat tails due to exponential scaling
library(moments)
kurt <- kurtosis(sdf_innov_7, na.rm = TRUE)
skew <- skewness(sdf_innov_7, na.rm = TRUE)

cat(sprintf("  Kurtosis: %.2f (>3 indicates fat tails)\n", kurt))
cat(sprintf("  Skewness: %.2f\n", skew))

# Time series plot of a sample
cat("\nVisual inspection of time series\n")
sdf_innov_2 <- compute_sdf_innovations(yields, term_premia, i = 2)
dates <- acm_data$date[2:length(acm_data$date)]

# Show periods of high volatility
high_vol_periods <- which(abs(sdf_innov_2) > 2 * sd(sdf_innov_2, na.rm = TRUE))
cat(sprintf(
  "  Observations with |innovation| > 2*SD: %d\n",
  length(high_vol_periods)
))

if (length(high_vol_periods) > 0) {
  cat("  Sample high volatility dates:\n")
  for (idx in head(high_vol_periods, 5)) {
    cat(sprintf(
      "    %s: innovation = %.6f\n",
      dates[idx], sdf_innov_2[idx]
    ))
  }
}

cat("\nTest complete!\n")
