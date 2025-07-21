# =============================================================================
# Test: compute_price_news()
# =============================================================================
# This function computes price news, which represents
# the unexpected component of bond price changes. It calculates the difference
# between realized and expected log bond prices.
#
# Note: The empirical price news shows significant persistence (AR(1) ≈ 0.45),
# which differs from theoretical expectations of unpredictability. This occurs
# because n_hat values are extremely persistent (AR(1) > 0.99), and price news
# is computed as differences of these highly persistent series.

# Load package
library(hetid)

cat("Testing compute_price_news() function\n")
cat("===================================\n\n")

# Load ACM data
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Test with default parameters (i = 5)
cat("Computing price news for i=5\n")
price_news_5 <- compute_price_news(yields, term_premia, i = 5)

# Display results
cat(sprintf("  Number of observations: %d\n", length(price_news_5)))
cat(sprintf("  Note: Length is n-1 due to differencing\n"))
cat(sprintf(
  "  First 5 values: %s\n",
  paste(round(price_news_5[1:5], 4), collapse = ", ")
))
cat(sprintf("  Mean: %.4f (should be near 0)\n", mean(price_news_5, na.rm = TRUE)))
cat(sprintf("  SD: %.4f\n", sd(price_news_5, na.rm = TRUE)))

# Test for different maturities
cat("\nPrice news statistics by maturity\n")
maturities <- 1:9 # c(1, 2, 5, 9)
for (i in maturities) {
  price_news_i <- compute_price_news(yields, term_premia, i = i)
  cat(sprintf(
    "  i=%d: mean=%.4f, sd=%.4f, skew=%.2f\n",
    i,
    mean(price_news_i, na.rm = TRUE),
    sd(price_news_i, na.rm = TRUE),
    moments::skewness(price_news_i, na.rm = TRUE)
  ))
}

# Test yield news option
cat("\nYield news (return_yield_news = TRUE)\n")
yield_news_5 <- compute_price_news(yields, term_premia,
  i = 5,
  return_yield_news = TRUE
)
price_news_5_check <- compute_price_news(yields, term_premia,
  i = 5,
  return_yield_news = FALSE
)

# Yield news should be negative of price news
cat(sprintf("  First value price news: %.4f\n", price_news_5_check[1]))
cat(sprintf("  First value yield news: %.4f\n", yield_news_5[1]))
cat(sprintf("  Sum should be ~0: %.6f\n", price_news_5_check[1] + yield_news_5[1]))

# Check correlation with yield changes
cat("\nCorrelation with yield changes\n")
i <- 3
price_news_3 <- compute_price_news(yields, term_premia, i = 3)
yield_changes_3 <- diff(yields$y3) / 100 # Convert to decimal

# Align lengths
min_len <- min(length(price_news_3), length(yield_changes_3))
correlation <- cor(price_news_3[1:min_len], yield_changes_3[1:min_len],
  use = "complete.obs"
)

cat(sprintf(
  "  Correlation between price news and yield changes (i=3): %.3f\n",
  correlation
))
cat(sprintf("  Should be negative (higher yields → lower bond prices)\n"))

# Compare to manual construction
# \Delta_(t+1)p_(t+i)^(1) = E_(t+1)[p_(t+i)^(1)]-E_(t)[p_(t+i)^(1)]

# Compute n_hat(i,t), which estimates E_(t)[p_(t+i)^(1)]
n_hat_3 <- compute_n_hat(yields, term_premia, i = 3)
n_hat_3_t_minus_1 <- c(NA, n_hat_3[-length(n_hat_3)]) # lag one period
# Compute n_hat(i-1,t+1), which estimates E_(t+1)[p_(t+i)^(1)]
n_hat_2 <- compute_n_hat(yields, term_premia, i = 2)
n_hat_2_t_plus_1 <- c(n_hat_2[-1], NA) # forward one period
# Compute n_hat(i-1,t+1) - n_hat(i,t), which estimates \Delta_(t+1)p_(t+i)^(1)
price_news_3_manual <- n_hat_2 - n_hat_3_t_minus_1
error <- price_news_3 - price_news_3_manual[-1]
cat(sprintf(
  paste0(
    "Error in price_news with i=3 (price_news_3 - manually computed value):",
    "%.4f\n"
  ),
  max(abs(1e4 * error))
)) # should be zero

# Time series properties
cat("\nTime series properties\n")
price_news_7 <- compute_price_news(yields, term_premia, i = 7)
acf_1 <- cor(
  price_news_7[-length(price_news_7)],
  price_news_7[-1],
  use = "complete.obs"
)
cat(sprintf("  Autocorrelation at lag 1: %.3f\n", acf_1))
cat(sprintf("  Note: Shows persistence due to highly persistent n_hat values (AR(1) > 0.99)\n"))

# Predictability analysis
cat("\nPredictability analysis\n")
# Regress on lagged values
n <- length(price_news_5)
y <- price_news_5[2:n]
x <- price_news_5[1:(n - 1)]
reg <- lm(y ~ x)
r_squared <- summary(reg)$r.squared

cat(sprintf("  R² from AR(1) regression: %.4f\n", r_squared))
cat(sprintf("  Note: ~20%% R² reflects persistence inherited from n_hat components\n"))
cat(sprintf("  True 'news' would have R² ≈ 0, but empirical bond prices are highly persistent\n"))

cat("\nTest complete!\n")
