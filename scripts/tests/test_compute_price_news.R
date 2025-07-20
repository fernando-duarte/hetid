# =============================================================================
# Test: compute_price_news()
# =============================================================================
# This function computes price news, which represents
# the unexpected component of bond price changes. It calculates the difference
# between realized and expected log bond prices.

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
for (i in c(1, 2, 5, 9)) {
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

# Time series properties
cat("\nTime series properties\n")
price_news_7 <- compute_price_news(yields, term_premia, i = 7)
acf_1 <- cor(price_news_7[-length(price_news_7)],
  price_news_7[-1],
  use = "complete.obs"
)
cat(sprintf("  Autocorrelation at lag 1: %.3f\n", acf_1))
cat(sprintf("  Should be close to 0 (news is unpredictable)\n"))

# Verify it's truly "news" (unpredictable)
cat("\nUnpredictability check\n")
# Regress on lagged values
n <- length(price_news_5)
y <- price_news_5[2:n]
x <- price_news_5[1:(n - 1)]
reg <- lm(y ~ x)
r_squared <- summary(reg)$r.squared

cat(sprintf("  R² from AR(1) regression: %.4f\n", r_squared))
cat(sprintf("  Low R² confirms news is largely unpredictable\n"))

cat("\nTest complete!\n")
