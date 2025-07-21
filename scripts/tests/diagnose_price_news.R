# =============================================================================
# Diagnostic: Price News Predictability Analysis
# =============================================================================
# This script investigates why price news shows unexpected predictability
# (high autocorrelation and R²)

library(hetid)
library(ggplot2)

cat("Price News Predictability Diagnostic\n")
cat("====================================\n\n")

# Load data
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Function to analyze predictability
analyze_predictability <- function(i) {
  cat(sprintf("\n--- Maturity i=%d ---\n", i))

  # Compute price news
  price_news <- compute_price_news(yields, term_premia, i = i)

  # Remove NAs
  price_news_clean <- na.omit(price_news)
  n <- length(price_news_clean)

  # 1. Basic statistics
  cat(sprintf("Number of observations: %d\n", n))
  cat(sprintf("Mean: %.6f\n", mean(price_news_clean)))
  cat(sprintf("SD: %.6f\n", sd(price_news_clean)))

  # 2. Autocorrelation analysis
  cat("\nAutocorrelation analysis:\n")
  acf_result <- acf(price_news_clean, lag.max = 10, plot = FALSE)
  for (lag in 1:5) {
    cat(sprintf("  Lag %d: %.3f\n", lag, acf_result$acf[lag + 1]))
  }

  # 3. AR(1) regression
  ar1_model <- lm(price_news_clean[2:n] ~ price_news_clean[1:(n - 1)])
  ar1_summary <- summary(ar1_model)
  cat(sprintf("\nAR(1) regression:\n"))
  cat(sprintf(
    "  Coefficient: %.4f (p-value: %.4f)\n",
    coef(ar1_model)[2],
    ar1_summary$coefficients[2, 4]
  ))
  cat(sprintf("  R²: %.4f\n", ar1_summary$r.squared))

  # 4. Check components
  cat("\nDecomposing price news calculation:\n")

  # Get n_hat values
  n_hat_i <- compute_n_hat(yields, term_premia, i)
  if (i == 1) {
    n_hat_i_minus_1 <- -yields$y1 / 100
  } else {
    n_hat_i_minus_1 <- compute_n_hat(yields, term_premia, i - 1)
  }

  # Check persistence of n_hat
  n_hat_i_clean <- na.omit(n_hat_i)
  n_i <- length(n_hat_i_clean)
  n_hat_ar1 <- lm(n_hat_i_clean[2:n_i] ~ n_hat_i_clean[1:(n_i - 1)])
  cat(sprintf("  n_hat(%d) AR(1) coefficient: %.4f\n", i, coef(n_hat_ar1)[2]))
  cat(sprintf("  n_hat(%d) R²: %.4f\n", i, summary(n_hat_ar1)$r.squared))

  if (i > 1) {
    n_hat_im1_clean <- na.omit(n_hat_i_minus_1)
    n_im1 <- length(n_hat_im1_clean)
    n_hat_im1_ar1 <- lm(n_hat_im1_clean[2:n_im1] ~ n_hat_im1_clean[1:(n_im1 - 1)])
    cat(sprintf("  n_hat(%d) AR(1) coefficient: %.4f\n", i - 1, coef(n_hat_im1_ar1)[2]))
    cat(sprintf("  n_hat(%d) R²: %.4f\n", i - 1, summary(n_hat_im1_ar1)$r.squared))
  }

  # 5. Verify the price news formula
  cat("\nVerifying price news formula:\n")
  # price_news[t] should equal n_hat(i-1,t+1) - n_hat(i,t)
  manual_price_news <- rep(NA, length(n_hat_i) - 1)
  for (t in 1:(length(n_hat_i) - 1)) {
    if (!is.na(n_hat_i_minus_1[t + 1]) && !is.na(n_hat_i[t])) {
      manual_price_news[t] <- n_hat_i_minus_1[t + 1] - n_hat_i[t]
    }
  }

  # Compare
  valid_idx <- !is.na(price_news) & !is.na(manual_price_news)
  max_diff <- max(abs(price_news[valid_idx] - manual_price_news[valid_idx]))
  cat(sprintf("  Max difference between computed and manual: %.2e\n", max_diff))

  # Return results for plotting
  invisible(list(
    price_news = price_news_clean,
    acf = acf_result,
    ar1_coef = coef(ar1_model)[2],
    ar1_r2 = ar1_summary$r.squared
  ))
}

# Analyze multiple maturities
maturities_to_test <- c(1, 3, 5, 7, 9)
results <- list()

for (i in maturities_to_test) {
  results[[paste0("i_", i)]] <- analyze_predictability(i)
}

# Summary plot
cat("\n\nSummary of AR(1) coefficients and R² by maturity:\n")
ar1_coefs <- sapply(results, function(x) x$ar1_coef)
ar1_r2s <- sapply(results, function(x) x$ar1_r2)

summary_df <- data.frame(
  Maturity = maturities_to_test,
  AR1_Coefficient = ar1_coefs,
  R_squared = ar1_r2s
)

print(summary_df)

# Plot autocorrelation functions
par(mfrow = c(2, 3))
for (i in 1:length(maturities_to_test)) {
  mat <- maturities_to_test[i]
  result <- results[[paste0("i_", mat)]]
  plot(result$acf, main = paste("ACF for Price News, i =", mat))
}

# Additional analysis: Check if the issue is with overlapping periods
cat("\n\nOverlapping periods analysis:\n")
cat("Price news at time t depends on n_hat(i-1,t+1) and n_hat(i,t)\n")
cat("This creates overlap when we look at consecutive price news values\n")

# Test with non-overlapping observations
i <- 5
price_news_5 <- compute_price_news(yields, term_premia, i = i)
price_news_5_clean <- na.omit(price_news_5)

# Use every i-th observation to avoid overlap
non_overlap_indices <- seq(1, length(price_news_5_clean), by = i)
price_news_non_overlap <- price_news_5_clean[non_overlap_indices]

if (length(price_news_non_overlap) > 10) {
  n_no <- length(price_news_non_overlap)
  ar1_no <- lm(price_news_non_overlap[2:n_no] ~ price_news_non_overlap[1:(n_no - 1)])

  cat(sprintf("\nNon-overlapping analysis (using every %d-th observation):\n", i))
  cat(sprintf("  Number of observations: %d\n", n_no))
  cat(sprintf("  AR(1) coefficient: %.4f\n", coef(ar1_no)[2]))
  cat(sprintf("  R²: %.4f\n", summary(ar1_no)$r.squared))

  acf_no <- acf(price_news_non_overlap, lag.max = 5, plot = FALSE)
  cat("  Autocorrelations:\n")
  for (lag in 1:min(5, length(acf_no$acf) - 1)) {
    cat(sprintf("    Lag %d: %.3f\n", lag, acf_no$acf[lag + 1]))
  }
}

cat("\n\nDiagnostic complete!\n")
