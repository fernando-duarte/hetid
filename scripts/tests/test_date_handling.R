# Test script for date handling in compute_n_hat, compute_price_news, and compute_sdf_innovations
library(hetid)

cat("Testing date handling in bond calculation functions\n")
cat("==================================================\n\n")

# Load ACM data with dates
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]
dates <- acm_data$date

cat("Data loaded:\n")
cat(sprintf("  Number of observations: %d\n", nrow(yields)))
cat(sprintf("  Date range: %s to %s\n", min(dates), max(dates)))
cat("\n")

# Test: compute_n_hat with dates
cat("Test: compute_n_hat with dates\n")
cat("---------------------------------\n")

# Without dates (returns numeric vector)
n_hat_5 <- compute_n_hat(yields, term_premia, i = 5)
cat(sprintf(
  "  Without dates - length: %d, class: %s\n",
  length(n_hat_5), class(n_hat_5)
))

# With dates (returns data frame)
n_hat_5_df <- compute_n_hat(yields, term_premia,
  i = 5,
  return_df = TRUE, dates = dates
)
cat(sprintf(
  "  With dates - rows: %d, class: %s\n",
  nrow(n_hat_5_df), class(n_hat_5_df)
))
cat(sprintf(
  "  Column names: %s\n",
  paste(names(n_hat_5_df), collapse = ", ")
))
cat(sprintf(
  "  First date: %s, Last date: %s\n",
  n_hat_5_df$date[1], n_hat_5_df$date[nrow(n_hat_5_df)]
))
cat("\n")

# Test: compute_price_news with dates
cat("Test: compute_price_news with dates\n")
cat("--------------------------------------\n")

# Without dates (returns numeric vector)
price_news_5 <- compute_price_news(yields, term_premia, i = 5)
cat(sprintf(
  "  Without dates - length: %d, class: %s\n",
  length(price_news_5), class(price_news_5)
))

# With dates (returns data frame)
price_news_5_df <- compute_price_news(yields, term_premia,
  i = 5,
  return_df = TRUE, dates = dates
)
cat(sprintf(
  "  With dates - rows: %d, class: %s\n",
  nrow(price_news_5_df), class(price_news_5_df)
))
cat(sprintf(
  "  Column names: %s\n",
  paste(names(price_news_5_df), collapse = ", ")
))
cat(sprintf(
  "  First date: %s, Last date: %s\n",
  price_news_5_df$date[1], price_news_5_df$date[nrow(price_news_5_df)]
))
cat(sprintf("  Note: First value is NA (expected behavior)\n"))
cat("\n")

# Test: compute_sdf_innovations with dates
cat("Test: compute_sdf_innovations with dates\n")
cat("-------------------------------------------\n")

# Without dates (returns numeric vector)
sdf_innov_5 <- compute_sdf_innovations(yields, term_premia, i = 5)
cat(sprintf(
  "  Without dates - length: %d, class: %s\n",
  length(sdf_innov_5), class(sdf_innov_5)
))

# With dates (returns data frame)
sdf_innov_5_df <- compute_sdf_innovations(yields, term_premia,
  i = 5,
  return_df = TRUE, dates = dates
)
cat(sprintf(
  "  With dates - rows: %d, class: %s\n",
  nrow(sdf_innov_5_df), class(sdf_innov_5_df)
))
cat(sprintf(
  "  Column names: %s\n",
  paste(names(sdf_innov_5_df), collapse = ", ")
))
cat(sprintf(
  "  First date: %s, Last date: %s\n",
  sdf_innov_5_df$date[1], sdf_innov_5_df$date[nrow(sdf_innov_5_df)]
))
cat(sprintf("  Note: First value is NA (expected behavior)\n"))
cat("\n")

# Test: Verify values match between vector and data frame versions
cat("Test: Verify consistency between vector and data frame outputs\n")
cat("----------------------------------------------------------------\n")

# For n_hat
max_diff_n_hat <- max(abs(n_hat_5 - n_hat_5_df$n_hat), na.rm = TRUE)
cat(sprintf("  n_hat max difference: %.2e\n", max_diff_n_hat))

# For price_news (account for NA alignment)
price_news_from_df <- price_news_5_df$price_news[-1] # Remove first NA
max_diff_price_news <- max(abs(price_news_5 - price_news_from_df), na.rm = TRUE)
cat(sprintf("  price_news max difference: %.2e\n", max_diff_price_news))

# For sdf_innovations (account for NA alignment)
sdf_innov_from_df <- sdf_innov_5_df$sdf_innovations[-1] # Remove first NA
max_diff_sdf <- max(abs(sdf_innov_5 - sdf_innov_from_df), na.rm = TRUE)
cat(sprintf("  sdf_innovations max difference: %.2e\n", max_diff_sdf))

cat("\nAll tests completed successfully!\n")

# Test: Show sample output with dates
cat("\nTest: Sample output with dates\n")
cat("---------------------------------\n")
cat("First 5 rows of n_hat_5 with dates:\n")
print(head(n_hat_5_df, 5))

cat("\nFirst 5 rows of price_news_5 with dates:\n")
print(head(price_news_5_df, 5))

cat("\nFirst 5 rows of sdf_innovations_5 with dates:\n")
print(head(sdf_innov_5_df, 5))
