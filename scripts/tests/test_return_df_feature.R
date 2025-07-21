# =============================================================================
# Test: return_df Parameter for Time Series Functions
# =============================================================================
# This script tests the new return_df parameter that allows functions to
# return data frames with dates instead of just numeric vectors.

library(hetid)

cat("Testing return_df parameter for time series functions\n")
cat("===================================================\n\n")

# Load ACM data
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Check if data has dates
has_dates <- !is.null(rownames(yields))
cat(sprintf("Input data has dates: %s\n", ifelse(has_dates, "Yes", "No")))
if (has_dates) {
  cat(sprintf(
    "Date range: %s to %s\n", rownames(yields)[1],
    rownames(yields)[nrow(yields)]
  ))
  cat(sprintf("Number of observations: %d\n\n", nrow(yields)))
}

# =============================================================================
# Test: compute_n_hat with return_df
# =============================================================================
cat("Test: compute_n_hat()\n")
cat("-----------------------\n")

# Standard output (numeric vector)
n_hat_5_vec <- compute_n_hat(yields, term_premia, i = 5, return_df = FALSE)
cat(sprintf("  Standard output length: %d\n", length(n_hat_5_vec)))
cat(sprintf(
  "  First 3 values: %s\n",
  paste(round(n_hat_5_vec[1:3], 4), collapse = ", ")
))

# Data frame output
n_hat_5_df <- compute_n_hat(yields, term_premia, i = 5, return_df = TRUE)
cat(sprintf(
  "  Data frame dimensions: %d x %d\n",
  nrow(n_hat_5_df), ncol(n_hat_5_df)
))
cat(sprintf("  Column names: %s\n", paste(names(n_hat_5_df), collapse = ", ")))
cat(sprintf(
  "  First 3 dates: %s\n",
  paste(n_hat_5_df$date[1:3], collapse = ", ")
))

# Verify values match
values_match <- all.equal(n_hat_5_vec, n_hat_5_df$n_hat)
cat(sprintf("  Values match: %s\n\n", ifelse(values_match, "Yes", "No")))

# =============================================================================
# Test: compute_price_news with return_df
# =============================================================================
cat("Test: compute_price_news()\n")
cat("----------------------------\n")

# Standard output
price_news_5_vec <- compute_price_news(yields, term_premia, i = 5, return_df = FALSE)
cat(sprintf("  Standard output length: %d\n", length(price_news_5_vec)))
cat(sprintf("  Note: Length is n-1 due to differencing\n"))

# Data frame output
price_news_5_df <- compute_price_news(yields, term_premia, i = 5, return_df = TRUE)
cat(sprintf(
  "  Data frame dimensions: %d x %d\n",
  nrow(price_news_5_df), ncol(price_news_5_df)
))
cat(sprintf("  Note: Same length as input, with first observation NA\n"))

# Check alignment
cat(sprintf(
  "  First observation is NA: %s\n",
  ifelse(is.na(price_news_5_df$price_news[1]), "Yes", "No")
))
cat(sprintf(
  "  Non-NA values start at row 2: %s\n",
  ifelse(!is.na(price_news_5_df$price_news[2]), "Yes", "No")
))

# Verify values match (excluding the first NA)
values_match <- all.equal(price_news_5_vec,
  price_news_5_df$price_news[-1],
  check.attributes = FALSE
)
cat(sprintf(
  "  Values match (excluding first NA): %s\n\n",
  ifelse(values_match, "Yes", "No")
))

# Show alignment
if (has_dates) {
  cat("  Date alignment example:\n")
  cat(sprintf(
    "    Row 1: Date = %s, price_news = %s\n",
    price_news_5_df$date[1],
    ifelse(is.na(price_news_5_df$price_news[1]), "NA",
      round(price_news_5_df$price_news[1], 4)
    )
  ))
  cat(sprintf(
    "    Row 2: Date = %s, price_news = %.4f\n",
    price_news_5_df$date[2],
    price_news_5_df$price_news[2]
  ))
  cat("\n")
}

# =============================================================================
# Test: compute_sdf_innovations with return_df
# =============================================================================
cat("Test: compute_sdf_innovations()\n")
cat("---------------------------------\n")

# Standard output
sdf_innov_5_vec <- compute_sdf_innovations(yields, term_premia, i = 5, return_df = FALSE)
cat(sprintf("  Standard output length: %d\n", length(sdf_innov_5_vec)))

# Data frame output
sdf_innov_5_df <- compute_sdf_innovations(yields, term_premia, i = 5, return_df = TRUE)
cat(sprintf(
  "  Data frame dimensions: %d x %d\n",
  nrow(sdf_innov_5_df), ncol(sdf_innov_5_df)
))

# Check alignment
cat(sprintf(
  "  First observation is NA: %s\n",
  ifelse(is.na(sdf_innov_5_df$sdf_innovations[1]), "Yes", "No")
))

# Verify values match
values_match <- all.equal(sdf_innov_5_vec,
  sdf_innov_5_df$sdf_innovations[-1],
  check.attributes = FALSE
)
cat(sprintf(
  "  Values match (excluding first NA): %s\n\n",
  ifelse(values_match, "Yes", "No")
))

# =============================================================================
# Test: Yield news option with return_df
# =============================================================================
cat("Test: Yield news with return_df\n")
cat("----------------------------------\n")

# Get yield news
yield_news_5_df <- compute_price_news(yields, term_premia,
  i = 5,
  return_yield_news = TRUE,
  return_df = TRUE
)

# Compare with price news
price_news_5_df <- compute_price_news(yields, term_premia,
  i = 5,
  return_yield_news = FALSE,
  return_df = TRUE
)

# Check they are negatives of each other
valid_idx <- !is.na(yield_news_5_df$price_news) & !is.na(price_news_5_df$price_news)
sum_check <- sum(yield_news_5_df$price_news[valid_idx] +
  price_news_5_df$price_news[valid_idx])

cat(sprintf(
  "  Yield news = -Price news: %s\n",
  ifelse(abs(sum_check) < 1e-10, "Yes", "No")
))

# =============================================================================
# Test: Data without dates
# =============================================================================
cat("\nTest: Data without dates\n")
cat("---------------------------\n")

# Remove rownames to simulate data without dates
yields_no_dates <- yields
rownames(yields_no_dates) <- NULL
term_premia_no_dates <- term_premia
rownames(term_premia_no_dates) <- NULL

# Test with no dates
n_hat_no_dates <- compute_n_hat(yields_no_dates, term_premia_no_dates,
  i = 5, return_df = TRUE
)
cat(sprintf(
  "  Generic date indices created: %s\n",
  paste(n_hat_no_dates$date[1:3], collapse = ", ")
))

cat("\nAll tests completed!\n")
