# Simple test of date functionality
library(hetid)

# Load ACM data
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]
dates <- acm_data$date

cat("Testing date functionality\n")
cat("=========================\n\n")

# Test: Check dates format
cat("Date information:\n")
cat(sprintf("  Class: %s\n", class(dates)[1]))
cat(sprintf("  First date: %s\n", as.character(dates[1])))
cat(sprintf("  Last date: %s\n", as.character(dates[length(dates)])))
cat(sprintf("  Total observations: %d\n\n", length(dates)))

# Test: compute_n_hat with dates
n_hat_df <- compute_n_hat(yields, term_premia, i = 5, dates = dates, return_df = TRUE)
cat("compute_n_hat with dates:\n")
cat(sprintf(
  "  First 3 dates: %s\n",
  paste(as.character(n_hat_df$date[1:3]), collapse = ", ")
))
cat(sprintf("  Date class: %s\n\n", class(n_hat_df$date)[1]))

# Test: compute_price_news with dates
price_news_df <- compute_price_news(yields, term_premia, i = 5, dates = dates, return_df = TRUE)
cat("compute_price_news with dates:\n")
cat(sprintf(
  "  First 3 dates: %s\n",
  paste(as.character(price_news_df$date[1:3]), collapse = ", ")
))
cat(sprintf(
  "  First value is NA: %s\n",
  ifelse(is.na(price_news_df$price_news[1]), "Yes", "No")
))
cat(sprintf("  Second value: %.4f\n\n", price_news_df$price_news[2]))

# Test: compute_sdf_innovations with dates
sdf_df <- compute_sdf_innovations(yields, term_premia, i = 5, dates = dates, return_df = TRUE)
cat("compute_sdf_innovations with dates:\n")
cat(sprintf(
  "  First 3 dates: %s\n",
  paste(as.character(sdf_df$date[1:3]), collapse = ", ")
))
cat(sprintf(
  "  Date alignment matches price_news: %s\n",
  ifelse(all(sdf_df$date == price_news_df$date), "Yes", "No")
))

cat("\nTest completed successfully!\n")
