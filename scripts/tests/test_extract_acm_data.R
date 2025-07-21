# =============================================================================
# Test: extract_acm_data()
# =============================================================================
# This function extracts ACM (Adrian, Crump, and Moench) term structure data
# including yields, term premia, and risk-neutral yields. It can filter by
# date range, maturity, and frequency (monthly/quarterly).

# Load package
library(hetid)

cat("Testing extract_acm_data() function\n")
cat("===================================\n\n")

# Test: Basic extraction with defaults
cat("Default extraction (all data types)\n")
acm_default <- extract_acm_data()

cat(sprintf(
  "  Dimensions: %d rows × %d columns\n",
  nrow(acm_default), ncol(acm_default)
))
cat(sprintf(
  "  Date range: %s to %s\n",
  min(acm_default$date), max(acm_default$date)
))
cat(sprintf(
  "  Column names: %s\n",
  paste(names(acm_default)[1:5], collapse = ", ")
))
cat("  ...\n")

# Test: Specific data types
cat("\nExtracting specific data types\n")
# Yields only
yields_only <- extract_acm_data(data_types = "yields")
cat(sprintf("  Yields only: %d columns\n", ncol(yields_only)))

# Term premia only
tp_only <- extract_acm_data(data_types = "term_premia")
cat(sprintf("  Term premia only: %d columns\n", ncol(tp_only)))

# Multiple types
yields_tp <- extract_acm_data(data_types = c("yields", "term_premia"))
cat(sprintf("  Yields + term premia: %d columns\n", ncol(yields_tp)))

# Test: Maturity filtering
cat("\nFiltering by maturity\n")
mat_subset <- extract_acm_data(
  data_types = "yields",
  maturities = c(1, 5, 10)
)
cat(sprintf("  Requested maturities: 1, 5, 10\n"))
cat(sprintf(
  "  Columns returned: %s\n",
  paste(names(mat_subset)[-1], collapse = ", ")
))

# Test: Date filtering
cat("\nDate range filtering\n")
date_filtered <- extract_acm_data(
  start_date = "2010-01-01",
  end_date = "2020-12-31"
)
cat(sprintf("  Requested: 2010-01-01 to 2020-12-31\n"))
cat(sprintf(
  "  Actual range: %s to %s\n",
  min(date_filtered$date), max(date_filtered$date)
))
cat(sprintf("  Observations: %d\n", nrow(date_filtered)))

# Test: Frequency conversion
cat("\nFrequency conversion\n")
monthly_data <- extract_acm_data(frequency = "monthly")
quarterly_data <- extract_acm_data(frequency = "quarterly")

cat(sprintf("  Monthly observations: %d\n", nrow(monthly_data)))
cat(sprintf("  Quarterly observations: %d\n", nrow(quarterly_data)))
cat(sprintf(
  "  Ratio: %.2f (should be ~3)\n",
  nrow(monthly_data) / nrow(quarterly_data)
))

# Show sample of quarterly dates
cat("  Sample quarterly dates:\n")
for (i in 1:5) {
  cat(sprintf("    %s\n", quarterly_data$date[i]))
}

# Test: Auto-download feature
cat("\nAuto-download feature\n")
# This would download if file doesn't exist
result <- extract_acm_data(auto_download = TRUE)
cat(sprintf("  Data loaded: %s\n", ifelse(nrow(result) > 0, "Yes", "No")))

# Test: Data integrity checks
cat("\nData integrity checks\n")
full_data <- extract_acm_data(data_types = c("yields", "term_premia", "risk_neutral"))

# Check for missing values
n_missing <- sum(is.na(full_data))
cat(sprintf(
  "  Missing values: %d (%.2f%%)\n",
  n_missing, n_missing / length(full_data) * 100
))

# Check yield ordering (should increase with maturity)
yields <- full_data[100, grep("^y\\d+$", names(full_data))]
is_ordered <- all(diff(as.numeric(yields)) > 0)
cat(sprintf(
  "  Yields increase with maturity: %s\n",
  ifelse(is_ordered, "✓ Yes", "✗ No")
))

# Check term premium signs (usually positive for longer maturities)
tp_means <- colMeans(full_data[, grep("^tp", names(full_data))], na.rm = TRUE)
cat(sprintf("  Mean term premium at 10Y: %.3f%%\n", tp_means["tp10"]))

# Test: Risk-neutral yields
cat("\nRisk-neutral yields\n")
rn_data <- extract_acm_data(data_types = "risk_neutral")
rn_cols <- grep("^rn", names(rn_data))

# Risk-neutral yield = actual yield - term premium
# Check this relationship
sample_row <- 200
# Need to get all data types to check the relationship
check_data <- extract_acm_data(data_types = c("yields", "term_premia", "risk_neutral"))
y5 <- check_data[sample_row, "y5"]
tp5 <- check_data[sample_row, "tp5"]
rn5 <- check_data[sample_row, "rn5"]

cat(sprintf("  Sample check (row %d, maturity 5Y):\n", sample_row))
cat(sprintf("    Actual yield: %.3f%%\n", y5))
cat(sprintf("    Term premium: %.3f%%\n", tp5))
cat(sprintf("    Risk-neutral yield: %.3f%%\n", rn5))
cat(sprintf("    y - rn = %.3f%% (should equal tp)\n", y5 - rn5))

cat("\nTest complete!\n")
