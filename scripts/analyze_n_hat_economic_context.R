# Detailed economic context analysis for positive n_hat_1 values
library(hetid)

# Load data
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]
dates <- acm_data$date

# Compute n_hat_1
n_hat_1 <- compute_n_hat(yields, term_premia, i = 1)

# Find positive values
positive_idx <- which(n_hat_1 > 0)

cat("ECONOMIC CONTEXT FOR POSITIVE n_hat_1 VALUES\n")
cat("============================================\n\n")

# Key economic periods
cat("Historical Context of Positive n_hat_1 Dates:\n\n")

cat("1. November 2008 (Financial Crisis Peak):\n")
cat("   - Lehman Brothers had collapsed in September 2008\n")
cat("   - Fed funds rate was being aggressively cut (reached 0-0.25% in Dec 2008)\n")
cat("   - Market expected continued rate cuts and extraordinary monetary policy\n")
cat("   - Flight to quality drove Treasury yields to historic lows\n\n")

cat("2. July-November 2010 (Post-Crisis Recovery Concerns):\n")
cat("   - Fed funds rate at zero lower bound (0-0.25%)\n")
cat("   - QE1 had ended, QE2 was being discussed/implemented\n")
cat("   - Concerns about double-dip recession\n")
cat("   - European debt crisis emerging\n\n")

cat("3. January & May-August 2011 (Operation Twist Period):\n")
cat("   - Fed still at zero lower bound\n")
cat("   - Operation Twist announced (selling short-term, buying long-term Treasuries)\n")
cat("   - S&P downgraded US credit rating in August 2011\n")
cat("   - Heightened uncertainty about economic recovery\n\n")

cat("4. November 2013 (Taper Tantrum Aftermath):\n")
cat("   - Fed discussing tapering of QE3\n")
cat("   - Markets adjusting to potential end of extraordinary accommodation\n")
cat("   - But short rates still expected to stay low for extended period\n\n")

# Analyze the mathematical mechanism
cat("\nMATHEMATICAL MECHANISM:\n")
cat("======================\n")
cat("n_hat_1 = y1 - 2*y2 + 2*tp2 - tp1\n\n")

for (idx in positive_idx[1:min(3, length(positive_idx))]) {
  y1 <- yields$y1[idx] / 100
  y2 <- yields$y2[idx] / 100
  tp1 <- term_premia$tp1[idx] / 100
  tp2 <- term_premia$tp2[idx] / 100

  cat(sprintf("Example: %s\n", as.character(dates[idx])))
  cat(sprintf("  y1 = %.4f, y2 = %.4f, tp1 = %.4f, tp2 = %.4f\n", y1, y2, tp1, tp2))
  cat(sprintf("  Component breakdown:\n"))
  cat(sprintf("    y1         = %+.4f\n", y1))
  cat(sprintf("    -2*y2      = %+.4f\n", -2 * y2))
  cat(sprintf("    +2*tp2     = %+.4f\n", 2 * tp2))
  cat(sprintf("    -tp1       = %+.4f\n", -tp1))
  cat(sprintf("    Total      = %+.4f\n\n", n_hat_1[idx]))
}

# Key insight
cat("KEY INSIGHT:\n")
cat("============\n")
cat("Positive n_hat_1 occurs when term premia effects dominate yield curve effects.\n")
cat("Specifically, when 2*(tp2 - y2) > (tp1 - y1)\n\n")

cat("This happens during periods of extreme monetary accommodation when:\n")
cat("1. Short rates are at or near zero (y1 very low)\n")
cat("2. 2-year yields are also very low (y2 low)\n")
cat("3. Term premia reflect compensation for holding longer-term bonds\n")
cat("4. The term structure of term premia (tp2 > tp1) is steep enough\n\n")

cat("ECONOMIC INTERPRETATION:\n")
cat("=======================\n")
cat("n_hat_1 > 0 means the market expects the 1-year rate next year to be negative.\n")
cat("While nominal rates can't go (significantly) negative, this reflects:\n")
cat("1. Extreme monetary policy accommodation expectations\n")
cat("2. Technical factors in bond pricing models\n")
cat("3. Risk premium effects dominating pure rate expectations\n")
cat("4. Model-implied 'shadow rates' that can be negative\n\n")

# Check relationship with actual future rates
cat("VALIDATION: What actually happened to rates?\n")
cat("==========================================\n")
for (i in 1:min(3, length(positive_idx))) {
  idx <- positive_idx[i]
  date_current <- dates[idx]

  # Find the index one year later (approximately)
  future_idx <- which(dates >= date_current + 365)[1]

  if (!is.na(future_idx) && future_idx <= length(dates)) {
    cat(sprintf(
      "%s: n_hat_1 = %.4f (predicts 1y rate in 1 year = %.4f%%)\n",
      as.character(date_current), n_hat_1[idx], -n_hat_1[idx] * 100
    ))
    cat(sprintf(
      "  Actual 1y rate on %s: %.2f%%\n\n",
      as.character(dates[future_idx]), yields$y1[future_idx]
    ))
  }
}
