# Analysis of positive n_hat_1 values
library(hetid)

# Load ACM data with dates
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]
dates <- acm_data$date

# Compute n_hat for i=1
n_hat_1 <- compute_n_hat(yields, term_premia, i = 1)

# Find positive values
positive_idx <- which(n_hat_1 > 0)
cat(sprintf(
  "Number of positive n_hat_1 values: %d out of %d (%.1f%%)\n",
  length(positive_idx), length(n_hat_1),
  100 * length(positive_idx) / length(n_hat_1)
))

if (length(positive_idx) > 0) {
  cat("\nDates with positive n_hat_1:\n")
  cat("============================\n")

  # Show first few and last few positive occurrences
  show_idx <- c(
    head(positive_idx, 10),
    if (length(positive_idx) > 20) tail(positive_idx, 10)
  )
  show_idx <- unique(show_idx)

  for (idx in show_idx) {
    cat(sprintf(
      "%s: n_hat_1 = %6.4f, y1 = %5.2f%%, y2 = %5.2f%%, tp1 = %5.2f%%, tp2 = %5.2f%%\n",
      as.character(dates[idx]), n_hat_1[idx],
      yields$y1[idx], yields$y2[idx],
      term_premia$tp1[idx], term_premia$tp2[idx]
    ))

    if (idx == show_idx[10] && length(positive_idx) > 20) {
      cat("...\n")
    }
  }

  # Analyze the formula components
  cat("\nAnalyzing formula: n_hat_1 = 1*y1 - 2*y2 + 2*tp2 - 1*tp1\n")
  cat("For n_hat_1 > 0, we need: y1 + 2*tp2 > 2*y2 + tp1\n")
  cat("Rearranging: y1 - y2 > y2 - 2*tp2 + tp1\n\n")

  # Look at specific periods
  cat("Period analysis:\n")

  # Find contiguous periods of positive values
  runs <- rle(n_hat_1 > 0)
  positive_runs <- which(runs$values)

  if (length(positive_runs) > 0) {
    cat("\nContiguous periods with positive n_hat_1:\n")
    start_idx <- 1
    for (i in 1:length(runs$lengths)) {
      if (runs$values[i]) {
        end_idx <- start_idx + runs$lengths[i] - 1
        cat(sprintf(
          "  %s to %s (%d months)\n",
          as.character(dates[start_idx]),
          as.character(dates[end_idx]),
          runs$lengths[i]
        ))
      }
      start_idx <- start_idx + runs$lengths[i]
    }
  }

  # Check for yield curve inversions
  cat("\nYield curve analysis at positive n_hat_1 dates:\n")
  inversions <- yields$y1[positive_idx] > yields$y2[positive_idx]
  cat(sprintf(
    "  Yield curve inverted (y1 > y2): %d out of %d positive cases (%.1f%%)\n",
    sum(inversions), length(positive_idx), 100 * sum(inversions) / length(positive_idx)
  ))

  # Check term premia patterns
  cat("\nTerm premia analysis:\n")
  tp_diff <- term_premia$tp2[positive_idx] - term_premia$tp1[positive_idx]
  cat(sprintf("  Average tp2 - tp1 when n_hat_1 > 0: %.4f%%\n", mean(tp_diff)))
  cat(sprintf(
    "  Average tp2 - tp1 overall: %.4f%%\n",
    mean(term_premia$tp2 - term_premia$tp1, na.rm = TRUE)
  ))

  # Economic interpretation
  cat("\nEconomic interpretation:\n")
  cat("n_hat_1 estimates -E_t[y_{t+1}^{(1)}], the negative of expected 1-year yield next period.\n")
  cat("Positive n_hat_1 implies E_t[y_{t+1}^{(1)}] < 0, meaning negative expected short rates.\n")
  cat("This typically occurs when:\n")
  cat("1. The yield curve is inverted (recession expectations)\n")
  cat("2. Term premia at 2-year maturity are unusually high relative to 1-year\n")
  cat("3. Market expects significant monetary policy easing\n")
}

# Plot if needed
if (length(positive_idx) > 0) {
  library(ggplot2)

  # Create data for plotting
  plot_data <- data.frame(
    date = dates,
    n_hat_1 = n_hat_1,
    y1 = yields$y1,
    y2 = yields$y2,
    spread_2_1 = yields$y2 - yields$y1,
    tp1 = term_premia$tp1,
    tp2 = term_premia$tp2,
    is_positive = n_hat_1 > 0
  )

  # Plot n_hat_1 over time
  p1 <- ggplot(plot_data, aes(x = date, y = n_hat_1)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_point(
      data = plot_data[plot_data$is_positive, ],
      aes(x = date, y = n_hat_1), color = "red", size = 2
    ) +
    labs(
      title = "n_hat_1 Over Time (Red points indicate positive values)",
      x = "Date", y = "n_hat_1"
    ) +
    theme_minimal()

  # Save plot
  ggsave("n_hat_1_analysis.png", p1, width = 12, height = 6)
  cat("\nPlot saved as 'n_hat_1_analysis.png'\n")
}
