# =============================================================================
# Test: compute_w2_residuals() (formerly compute_reduced_form_residual_y2)
# =============================================================================
# This function computes reduced form residuals for Y2 (SDF innovations) by
# regressing SDF innovations on lagged principal components. It can compute
# residuals for multiple maturities simultaneously.

# Load package
library(hetid)

cat("Testing compute_w2_residuals() function\n")
cat("===================================================\n\n")

# Load data
data("variables")
# Use quarterly ACM data to match the frequency of variables (PCs)
acm_data <- extract_acm_data(
  data_types = c("yields", "term_premia"),
  frequency = "quarterly"
)
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Check dimensions and align data if needed
cat(sprintf("Variables (PCs) rows: %d\n", nrow(variables)))
cat(sprintf("ACM data rows: %d\n", nrow(acm_data)))

# Align the data by matching quarterly periods
if (nrow(variables) != nrow(acm_data)) {
  cat("\nAligning data by quarterly period...\n")

  # Create year-quarter identifiers
  variables$year_quarter <- paste0(format(variables$date, "%Y"), "-Q", quarters(variables$date))
  acm_data$year_quarter <- paste0(format(acm_data$date, "%Y"), "-Q", quarters(acm_data$date))

  # Find common quarters
  common_quarters <- intersect(variables$year_quarter, acm_data$year_quarter)
  cat(sprintf("Common quarters: %d\n", length(common_quarters)))

  # Subset both datasets to common quarters
  variables_indices <- which(variables$year_quarter %in% common_quarters)
  acm_indices <- which(acm_data$year_quarter %in% common_quarters)

  variables_aligned <- variables[variables_indices, ]
  yields <- yields[acm_indices, ]
  term_premia <- term_premia[acm_indices, ]

  # Extract aligned PCs
  pcs_aligned <- as.matrix(variables_aligned[, paste0("pc", 1:6)])

  cat(sprintf("Aligned data rows: %d\n", nrow(yields)))
} else {
  pcs_aligned <- as.matrix(variables[, paste0("pc", 1:6)])
}

cat("\n")

# Test: Single maturity
cat("Computing W2 for single maturity (i=5)\n")
res_y2_single <- compute_w2_residuals(
  yields = yields,
  term_premia = term_premia,
  maturities = 5,
  n_pcs = 4,
  pcs = pcs_aligned[, 1:4]
)

cat(sprintf("  Number of maturities: %d\n", length(res_y2_single$residuals)))
cat(sprintf("  Residuals length: %d\n", length(res_y2_single$residuals[[1]])))
cat(sprintf("  R-squared: %.4f\n", res_y2_single$r_squared[1]))

# Test: Multiple maturities
cat("\nComputing W2 for multiple maturities\n")
maturities <- c(1, 2, 5, 7, 9)
res_y2_multi <- compute_w2_residuals(
  yields = yields,
  term_premia = term_premia,
  maturities = maturities,
  n_pcs = 4,
  pcs = pcs_aligned[, 1:4]
)

cat(sprintf("  Maturities requested: %s\n", paste(maturities, collapse = ", ")))
cat(sprintf("  Results returned: %d\n", length(res_y2_multi$residuals)))
cat("\n  R-squared by maturity:\n")
for (i in 1:length(maturities)) {
  cat(sprintf(
    "    Maturity %d: R² = %.4f\n",
    maturities[i], res_y2_multi$r_squared[i]
  ))
}

# Test: Different numbers of PCs
cat("\nEffect of number of PCs on R-squared\n")
for (n_pc in c(2, 4, 6)) {
  res <- compute_w2_residuals(
    yields = yields,
    term_premia = term_premia,
    maturities = 5,
    n_pcs = n_pc,
    pcs = pcs_aligned[, 1:n_pc]
  )
  cat(sprintf("  %d PCs: R² = %.4f\n", n_pc, res$r_squared[1]))
}

# Test: Residual properties
cat("\nResidual properties for maturity 3\n")
res_y2_3 <- compute_w2_residuals(
  yields = yields,
  term_premia = term_premia,
  maturities = 3,
  n_pcs = 4,
  pcs = pcs_aligned[, 1:4]
)
W2_3 <- res_y2_3$residuals[[1]]

cat(sprintf("  Mean: %.6f\n", mean(W2_3, na.rm = TRUE)))
cat(sprintf("  SD: %.6f\n", sd(W2_3, na.rm = TRUE)))
cat(sprintf("  Skewness: %.3f\n", moments::skewness(W2_3, na.rm = TRUE)))
cat(sprintf("  Kurtosis: %.3f\n", moments::kurtosis(W2_3, na.rm = TRUE)))

# Test: Compare residuals across maturities
cat("\nResidual standard deviations by maturity\n")
all_maturities <- 1:9
res_y2_all <- compute_w2_residuals(
  yields = yields,
  term_premia = term_premia,
  maturities = all_maturities,
  n_pcs = 4,
  pcs = pcs_aligned[, 1:4]
)

sd_by_maturity <- sapply(res_y2_all$residuals, sd, na.rm = TRUE)
for (i in 1:length(all_maturities)) {
  cat(sprintf("  Maturity %d: SD = %.6f\n", all_maturities[i], sd_by_maturity[i]))
}

# Test: Verify SDF innovations computation
cat("\nVerify Y2 (SDF innovations) computation\n")
# Manually compute SDF innovations for maturity 2
i <- 2
sdf_innov_manual <- compute_sdf_innovations(yields, term_premia, i = i, date = variables_aligned$date)

# Get residuals from function
res_y2_2 <- compute_w2_residuals(
  yields = yields,
  term_premia = term_premia,
  maturities = i,
  n_pcs = 4,
  pcs = pcs_aligned[, 1:4]
)

# What compute_w2_residuals actually uses as Y variable
y_i <- yields[[paste0("y", i)]] / 100
tp_i <- term_premia[[paste0("tp", i)]] / 100
y2_base <- y_i - tp_i # This is what the function uses
y2_future <- y2_base[-1] # Future values

# Now the function correctly uses SDF innovations
cat(sprintf("\n  Verifying correct implementation:\n"))
cat(sprintf("  compute_w2_residuals now uses: SDF innovations from compute_sdf_innovations()\n"))
cat(sprintf("  This matches the theoretical definition: Y_{2,t+1} = E_{t+1}[SDF_{t+1+i}] - E_t[SDF_{t+1+i}]\n\n"))

# Create lagged PCs manually to match what function does
n <- length(sdf_innov_manual)
pc_lagged <- pcs_aligned[1:n, 1:4]

# Run regression manually using SDF innovations
manual_reg_sdf <- lm(sdf_innov_manual ~ pc_lagged)
manual_r2_sdf <- summary(manual_reg_sdf)$r.squared

cat(sprintf("  Manual regression (SDF innovations): R² = %.4f\n", manual_r2_sdf))
cat(sprintf("  Function R²: %.4f\n", res_y2_2$r_squared))
cat(sprintf("  Match: %s\n", ifelse(abs(manual_r2_sdf - res_y2_2$r_squared) < 0.0001,
  "✓ Yes", "✗ No"
)))


# The function regresses SDF innovations on lagged PCs
# So the dependent variable should be SDF innovations
cat(sprintf("  SDF innovations length: %d\n", length(sdf_innov_manual)))
cat(sprintf("  W2 residuals length: %d\n", length(res_y2_2$residuals[[1]])))

# Compare to running regression manually


# Visualization section
cat("\nCreating diagnostic plots...\n")

# User-configurable parameters for plots
plot_maturity <- 5 # Change this to use different maturity for plotting
plot_n_pcs <- 4 # Change this to use different number of PCs for plotting

# Run the regression for plotting
res_plot <- compute_w2_residuals(
  yields = yields,
  term_premia = term_premia,
  maturities = plot_maturity,
  n_pcs = plot_n_pcs,
  pcs = pcs_aligned[, 1:plot_n_pcs]
)

# Get the SDF innovations (Y2) for this maturity
sdf_innov <- compute_sdf_innovations(yields, term_premia, i = plot_maturity)
# Align dates - remove first observation from variables_aligned
plot_dates <- variables_aligned$date[-1]

# Set up plotting parameters
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# Plot 1: Time series of SDF innovations and PC predictions
plot(plot_dates, res_plot$residuals[[1]] + res_plot$fitted[[1]],
  type = "l", col = "black", lwd = 2,
  main = paste("SDF Innovations (Y2) for", plot_maturity, "Year Maturity"),
  xlab = "Date", ylab = "SDF Innovation",
  las = 1
)
lines(plot_dates, res_plot$fitted[[1]], col = "red", lwd = 2)
legend("topright",
  legend = c("Actual (Y2)", "Predicted"),
  col = c("black", "red"),
  lty = 1, lwd = 2, cex = 0.8
)

# Plot 2: Scatter of actual vs predicted
plot(res_plot$fitted[[1]], res_plot$residuals[[1]] + res_plot$fitted[[1]],
  main = "Actual vs Predicted SDF Innovations",
  xlab = "Predicted from Regression",
  ylab = "Actual SDF Innovation",
  pch = 16, cex = 0.7, col = rgb(0, 0, 0, 0.5)
)
abline(0, 1, col = "red", lwd = 2)
text(min(res_plot$fitted[[1]]), max(res_plot$residuals[[1]] + res_plot$fitted[[1]]),
  paste("R² =", round(res_plot$r_squared[1], 3)),
  pos = 4, cex = 0.9
)

# Plot 3: Time series of residuals
plot(plot_dates, res_plot$residuals[[1]],
  type = "l", col = "darkblue",
  main = paste("Residuals Over Time (W2) -", plot_maturity, "Year"),
  xlab = "Date", ylab = "Residual",
  las = 1
)
abline(h = 0, col = "gray", lty = 2)
# Add ±2 SD bands
sd_resid <- sd(res_plot$residuals[[1]])
abline(h = c(-2, 2) * sd_resid, col = "red", lty = 2)

# Plot 4: Histogram of residuals
hist(res_plot$residuals[[1]],
  breaks = 30, col = "lightblue",
  main = "Distribution of Residuals",
  xlab = "Residual Value",
  ylab = "Frequency",
  probability = TRUE
)
# Add normal distribution overlay
x_seq <- seq(min(res_plot$residuals[[1]]), max(res_plot$residuals[[1]]), length = 100)
lines(x_seq, dnorm(x_seq, mean = mean(res_plot$residuals[[1]]), sd = sd_resid),
  col = "red", lwd = 2
)
legend("topright",
  legend = c("Residuals", "Normal"),
  col = c("lightblue", "red"),
  lty = c(NA, 1), lwd = c(NA, 2),
  pch = c(15, NA), cex = 0.8
)

# Reset plotting parameters
par(mfrow = c(1, 1))

cat(sprintf("\nPlots created for maturity %d with %d PCs\n", plot_maturity, plot_n_pcs))
cat("To change the maturity for plotting, modify 'plot_maturity' variable\n")
cat("To change the number of PCs for plotting, modify 'plot_n_pcs' variable\n")

cat("\nTest complete!\n")
