# =============================================================================
# Test: compute_w1_residuals() (formerly compute_reduced_form_residual_y1)
# =============================================================================
# This function computes the reduced form residual for Y1 (consumption growth)
# by regressing consumption growth on lagged principal components. The residuals
# W1 are used in the heteroskedasticity identification framework.

# Load package
library(hetid)

cat("Testing compute_w1_residuals() function\n")
cat("===================================================\n\n")

# Load variables data
data("variables")
cat(sprintf(
  "Variables data: %d observations, %d variables\n",
  nrow(variables), ncol(variables)
))

# Test with default parameters (n_pcs = 4)
cat("\nDefault computation with 4 PCs\n")
res_y1 <- compute_w1_residuals(n_pcs = 4)

# Display results
cat(sprintf("  Residuals length: %d\n", length(res_y1$residuals)))
cat(sprintf("  R-squared: %.4f\n", res_y1$r_squared))
cat(sprintf(
  "  Coefficients: %d (intercept + %d PCs)\n",
  length(res_y1$coefficients),
  length(res_y1$coefficients) - 1
))

# Show coefficient estimates
cat("\n  Coefficient estimates:\n")
coef_names <- names(res_y1$coefficients)
for (i in 1:length(res_y1$coefficients)) {
  cat(sprintf("    %s: %.4f\n", coef_names[i], res_y1$coefficients[i]))
}

# Test with different numbers of PCs
cat("\nR-squared with different numbers of PCs\n")
for (j in 1:6) {
  res <- compute_w1_residuals(n_pcs = j)
  cat(sprintf("  %d PCs: R² = %.4f\n", j, res$r_squared))
}

# Test providing custom data
cat("\nUsing custom variables data\n")
# Use a subset of the data
subset_vars <- variables[1:100, ]
res_subset <- compute_w1_residuals(n_pcs = 3, data = subset_vars)
cat(sprintf("  Subset residuals length: %d\n", length(res_subset$residuals)))
cat(sprintf("  Subset R-squared: %.4f\n", res_subset$r_squared))

# Residual diagnostics
cat("\nResidual diagnostics\n")
res_y1 <- compute_w1_residuals(n_pcs = 4)
W1 <- res_y1$residuals

cat(sprintf("  Mean: %.6f (should be ~0)\n", mean(W1)))
cat(sprintf("  SD: %.4f\n", sd(W1)))
cat(sprintf("  Min: %.4f\n", min(W1)))
cat(sprintf("  Max: %.4f\n", max(W1)))

# Check for autocorrelation
acf_1 <- cor(W1[-length(W1)], W1[-1])
cat(sprintf("  Autocorrelation(1): %.3f\n", acf_1))

# Test the regression setup
cat("\nVerify regression setup\n")
# The function regresses cons_growth on lagged PCs
cons_growth <- variables$gr1.pcecc96
pc_cols <- paste0("pc", 1:4)
pc_data <- as.matrix(variables[, pc_cols])

# Create lagged PCs manually
n <- nrow(variables)
pc_lagged <- pc_data[1:(n - 1), ]
y <- cons_growth[2:n]

# Run regression manually
manual_reg <- lm(y ~ pc_lagged)
manual_r2 <- summary(manual_reg)$r.squared

cat(sprintf("  Manual regression R²: %.4f\n", manual_r2))
cat(sprintf("  Function R²: %.4f\n", res_y1$r_squared))
cat(sprintf("  Match: %s\n", ifelse(abs(manual_r2 - res_y1$r_squared) < 0.0001,
  "✓ Yes", "✗ No"
)))

# Show what Y1 represents
cat("\nUnderstanding Y1 (consumption growth)\n")
cat(sprintf("  Mean consumption growth: %.4f\n", mean(cons_growth, na.rm = TRUE)))
cat(sprintf("  SD consumption growth: %.4f\n", sd(cons_growth, na.rm = TRUE)))
cat(sprintf("  After removing PC effects:\n"))
cat(sprintf("  Mean residual: %.6f\n", mean(W1)))
cat(sprintf("  SD residual: %.4f\n", sd(W1)))
cat(sprintf(
  "  Variance reduction: %.1f%%\n",
  (1 - var(W1) / var(cons_growth[2:n])) * 100
))

# Visualization section
cat("\nCreating diagnostic plots...\n")

# User-configurable parameter for plots
plot_n_pcs <- 4 # Change this to use different number of PCs for plotting

# Run the regression for plotting
res_plot <- compute_w1_residuals(n_pcs = plot_n_pcs)

# Set up plotting parameters
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# Plot: Time series of dependent and independent variables
plot(res_plot$dates, res_plot$residuals + res_plot$fitted,
  type = "l", col = "black", lwd = 2,
  main = paste("Consumption Growth and PC", plot_n_pcs, "Predictions"),
  xlab = "Date", ylab = "Growth Rate",
  las = 1
)
lines(res_plot$dates, res_plot$fitted, col = "red", lwd = 2)
legend("topright",
  legend = c("Actual (Y1)", "Predicted"),
  col = c("black", "red"),
  lty = 1, lwd = 2, cex = 0.8
)

# Plot: Scatter of actual vs predicted
plot(res_plot$fitted, res_plot$residuals + res_plot$fitted,
  main = "Actual vs Predicted Values",
  xlab = "Predicted from Regression",
  ylab = "Actual Consumption Growth",
  pch = 16, cex = 0.7, col = rgb(0, 0, 0, 0.5)
)
abline(0, 1, col = "red", lwd = 2)
text(min(res_plot$fitted), max(res_plot$residuals + res_plot$fitted),
  paste("R² =", round(res_plot$r_squared, 3)),
  pos = 4, cex = 0.9
)

# Plot: Time series of residuals
plot(res_plot$dates, res_plot$residuals,
  type = "l", col = "darkblue",
  main = "Residuals Over Time (W1)",
  xlab = "Date", ylab = "Residual",
  las = 1
)
abline(h = 0, col = "gray", lty = 2)
# Add ±2 SD bands
sd_resid <- sd(res_plot$residuals)
abline(h = c(-2, 2) * sd_resid, col = "red", lty = 2)

# Plot: Histogram of residuals
hist(res_plot$residuals,
  breaks = 30, col = "lightblue",
  main = "Distribution of Residuals",
  xlab = "Residual Value",
  ylab = "Frequency",
  probability = TRUE
)
# Add normal distribution overlay
x_seq <- seq(min(res_plot$residuals), max(res_plot$residuals), length = 100)
lines(x_seq, dnorm(x_seq, mean = mean(res_plot$residuals), sd = sd_resid),
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

cat(sprintf("\nPlots created for %d PCs\n", plot_n_pcs))
cat("To change the number of PCs for plotting, modify 'plot_n_pcs' variable\n")

cat("\nTest complete!\n")
