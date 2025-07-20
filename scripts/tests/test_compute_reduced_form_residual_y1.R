# =============================================================================
# Test: compute_reduced_form_residual_y1()
# =============================================================================
# This function computes the reduced form residual for Y1 (consumption growth)
# by regressing consumption growth on lagged principal components. The residuals
# W1 are used in the heteroskedasticity identification framework.

# Load package
library(hetid)

cat("Testing compute_reduced_form_residual_y1() function\n")
cat("===================================================\n\n")

# Load variables data
data("variables")
cat(sprintf(
  "Variables data: %d observations, %d variables\n",
  nrow(variables), ncol(variables)
))

# Test with default parameters (n_pcs = 4)
cat("\nDefault computation with 4 PCs\n")
res_y1 <- compute_reduced_form_residual_y1(n_pcs = 4)

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
  res <- compute_reduced_form_residual_y1(n_pcs = j)
  cat(sprintf("  %d PCs: R² = %.4f\n", j, res$r_squared))
}

# Test providing custom data
cat("\nUsing custom variables data\n")
# Use a subset of the data
subset_vars <- variables[1:100, ]
res_subset <- compute_reduced_form_residual_y1(n_pcs = 3, data = subset_vars)
cat(sprintf("  Subset residuals length: %d\n", length(res_subset$residuals)))
cat(sprintf("  Subset R-squared: %.4f\n", res_subset$r_squared))

# Residual diagnostics
cat("\nResidual diagnostics\n")
res_y1 <- compute_reduced_form_residual_y1(n_pcs = 4)
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
cons_growth <- variables$cons_growth
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

cat("\nTest complete!\n")
