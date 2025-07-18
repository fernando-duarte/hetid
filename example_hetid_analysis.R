# Simple example of heteroskedasticity identification analysis
# This script demonstrates basic usage of the hetid package

# Load required libraries
library(hetid)

# Set parameters
J <- 4 # Use first 4 principal components
tau <- 0.5 # Set tau to 0.5
I <- 5 # Analyze maturities 1-5 years (for faster demo)

cat("Heteroskedasticity Identification Analysis - Quick Demo\n")
cat("======================================================\n\n")

# Load data
data("variables")

# Ensure ACM data is available
if (!file.exists(system.file("extdata", "acm_term_premia.csv", package = "hetid"))) {
  cat("Downloading ACM data...\n")
  download_term_premia(quiet = TRUE)
}

# Load ACM data
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields_data <- acm_data[, grep("^y", names(acm_data))]
tp_data <- acm_data[, grep("^tp", names(acm_data))]

# Step 1: Compute reduced form residuals for consumption growth (W1)
cat("Step 1: Computing consumption growth residuals (W1)...\n")
res_y1 <- compute_reduced_form_residual_y1(n_pcs = J)
W1 <- res_y1$residuals
cat(sprintf("  - R-squared from regression: %.3f\n", res_y1$r_squared))
cat(sprintf("  - Number of observations: %d\n\n", length(W1)))

# Step 2: Compute reduced form residuals for SDF innovations (W2)
cat("Step 2: Computing SDF innovation residuals (W2) for maturities 1-5...\n")
res_y2 <- compute_reduced_form_residual_y2(
  yields = yields_data,
  term_premia = tp_data,
  maturities = 1:I,
  n_pcs = J,
  variables_data = variables
)

# Display R-squared for each maturity
for (i in 1:I) {
  cat(sprintf("  - Maturity %d: R-squared = %.3f\n", i, res_y2$r_squared[i]))
}
cat("\n")

# Step 3: Solve quadratic for specific examples
cat("Step 3: Example calculations\n")
cat("----------------------------\n\n")

# Example 1: PC1 and maturity 2
j <- 1
i <- 2

# Extract data
PC_data <- as.matrix(variables[, paste0("pc", 1:J)])
n_obs <- min(length(W1), nrow(PC_data), length(res_y2$residuals[[1]]))

pc_j <- PC_data[1:n_obs, j]
w1_aligned <- W1[1:n_obs]
w2_i <- res_y2$residuals[[i]][1:n_obs]

# Solve quadratic
result <- solve_gamma_quadratic(
  pc_j = pc_j,
  w1 = w1_aligned,
  w2 = w2_i,
  tau = tau
)

cat(sprintf("Example: PC%d and Maturity %d year(s) with tau = %.2f\n", j, i, tau))
cat(sprintf("  Quadratic coefficients:\n"))
cat(sprintf("    a = %.4f\n", result$coefficients["a"]))
cat(sprintf("    b = %.4f\n", result$coefficients["b"]))
cat(sprintf("    c = %.4f\n", result$coefficients["c"]))
cat(sprintf("  Discriminant = %.4f\n", result$discriminant))
cat(sprintf("  Roots:\n"))
cat(sprintf("    γ₁⁽¹⁾ = %.4f\n", result$roots[1]))
cat(sprintf("    γ₁⁽²⁾ = %.4f\n", result$roots[2]))
cat("\n")

# Show variance components
cat("  Variance/Covariance components:\n")
cat(sprintf("    Cov(W1*W2, PC_j) = %.6f\n", result$components$cov_w1w2_pcj))
cat(sprintf("    Cov(W2², PC_j) = %.6f\n", result$components$cov_w2sq_pcj))
cat(sprintf("    Var(W1*W2) = %.6f\n", result$components$var_w1w2))
cat(sprintf("    Var(W2²) = %.6f\n", result$components$var_w2sq))
cat("\n")

# Quick summary table for all PC1 combinations
cat("Summary: γ₁⁽¹⁾ values for PC1 across all maturities\n")
cat("---------------------------------------------------\n")
cat("Maturity:  ")
for (i in 1:I) {
  cat(sprintf(" %8d ", i))
}
cat("\nγ₁⁽¹⁾:     ")
for (i in 1:I) {
  w2_i <- res_y2$residuals[[i]][1:n_obs]
  result <- tryCatch(
    {
      solve_gamma_quadratic(pc_j, w1_aligned, w2_i, tau)
    },
    error = function(e) list(roots = c(NA, NA))
  )

  cat(sprintf(" %8.2f ", Re(result$roots[1])))
}
cat("\n\n")

cat("Demo complete! For full analysis, run hetid_analysis_script.R\n")
