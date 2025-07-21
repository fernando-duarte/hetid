# =============================================================================
# Summary Statistics and Diagnostics
# =============================================================================
# This script computes and displays comprehensive summary statistics for the
# heteroskedasticity identification analysis.

# -----------------------------------------------------------------------------
# USER PARAMETERS - MODIFY THESE VALUES AS NEEDED
# -----------------------------------------------------------------------------
J <- 4 # Number of principal components to use (1-6)
tau <- 0.5 # Quantile parameter (between 0 and 1)
digits <- 4 # Number of decimal places for output
save_output <- TRUE # Whether to save summary to text file
output_dir <- "scripts/output/05_summary" # Output directory
output_file <- "analysis_summary.txt" # Output filename

# -----------------------------------------------------------------------------
# SETUP
# -----------------------------------------------------------------------------
library(hetid)
library(moments) # For skewness and kurtosis

# Function to capture output
if (save_output) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  sink(file.path(output_dir, output_file), split = TRUE) # Output to both console and file
}

# -----------------------------------------------------------------------------
# DATA LOADING
# -----------------------------------------------------------------------------
cat("========================================\n")
cat("HETEROSKEDASTICITY IDENTIFICATION\n")
cat("SUMMARY STATISTICS AND DIAGNOSTICS\n")
cat("========================================\n")
cat(sprintf("Date: %s\n", Sys.Date()))
cat(sprintf("Time: %s\n\n", format(Sys.time(), "%H:%M:%S")))

# Load data
data("variables")
acm_data <- extract_acm_data(
  data_types = c("yields", "term_premia"),
  frequency = "quarterly"
)
yields_data <- acm_data[, grep("^y", names(acm_data))]
tp_data <- acm_data[, grep("^tp", names(acm_data))]

I <- 9 # Maximum maturity

# -----------------------------------------------------------------------------
# 1. DATA OVERVIEW
# -----------------------------------------------------------------------------
cat("1. DATA OVERVIEW\n")
cat("================\n\n")

cat("Variables dataset:\n")
cat(sprintf("  Observations: %d\n", nrow(variables)))
cat(sprintf(
  "  Time period: %s to %s\n",
  min(rownames(variables)), max(rownames(variables))
))
cat(sprintf("  Variables: %d\n", ncol(variables)))
cat(sprintf(
  "  Principal components available: %d\n\n",
  sum(grepl("^pc", names(variables)))
))

cat("ACM dataset:\n")
cat(sprintf("  Observations: %d\n", nrow(acm_data)))
cat(sprintf(
  "  Time period: %s to %s\n",
  min(acm_data$date), max(acm_data$date)
))
cat(sprintf(
  "  Yield maturities: %d (1-10 years)\n",
  sum(grepl("^y\\d+$", names(acm_data)))
))
cat(sprintf(
  "  Term premia maturities: %d (1-10 years)\n\n",
  sum(grepl("^tp", names(acm_data)))
))

# -----------------------------------------------------------------------------
# 2. PRINCIPAL COMPONENTS ANALYSIS
# -----------------------------------------------------------------------------
cat("2. PRINCIPAL COMPONENTS SUMMARY\n")
cat("================================\n\n")

# Extract PCs
pc_cols <- paste0("pc", 1:6)
pc_data <- variables[, pc_cols]

# Summary statistics for each PC
cat("Summary statistics:\n")
pc_summary <- data.frame(
  PC = 1:6,
  Mean = round(colMeans(pc_data), digits),
  SD = round(apply(pc_data, 2, sd), digits),
  Min = round(apply(pc_data, 2, min), digits),
  Max = round(apply(pc_data, 2, max), digits),
  Skew = round(apply(pc_data, 2, skewness), digits),
  Kurt = round(apply(pc_data, 2, kurtosis), digits)
)
print(pc_summary)

# Correlation matrix
cat("\nCorrelation matrix of PCs:\n")
pc_cor <- cor(pc_data)
print(round(pc_cor, digits))

# Variance explained (if using J components)
cat(sprintf("\nUsing %d principal components\n", J))
pc_var <- apply(pc_data[, 1:J], 2, var)
cat("Variance of each PC:\n")
print(round(pc_var, digits))
cat(sprintf(
  "Total variance explained: %.2f%%\n\n",
  sum(pc_var) / sum(apply(pc_data, 2, var)) * 100
))

# -----------------------------------------------------------------------------
# 3. CONSUMPTION GROWTH ANALYSIS
# -----------------------------------------------------------------------------
cat("3. CONSUMPTION GROWTH (Y1) ANALYSIS\n")
cat("====================================\n\n")

# Compute residuals
res_y1 <- compute_w1_residuals(n_pcs = J)
W1 <- res_y1$residuals

cat("Reduced form regression for Y1:\n")
cat(sprintf("  Dependent variable: Consumption growth\n"))
cat(sprintf("  Regressors: %d lagged PCs\n", J))
cat(sprintf("  R-squared: %.4f\n", res_y1$r_squared))
cat(sprintf("  Observations: %d\n\n", length(W1)))

# Residual diagnostics
cat("W1 residual diagnostics:\n")
cat(sprintf("  Mean: %.6f (should be near 0)\n", mean(W1)))
cat(sprintf("  SD: %.4f\n", sd(W1)))
cat(sprintf("  Skewness: %.4f\n", skewness(W1)))
cat(sprintf("  Kurtosis: %.4f\n", kurtosis(W1)))
cat(sprintf("  Min: %.4f\n", min(W1)))
cat(sprintf("  Max: %.4f\n\n", max(W1)))

# Normality test
jb_test <- jarque.test(W1)
cat(sprintf("Jarque-Bera normality test:\n"))
cat(sprintf("  Statistic: %.4f\n", jb_test$statistic))
cat(sprintf("  p-value: %.4f\n", jb_test$p.value))
cat(sprintf(
  "  %s at 5%% level\n\n",
  ifelse(jb_test$p.value < 0.05, "Reject normality", "Fail to reject normality")
))

# -----------------------------------------------------------------------------
# 4. SDF INNOVATIONS (Y2) ANALYSIS
# -----------------------------------------------------------------------------
cat("4. SDF INNOVATIONS (Y2) ANALYSIS\n")
cat("=================================\n\n")

# Compute residuals for all maturities
res_y2 <- compute_w2_residuals(
  yields = yields_data,
  term_premia = tp_data,
  maturities = 1:I,
  n_pcs = J,
  pcs = as.matrix(variables[, paste0("pc", 1:J)])
)
W2_list <- res_y2$residuals

# Summary by maturity
cat("R-squared by maturity:\n")
rsq_summary <- data.frame(
  Maturity = 1:I,
  R_squared = round(res_y2$r_squared, digits),
  Mean_W2 = round(sapply(W2_list, mean), digits),
  SD_W2 = round(sapply(W2_list, sd), digits),
  Skew_W2 = round(sapply(W2_list, skewness), digits),
  Kurt_W2 = round(sapply(W2_list, kurtosis), digits)
)
print(rsq_summary)

cat(sprintf(
  "\nAverage R-squared across maturities: %.4f\n",
  mean(res_y2$r_squared)
))
cat(sprintf(
  "Range: [%.4f, %.4f]\n\n",
  min(res_y2$r_squared), max(res_y2$r_squared)
))

# -----------------------------------------------------------------------------
# 5. GAMMA1 ROOTS SUMMARY
# -----------------------------------------------------------------------------
cat("5. GAMMA1 ROOTS SUMMARY\n")
cat("=======================\n\n")

# Compute a sample of roots
results_sample <- matrix(NA, nrow = J, ncol = I)
complex_count <- 0

PC_data <- as.matrix(variables[, paste0("pc", 1:J)])
n_obs <- min(length(W1), nrow(PC_data), length(W2_list[[1]]))
W1_aligned <- W1[1:n_obs]
PC_aligned <- PC_data[1:n_obs, ]

for (j in 1:J) {
  for (i in 1:I) {
    result <- tryCatch(
      {
        solve_gamma_quadratic(
          pc_j = PC_aligned[, j],
          w1 = W1_aligned,
          w2 = W2_list[[i]][1:n_obs],
          tau = tau,
          use_t_minus_1 = TRUE
        )
      },
      error = function(e) list(roots = c(NA, NA), error = e$message)
    )

    if (!is.null(result$error) || is.complex(result$roots[1])) {
      complex_count <- complex_count + 1
    } else {
      results_sample[j, i] <- abs(result$roots[1] - result$roots[2])
    }
  }
}

cat(sprintf("Root computation summary (tau = %.2f):\n", tau))
cat(sprintf("  Total combinations: %d (J=%d × I=%d)\n", J * I, J, I))
cat(sprintf(
  "  Complex/error cases: %d (%.1f%%)\n",
  complex_count, complex_count / (J * I) * 100
))
cat(sprintf(
  "  Real root cases: %d (%.1f%%)\n\n",
  J * I - complex_count, (J * I - complex_count) / (J * I) * 100
))

# Summary of root distances
valid_distances <- as.vector(results_sample[!is.na(results_sample)])
if (length(valid_distances) > 0) {
  cat("Root distance statistics (for real roots):\n")
  cat(sprintf("  Mean: %.4f\n", mean(valid_distances)))
  cat(sprintf("  Median: %.4f\n", median(valid_distances)))
  cat(sprintf("  SD: %.4f\n", sd(valid_distances)))
  cat(sprintf("  Min: %.4f\n", min(valid_distances)))
  cat(sprintf("  Max: %.4f\n\n", max(valid_distances)))

  # Best combinations
  cat("Best (smallest distance) combinations:\n")
  distance_df <- expand.grid(PC = 1:J, Maturity = 1:I)
  distance_df$Distance <- as.vector(t(results_sample))
  distance_df <- distance_df[!is.na(distance_df$Distance), ]
  distance_df <- distance_df[order(distance_df$Distance), ]

  n_show <- min(5, nrow(distance_df))
  for (k in 1:n_show) {
    cat(sprintf(
      "  %d. PC%d, Maturity %d: distance = %.4f\n",
      k, distance_df$PC[k], distance_df$Maturity[k],
      distance_df$Distance[k]
    ))
  }
}

# -----------------------------------------------------------------------------
# 6. DATA QUALITY CHECKS
# -----------------------------------------------------------------------------
cat("\n6. DATA QUALITY CHECKS\n")
cat("======================\n\n")

# Check for missing values
cat("Missing values:\n")
cat(sprintf(
  "  Variables data: %d (%.2f%%)\n",
  sum(is.na(variables)),
  sum(is.na(variables)) / length(variables) * 100
))
cat(sprintf(
  "  ACM yields: %d (%.2f%%)\n",
  sum(is.na(yields_data)),
  sum(is.na(yields_data)) / length(yields_data) * 100
))
cat(sprintf(
  "  ACM term premia: %d (%.2f%%)\n",
  sum(is.na(tp_data)),
  sum(is.na(tp_data)) / length(tp_data) * 100
))

# Check alignment
cat("\nData alignment:\n")
cat(sprintf("  Variables observations: %d\n", nrow(variables)))
cat(sprintf("  ACM observations: %d\n", nrow(acm_data)))
cat(sprintf("  Aligned observations used: %d\n", n_obs))

# -----------------------------------------------------------------------------
# FOOTER
# -----------------------------------------------------------------------------
cat("\n========================================\n")
cat("END OF SUMMARY REPORT\n")
cat("========================================\n")
cat(sprintf("Generated on: %s\n", Sys.time()))
cat(sprintf("R version: %s\n", R.version.string))
cat(sprintf("hetid package version: %s\n", packageVersion("hetid")))

# Close output file if saving
if (save_output) {
  sink()
  cat(sprintf("\nSummary saved to: %s\n", file.path(output_dir, output_file)))
}
