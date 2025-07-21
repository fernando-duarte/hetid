# Test: Date Support in Gamma Functions
# =====================================
# Tests the new date support in solve_gamma_quadratic and related functions

library(hetid)

cat("Testing date support in gamma functions\n")
cat("======================================\n\n")

# Generate sample data with some NAs
set.seed(123)
n <- 100
dates <- seq(as.Date("2020-01-01"), length.out = n, by = "month")

# Create data with some missing values
pc_j <- rnorm(n)
w1 <- rnorm(n)
w2 <- rnorm(n)

# Introduce some NAs
pc_j[c(5, 15, 25)] <- NA
w1[c(10, 20)] <- NA
w2[c(30, 40)] <- NA

# Test 1: solve_gamma_quadratic with dates
cat("Test 1: solve_gamma_quadratic with dates\n")
cat("-----------------------------------------\n")

result <- solve_gamma_quadratic(pc_j, w1, w2, tau = 0.5, dates = dates)

cat(sprintf("  Original data length: %d\n", n))
cat(sprintf("  Data length after removing NAs: %d\n", result$components$n_obs))
cat(sprintf("  Dates tracked: %s\n", !is.null(result$dates_used)))

if (!is.null(result$dates_used)) {
  cat(sprintf(
    "  Date range used: %s to %s\n",
    min(result$dates_used), max(result$dates_used)
  ))
  cat(sprintf("  Number of dates: %d\n", length(result$dates_used)))

  # Check which dates were removed
  removed_indices <- which(is.na(pc_j) | is.na(w1) | is.na(w2))
  cat(sprintf("\n  Dates removed due to NAs:\n"))
  for (idx in removed_indices[1:min(5, length(removed_indices))]) {
    cat(sprintf("    %s (index %d)\n", dates[idx], idx))
  }
  if (length(removed_indices) > 5) {
    cat(sprintf("    ... and %d more\n", length(removed_indices) - 5))
  }
}

# Test 2: solve_gamma_quadratic_lincomb with return_df
cat("\n\nTest 2: solve_gamma_quadratic_lincomb with return_df\n")
cat("----------------------------------------------------\n")

# Create PC matrix
pc_matrix <- matrix(rnorm(n * 4), nrow = n, ncol = 4)
weights <- c(0.5, 0.3, 0.1, 0.1)

# Test without return_df
result_vec <- solve_gamma_quadratic_lincomb(
  pc_matrix, weights, w1, w2,
  tau = 0.5,
  return_df = FALSE
)

# Test with return_df
result_df <- solve_gamma_quadratic_lincomb(
  pc_matrix, weights, w1, w2,
  tau = 0.5,
  return_df = TRUE, dates = dates
)

cat(sprintf("  Without return_df:\n"))
cat(sprintf("    linear_comb class: %s\n", class(result_vec$linear_comb)))
cat(sprintf("    linear_comb length: %d\n", length(result_vec$linear_comb)))

cat(sprintf("\n  With return_df:\n"))
cat(sprintf("    linear_comb class: %s\n", class(result_df$linear_comb)))
cat(sprintf(
  "    linear_comb dimensions: %d x %d\n",
  nrow(result_df$linear_comb), ncol(result_df$linear_comb)
))
cat(sprintf(
  "    Column names: %s\n",
  paste(names(result_df$linear_comb), collapse = ", ")
))

# Show first few rows
cat("\n  First 5 rows of linear_comb with dates:\n")
print(head(result_df$linear_comb, 5))

# Test 3: optimize_pc_weights with dates
cat("\n\nTest 3: optimize_pc_weights with dates\n")
cat("--------------------------------------\n")

# Remove NAs for this test
complete_idx <- complete.cases(pc_matrix, w1, w2)
pc_clean <- pc_matrix[complete_idx, ]
w1_clean <- w1[complete_idx]
w2_clean <- w2[complete_idx]
dates_clean <- dates[complete_idx]

opt_result <- optimize_pc_weights(
  pc_matrix = pc_clean,
  w1 = w1_clean,
  w2 = w2_clean,
  tau = 0.5,
  dates = dates_clean
)

cat(sprintf("  linear_comb class: %s\n", class(opt_result$linear_comb)))
if (is.data.frame(opt_result$linear_comb)) {
  cat(sprintf(
    "  Date range: %s to %s\n",
    min(opt_result$linear_comb$date),
    max(opt_result$linear_comb$date)
  ))
}

cat("\nAll tests completed!\n")
