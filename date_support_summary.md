# Date Support Summary for hetid Package

## Overview
Added `return_df` parameter to residual computation functions and fixed data alignment issues across the package.

## Functions Updated with `return_df` Parameter

### 1. `compute_w1_residuals()`
- **File**: `R/compute_w1_residuals.R`
- **New parameter**: `return_df = FALSE`
- **Behavior**:
  - When `FALSE`: Returns original list format with residuals, fitted values, coefficients, etc.
  - When `TRUE`: Returns data frame with columns: date, residuals, fitted
- **Example**:
  ```r
  res_y1_df <- compute_w1_residuals(n_pcs = 4, return_df = TRUE)
  ```

### 2. `compute_w2_residuals()`
- **File**: `R/compute_w2_residuals.R`
- **New parameters**: `return_df = FALSE`, `dates = NULL`
- **Behavior**:
  - When `FALSE`: Returns original list format with residuals and fitted values for each maturity
  - When `TRUE`: Returns data frame with columns: date, maturity, residuals, fitted
- **Example**:
  ```r
  res_y2_df <- compute_w2_residuals(yields, term_premia,
                                     maturities = c(3, 5, 7),
                                     return_df = TRUE)
  ```

### 3. `solve_gamma_quadratic_lincomb()`
- **File**: `R/solve_gamma_quadratic_lincomb.R`
- **New parameters**: `return_df = FALSE`, `dates = NULL`
- **Behavior**:
  - The `linear_comb` output can now be returned as a data frame with dates
  - When `return_df = TRUE`: `linear_comb` is a data frame with columns: date, linear_comb
- **Example**:
  ```r
  result <- solve_gamma_quadratic_lincomb(pc_matrix, weights, w1, w2, tau,
                                          return_df = TRUE, dates = dates)
  ```

### 4. `solve_gamma_quadratic()`
- **File**: `R/solve_gamma_quadratic.R`
- **New parameter**: `dates = NULL`
- **Behavior**:
  - Tracks which dates remain after removing NA values during validation
  - Returns `dates_used` in output when dates are provided
  - Helps users understand which observations were included in the analysis
- **Example**:
  ```r
  dates <- seq(as.Date("2020-01-01"), length.out = n, by = "month")
  result <- solve_gamma_quadratic(pc_j, w1, w2, tau = 0.5, dates = dates)
  print(result$dates_used) # Shows dates after removing NA values
  ```

## Data Alignment Issues Fixed

Fixed data frequency mismatch in 8 files where monthly ACM data was being used with quarterly PCs:

### Test Files:
1. `scripts/tests/test_solve_gamma_quadratic.R`
2. `scripts/tests/test_solve_gamma_quadratic_lincomb.R`
3. `scripts/tests/test_optimize_pc_weights.R`
4. `scripts/tests/test_optimize_pc_weights_all_maturities.R`

### Script Files:
5. `scripts/01_basic_analysis.R`
6. `scripts/02_heteroskedasticity_tests.R`
7. `scripts/03_optimization_analysis.R`
8. `scripts/05_summary_statistics.R`
9. `scripts/hetid_analysis_enhanced.R`

### Fix Applied:
Changed from:
```r
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
```

To:
```r
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"),
                             frequency = "quarterly")
```

## Functions Already Supporting Dates

The following functions already had `return_df` parameter support:
- `compute_n_hat()` - Returns expected log bond prices with dates
- `compute_price_news()` - Returns price/yield news with dates
- `compute_sdf_innovations()` - Returns SDF innovations with dates

## Benefits

1. **Consistent Interface**: All time series functions now support optional date output
2. **Data Alignment**: Fixed dimension mismatches between monthly and quarterly data
3. **Flexibility**: Users can choose between traditional list output or data frame with dates
4. **Backward Compatibility**: Default behavior unchanged (return_df = FALSE)

## Usage Notes

- When using `return_df = TRUE`, functions attempt to extract dates from the data
- If dates are not available, generic indices (1, 2, 3, ...) are used
- For `compute_w2_residuals()`, the data frame format includes a maturity column for easy filtering
