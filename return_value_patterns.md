# Return Value Patterns Analysis for hetid Package

## Common Patterns Found

### 1. Regression/Model Results Pattern
Functions that perform regression analysis commonly return:
- `residuals` - Model residuals
- `fitted` - Fitted values
- `coefficients` - Model coefficients
- `r_squared` - R-squared statistic
- `n_obs` - Number of observations used

Examples:
- `compute_w1_residuals()`
- `compute_w2_residuals()`
- `validate_w2_inputs()` (internal regression)

### 2. Quadratic Solution Pattern
Functions solving quadratic equations return:
- `roots` - The solution roots
- `coefficients` - Quadratic coefficients (a, b, c)
- `discriminant` - The discriminant value
- `error` - Error message if solution fails

Examples:
- `solve_gamma_quadratic()`
- `solve_gamma_quadratic_lincomb()`

### 3. Optimization Results Pattern
Optimization functions return:
- `optimal_weights` or `weights_opt` - Optimized weights
- `tau_opt` - Optimal tau parameter
- `objective_value` or `objective` - Final objective function value
- `convergence` - Convergence status
- `roots` - Solutions at optimal parameters
- `theta_lower`, `theta_upper` - Confidence bounds

Examples:
- `optimize_pc_weights()`
- `optimize_theta_runner()`
- `optimize_theta_identification()`

### 4. Data Processing Pattern
Functions that process data return either:
- A data.frame with specific columns (when `return_df = TRUE`)
- A list with processed values and metadata

### 5. Component/Metadata Pattern
Many functions include additional metadata:
- `dates` or `dates_used` - Dates after removing NA values
- `maturity` or `maturities` - Maturity information
- `n_pcs` - Number of principal components
- `settings` - Settings used in computation
- `components` - Intermediate calculation components

## Inconsistencies Identified

### 1. Naming Inconsistencies
- **Weights**: `optimal_weights` vs `weights_opt`
- **Tau**: `tau_opt` vs `tau` (as part of settings)
- **Objective function value**: `objective_value` vs `objective`
- **Number of observations**: `n_obs` vs `n_observations` (though n_obs is predominant)

### 2. Return Structure Inconsistencies
- Some functions return simple values (e.g., `compute_variance_bound()` returns a single numeric)
- Others return complex nested lists with multiple levels
- Some have conditional return structures based on parameters (e.g., `return_df`)

### 3. Error Handling Inconsistencies
- Some functions include an `error` field in the return list
- Others use stop() or warning() without structured error returns
- Mixed approaches to handling invalid inputs

### 4. Date Handling Inconsistencies
- Some functions return `dates` in the main list
- Others return `dates_used` to indicate filtering
- Some don't handle dates at all even when time series data is involved

## Recommendations for Standardization

1. **Standardize naming conventions**:
   - Use `weights` for current weights, `optimal_weights` for optimized
   - Use `objective` consistently for objective function values
   - Use `n_obs` consistently for observation counts

2. **Standardize error handling**:
   - All functions should return structured errors in a consistent format
   - Consider using a common error structure: `list(success = FALSE, error = "message")`

3. **Standardize date handling**:
   - Always include `dates_used` when dates are provided and filtering occurs
   - Use consistent naming for date-related fields

4. **Consider creating return value classes**:
   - `regression_result` class for regression outputs
   - `optimization_result` class for optimization outputs
   - `quadratic_solution` class for quadratic solutions

This would enable better method dispatch and consistent handling of results across the package.
