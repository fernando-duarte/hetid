# hetid Package Tests

This directory contains comprehensive test files for all major functions in the hetid package.

## Test Structure

Each test file corresponds to a specific function and includes:
- Basic functionality tests
- Input validation and error handling
- Mathematical validation and sanity checks
- Edge cases and boundary conditions
- Consistency checks across related functions

## Test Files

### Core Computation Functions
- `test-compute_c_hat.R` - Tests supremum estimator
- `test-compute_k_hat.R` - Tests fourth moment estimator
- `test-compute_n_hat.R` - Tests expected log bond price estimator
- `test-compute_price_news.R` - Tests unexpected bond price changes

### Residual Functions
- `test-compute_w1_residuals.R` - Tests consumption growth residuals
- `test-compute_w2_residuals.R` - Tests SDF innovation residuals

### Identification Functions
- `test-solve_gamma_quadratic.R` - Tests gamma1 quadratic solver
- `test-optimize_pc_weights.R` - Tests optimal PC weight finding

### Data Functions
- `test-extract_acm_data.R` - Tests ACM data extraction
- `test-download_functions.R` - Tests data download functions

## Running Tests

### Run all tests
```r
# From package root directory
devtools::test()

# Or using testthat directly
testthat::test_dir("tests/testthat")
```

### Run specific test file
```r
testthat::test_file("tests/testthat/test-compute_c_hat.R")
```

### Run tests for a specific function
```r
testthat::test_file("tests/testthat/test-solve_gamma_quadratic.R",
                    filter = "mathematical validation")
```

## Test Guidelines

1. **No Optional Parameters**: Tests use only default parameters unless testing specific parameter effects
2. **Manual Validation**: Where possible, tests include manual calculations to verify results
3. **Sanity Checks**: Tests verify reasonable ranges and expected relationships
4. **Reproducibility**: Tests use set.seed() where randomness is involved
5. **Data Independence**: Tests work with package-provided data via extract_acm_data()

## Expected Behavior

All tests should pass when run on a system with:
- R >= 4.0.0
- All package dependencies installed
- Internet connection (for download function tests)

Tests that require internet connection use `skip_if_offline()` to avoid failures in offline environments.

## Coverage

These tests aim to provide comprehensive coverage of:
- Normal use cases with default parameters
- Input validation and error conditions
- Mathematical correctness
- Edge cases and boundary conditions
- Consistency across related functions

The tests focus on ensuring functions behave correctly with typical usage patterns rather than exhaustively testing all parameter combinations.
