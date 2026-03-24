# hetid Package Tests

This directory contains comprehensive test files for all major
functions in the hetid package.

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
- `test-compute_sdf_innovations.R` - Tests SDF innovation computation
- `test-compute_variance_bound.R` - Tests variance bound computation

### Residual Functions
- `test-compute_w1_residuals.R` - Tests consumption growth residuals
- `test-compute_w2_residuals.R` - Tests SDF innovation residuals

### Identification Functions
- `test-compute_identified_set_components.R` - Tests identified
  set component calculations
- `test-compute_identified_set_quadratic.R` - Tests identified
  set quadratic solver
- `test-compute_identified_set_quadratic-values.R` - Tests
  identified set quadratic values

### Statistics Functions
- `test-compute_matrix_statistics.R` - Tests matrix statistics
- `test-compute_scalar_statistics.R` - Tests scalar statistics
- `test-compute_vector_statistics.R` - Tests vector statistics

### Data Functions
- `test-extract_acm_data.R` - Tests ACM data extraction
- `test-download_functions.R` - Tests data download functions
- `test-build_acm_col_mapping.R` - Tests ACM column mapping
- `test-data_paths.R` - Tests data path utilities

### Utility Functions
- `test-convert_to_quarterly.R` - Tests quarterly conversion
- `test-validation_utils.R` - Tests validation utilities

### Integration Tests
- `test-cross-function-consistency.R` - Tests consistency
  across related functions

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
testthat::test_file(
  "tests/testthat/test-compute_c_hat.R"
)
```

### Run tests for a specific function
```r
testthat::test_file(
  "tests/testthat/test-compute_identified_set_quadratic.R",
  filter = "mathematical validation"
)
```

## Test Guidelines

- **No Optional Parameters**: Tests use only default parameters
  unless testing specific parameter effects
- **Manual Validation**: Where possible, tests include manual
  calculations to verify results
- **Sanity Checks**: Tests verify reasonable ranges and expected
  relationships
- **Reproducibility**: Tests use set.seed() where randomness is
  involved
- **Data Independence**: Tests work with package-provided data
  via extract_acm_data()

## Expected Behavior

All tests should pass when run on a system with:
- R >= 3.5.0
- All package dependencies installed
- Internet connection (for download function tests)

Tests that require internet connection use `skip_if_offline()`
to avoid failures in offline environments.

## Coverage

These tests aim to provide comprehensive coverage of:
- Normal use cases with default parameters
- Input validation and error conditions
- Mathematical correctness
- Edge cases and boundary conditions
- Consistency across related functions

The tests focus on ensuring functions behave correctly with
typical usage patterns rather than exhaustively testing all
parameter combinations.
