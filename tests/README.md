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

Test files follow the convention `test-<name>.R`, where `<name>` matches the
function or module under test (see `tests/testthat/` for the full list). They
group into the following areas:

- **Bond pricing and moments**: supremum (`compute_c_hat`), fourth-moment
  (`compute_k_hat`, `compute_k2_hat`), expected log bond prices (`compute_n_hat`),
  price news, SDF innovations, expected SDF and its variance bound, and variance
  bounds.
- **Reduced-form residuals**: `compute_w1_residuals` and `compute_w2_residuals`,
  including their lagged-outcome, exogenous-predictor, and custom-PC variants.
- **Identification and identified set**: `hetid_moments`, quadratic-system
  builders (`build_quadratic_system`, `build_general_quadratic_system`),
  identified-set components and quadratic values, constraint checkers/factories,
  generalized instruments, and structural-coefficient recovery.
- **Statistics**: scalar, vector, and matrix statistics plus statistics
  utilities.
- **Data and download**: ACM extraction, column mapping, schema and unit
  validation, the bundled and daily ACM assets, and the download helpers.
- **Conventions and utilities**: period-end / date normalization, quarterly
  conversion, maturity and step parameterization, naming conformance, and the
  structured-condition helpers.
- **Integration**: `test-cross-function-consistency.R` checks consistency across
  related functions.

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
