#' Test Utilities for hetid Package
#'
#' Common functions and data setup for testing
#'

#' Load Standard Test Data
#'
#' Loads ACM data with standard configuration for testing
#'
#' @param data_types Character vector of data types to load
#' @param frequency Data frequency ("quarterly" or "monthly")
#' @return Data frame with requested data
#' @keywords internal
load_standard_test_data <- function(data_types = c("yields", "term_premia"),
                                    frequency = "quarterly") {
  extract_acm_data(
    data_types = data_types,
    frequency = frequency
  )
}

#' Extract Yields and Term Premia from Test Data
#'
#' Common pattern for extracting yields and term premia columns
#'
#' @param data Data frame from load_standard_test_data
#' @return List with yields and term_premia data frames
#' @keywords internal
extract_yields_and_tp <- function(data) {
  list(
    yields = data[, grep("^y", names(data)), drop = FALSE],
    term_premia = data[, grep("^tp", names(data)), drop = FALSE]
  )
}

#' Setup Standard Test Environment
#'
#' Complete setup for most computation tests
#'
#' @param data_types Character vector of data types to load
#' @param frequency Data frequency
#' @return List with data, yields, and term_premia
#' @keywords internal
setup_standard_test_env <- function(data_types = c("yields", "term_premia"),
                                    frequency = "quarterly") {
  data <- load_standard_test_data(data_types, frequency)
  extracted <- extract_yields_and_tp(data)

  list(
    data = data,
    yields = extracted$yields,
    term_premia = extracted$term_premia
  )
}

#' Standard Expectations for Single Value Results
#'
#' Common expectations for functions that return single numeric values
#'
#' @param result The result to test
#' @param should_be_positive Whether the result should be positive
#' @param label Label for the expectation
#' @keywords internal
expect_single_finite_value <- function(result, should_be_positive = TRUE, label = "result") {
  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(is.finite(result))

  if (should_be_positive) {
    expect_gt(result, 0, label = paste(label, "should be positive"))
  }
}

#' Standard Expectations for Time Series Results
#'
#' Common expectations for functions that return time series
#'
#' @param result The result to test
#' @param expected_length Expected length (optional)
#' @param allow_na Whether NA values are allowed
#' @param label Label for the expectation
#' @keywords internal
expect_time_series <- function(result, expected_length = NULL, allow_na = TRUE, label = "result") {
  expect_type(result, "double")

  if (!is.null(expected_length)) {
    expect_length(result, expected_length)
  }

  if (!allow_na) {
    expect_true(all(is.finite(result)), label = paste(label, "should not contain NA/Inf"))
  }
}

#' Standard Expectations for Data Frame Results
#'
#' Common expectations for functions that return data frames
#'
#' @param result The result to test
#' @param expected_cols Expected column names
#' @param expected_rows Expected number of rows (optional)
#' @param label Label for the expectation
#' @keywords internal
expect_standard_dataframe <- function(result, expected_cols, expected_rows = NULL,
                                      label = "result") {
  expect_s3_class(result, "data.frame")
  expect_named(result, expected_cols)

  if (!is.null(expected_rows)) {
    expect_equal(nrow(result), expected_rows)
  }

  # Check that date column exists and is properly formatted if expected
  if ("date" %in% expected_cols) {
    expect_true(all(!is.na(result$date)), label = "dates should not be NA")
  }
}

#' Test Function with Multiple Maturities
#'
#' Helper to test a function across multiple maturities
#'
#' @param test_fn Function to test (should take yields, term_premia, i as arguments)
#' @param maturities Vector of maturities to test
#' @param test_env Test environment from setup_standard_test_env
#' @param expectation_fn Function to apply expectations to each result
#' @keywords internal
test_across_maturities <- function(test_fn, maturities = c(2, 5, 10),
                                   test_env, expectation_fn) {
  for (i in maturities) {
    result <- test_fn(test_env$yields, test_env$term_premia, i)
    expectation_fn(result, i)
  }
}

#' Create Test Data with Known Properties
#'
#' Creates synthetic test data with known statistical properties
#'
#' @param n Number of observations
#' @param n_maturities Number of maturities
#' @param seed Random seed for reproducibility
#' @return List with yields and term_premia matrices
#' @keywords internal
create_synthetic_test_data <- function(n = 100, n_maturities = 10, seed = 123) {
  set.seed(seed)

  # Create correlated yield data
  yields <- matrix(rnorm(n * n_maturities), nrow = n, ncol = n_maturities)
  colnames(yields) <- paste0("y", 1:n_maturities)

  # Create term premia with some structure
  term_premia <- matrix(rnorm(n * n_maturities, mean = 0.01, sd = 0.005),
    nrow = n, ncol = n_maturities
  )
  colnames(term_premia) <- paste0("tp", 1:n_maturities)

  list(
    yields = as.data.frame(yields),
    term_premia = as.data.frame(term_premia)
  )
}

#' Standard Error Testing
#'
#' Common patterns for testing error conditions
#'
#' @param test_fn Function to test
#' @param test_env Test environment
#' @param error_tests List of error test configurations
#' @keywords internal
test_standard_errors <- function(test_fn, test_env, error_tests) {
  for (test_config in error_tests) {
    expect_error(
      do.call(test_fn, test_config$args),
      test_config$pattern,
      info = test_config$description
    )
  }
}

#' Validate Computation Results
#'
#' Standard validation for computation results
#'
#' @param result Computation result
#' @param validation_rules List of validation rules
#' @keywords internal
validate_computation_result <- function(result, validation_rules) {
  for (rule in validation_rules) {
    rule_fn <- get(rule$type)
    do.call(rule_fn, c(list(result), rule$args))
  }
}
