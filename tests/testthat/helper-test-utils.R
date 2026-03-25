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
