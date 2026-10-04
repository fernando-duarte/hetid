#' Test Utilities for hetid Package
#'
#' Common functions and data setup for testing
#'

#' Extract Yields and Term Premia from Test Data
#'
#' Common pattern for extracting yields and term premia columns
#'
#' @param data Data frame from extract_acm_data
#' @return List with yields and term_premia data frames
extract_yields_and_tp <- function(data) {
  mats <- HETID_CONSTANTS$DEFAULT_ACM_MATURITIES
  list(
    yields = data[, paste0("y", mats), drop = FALSE],
    term_premia = data[, paste0("tp", mats), drop = FALSE]
  )
}

#' Setup Standard Test Environment
#'
#' Complete setup for most computation tests
#'
#' @return List with data, yields, and term_premia
setup_standard_test_env <- function() {
  # Drop the trailing incomplete quarter so computation tests stay
  # warning-free; that warning has its own dedicated tests
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly",
    use_incomplete_quarters = FALSE
  )
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
create_synthetic_test_data <- function(n = 100, n_maturities = 10, seed = 123) {
  set.seed(seed)

  # Column names at the annual month nodes (y12, y24, ...)
  mats <- 12L * seq_len(n_maturities)

  yields <- matrix(rnorm(n * n_maturities), nrow = n, ncol = n_maturities)
  colnames(yields) <- paste0("y", mats)

  term_premia <- matrix(rnorm(n * n_maturities, mean = 0.01, sd = 0.005),
    nrow = n, ncol = n_maturities
  )
  colnames(term_premia) <- paste0("tp", mats)

  list(
    yields = as.data.frame(yields),
    term_premia = as.data.frame(term_premia)
  )
}
