# Tests for percentage-point yield-unit validation guardrail

test_that("validate_percent_units rejects non-tabular input", {
  expect_error(
    validate_percent_units(1:10),
    class = "hetid_error_bad_argument"
  )
})

test_that("validate_percent_units rejects a non-numeric tabular input", {
  y <- data.frame(a = c("x", "y"), b = c("z", "w"), stringsAsFactors = FALSE)
  expect_error(
    validate_percent_units(y),
    class = "hetid_error_bad_argument"
  )
})

test_that("validate_percent_units warns on decimal-looking yields", {
  y <- matrix(c(0.02, 0.03, 0.04, 0.05), ncol = 2)
  expect_warning(
    validate_percent_units(y),
    class = "hetid_warning_unit_scale"
  )
})

test_that("validate_percent_units is silent on percentage-point yields", {
  y <- matrix(c(2, 3, 4, 5), ncol = 2)
  expect_silent(validate_percent_units(y))
})
