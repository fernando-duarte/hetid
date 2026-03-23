test_that("validate_maturity_index rejects non-integer values", {
  expect_error(validate_maturity_index(1.5), "integer")
  expect_error(validate_maturity_index(2.7), "integer")
})

test_that("validate_maturity_index rejects out-of-range values", {
  expect_error(validate_maturity_index(0), "between")
  expect_error(validate_maturity_index(11), "between")
  expect_error(validate_maturity_index(-1), "between")
})

test_that("validate_maturity_index rejects invalid types", {
  expect_error(validate_maturity_index("a"), "single finite numeric")
  expect_error(validate_maturity_index(NA), "single finite numeric")
  expect_error(validate_maturity_index(Inf), "single finite numeric")
  expect_error(validate_maturity_index(c(1, 2)), "single finite numeric")
  expect_error(validate_maturity_index(NULL), "single finite numeric")
})

test_that("validate_maturity_index respects custom max_maturity", {
  expect_error(validate_maturity_index(10, max_maturity = 9), "between")
  expect_true(validate_maturity_index(9, max_maturity = 9))
})

test_that("validate_maturity_index accepts valid inputs", {
  expect_true(validate_maturity_index(1))
  expect_true(validate_maturity_index(10))
  expect_true(validate_maturity_index(5L))
  expect_true(validate_maturity_index(5.0))
})
