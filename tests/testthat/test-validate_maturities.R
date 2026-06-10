# Tests for the shared vector maturity validator and the scalar primitive.

test_that("validate_maturities accepts a valid vector invisibly", {
  expect_invisible(validate_maturities(c(1, 2, 3), max_value = 5))
  expect_true(validate_maturities(c(1, 2, 3), max_value = 5))
})

test_that("validate_maturities rejects an empty vector", {
  expect_error(
    validate_maturities(integer(0), max_value = 5),
    "maturities must not be empty",
    class = "hetid_error_bad_argument"
  )
})

test_that("validate_maturities rejects non-integer values", {
  expect_error(
    validate_maturities(c(1, 2.5), max_value = 5),
    "maturities must be finite integer values",
    class = "hetid_error_bad_argument"
  )
})

test_that("validate_maturities rejects a numeric matrix", {
  expect_error(
    validate_maturities(matrix(c(1, 2), 1, 2), max_value = 3),
    "maturities must be finite integer values",
    class = "hetid_error_bad_argument"
  )
})

test_that("validate_maturities rejects values below the minimum", {
  expect_error(
    validate_maturities(c(0, 1), max_value = 5),
    "must be between 1 and",
    class = "hetid_error_bad_argument"
  )
})

test_that("validate_maturities rejects values above the bound", {
  expect_error(
    validate_maturities(c(1, 9), max_value = 5),
    "must be between 1 and",
    class = "hetid_error_bad_argument"
  )
})

test_that("validate_maturities reports deduped offending values", {
  expect_error(
    validate_maturities(c(0, 9, 9), max_value = 5),
    "invalid: 0, 9"
  )
})

test_that("validate_maturities renders max_label label-first", {
  expect_error(
    validate_maturities(9, max_value = 4, max_label = "ncol(gamma)"),
    "between 1 and ncol\\(gamma\\) \\(4\\)"
  )
})

test_that("validate_maturities rejects duplicate values", {
  expect_error(
    validate_maturities(c(2, 2), max_value = 5),
    "maturities must not contain duplicates",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    validate_maturities(c(1, 3, 1), max_value = 5),
    "must not contain duplicates; got: 1, 3, 1",
    class = "hetid_error_bad_argument"
  )
})

# --- assert_scalar_integer_in_range ---

test_that("assert_scalar_integer_in_range accepts a valid scalar", {
  expect_invisible(
    assert_scalar_integer_in_range(3, "x", 1, 5)
  )
})

test_that("assert_scalar_integer_in_range rejects non-integers", {
  expect_error(
    assert_scalar_integer_in_range(2.5, "x", 1, 5),
    "x must be an integer",
    class = "hetid_error_bad_argument"
  )
})

test_that("assert_scalar_integer_in_range enforces the range", {
  expect_error(
    assert_scalar_integer_in_range(0, "x", 1, 5),
    "x must be between 1 and 5",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    assert_scalar_integer_in_range(6, "x", 1, 5),
    "x must be between 1 and 5",
    class = "hetid_error_bad_argument"
  )
})
