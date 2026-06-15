test_that("build_y1_lag_columns shifts each column and pads with leading NAs", {
  mat <- build_y1_lag_columns(1:5, 2)

  expect_true(is.matrix(mat))
  expect_equal(dim(mat), c(5L, 2L))
  expect_equal(colnames(mat), c("l.y1", "l2.y1"))
  expect_equal(mat[, 1], as.numeric(1:5))
  expect_equal(mat[, 2], c(NA, 1, 2, 3, 4))
})

test_that("build_y1_lag_columns: lag 1 has no NA, lag h has h-1 leading NAs", {
  mat <- build_y1_lag_columns(seq_len(8), 4)

  expect_equal(
    colSums(is.na(mat)),
    stats::setNames(as.numeric(0:3), lag_grammar_names("y1", 4L))
  )
  # Column h equals the outcome shifted down by h-1
  expect_equal(mat[, 4], c(NA, NA, NA, 1, 2, 3, 4, 5))
})

test_that("validate_y1_lags accepts a non-negative integer and returns it", {
  expect_identical(validate_y1_lags(2, 60), 2L)
  expect_identical(validate_y1_lags(0, 60), 0L)
})

test_that("validate_y1_lags rejects non-integer / negative / non-scalar", {
  expect_error(validate_y1_lags(-1, 60), class = "hetid_error_bad_argument")
  expect_error(validate_y1_lags(1.5, 60), class = "hetid_error_bad_argument")
  expect_error(validate_y1_lags(c(1, 2), 60), class = "hetid_error_bad_argument")
  expect_error(validate_y1_lags(NA_real_, 60), class = "hetid_error_bad_argument")
})

test_that("validate_y1_lags errors when lags exceed usable history", {
  expect_error(
    validate_y1_lags(60, 5),
    class = "hetid_error_insufficient_data"
  )
})
