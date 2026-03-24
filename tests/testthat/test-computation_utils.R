test_that("compute_expected_squared errors on all-NA input", {
  expect_error(
    compute_expected_squared(c(NA, NA, NA)),
    "No valid values"
  )
})

test_that("compute_expected_squared errors with custom message", {
  expect_error(
    compute_expected_squared(NA_real_, "custom failure"),
    "custom failure"
  )
})

test_that("apply_time_series_transform works with single series", {
  series <- c(1, 4, NA, 9)
  result <- apply_time_series_transform(
    series,
    transform_fn = sqrt
  )

  expect_equal(result[1], 1)
  expect_equal(result[2], 2)
  expect_true(is.na(result[3]))
  expect_equal(result[4], 3)
})

test_that("apply_time_series_transform single series handles all-NA", {
  series <- c(NA_real_, NA_real_)
  result <- apply_time_series_transform(
    series,
    transform_fn = sqrt
  )

  expect_true(all(is.na(result)))
  expect_length(result, 2)
})
