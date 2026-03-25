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

test_that("apply_time_series_transform works with two series", {
  s1 <- c(2, 4, 6)
  s2 <- c(1, 2, 3)
  result <- apply_time_series_transform(
    s1, s2,
    transform_fn = function(a, b) a + b
  )
  expect_equal(result, c(3, 6, 9))
})

test_that("apply_time_series_transform two series handles NAs", {
  s1 <- c(1, NA, 3, 4)
  s2 <- c(NA, 2, 3, 4)
  result <- apply_time_series_transform(
    s1, s2,
    transform_fn = function(a, b) a * b
  )
  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_equal(result[3], 9)
  expect_equal(result[4], 16)
})

test_that("apply_time_series_transform two series all-NA", {
  s1 <- c(NA_real_, NA_real_)
  s2 <- c(NA_real_, NA_real_)
  result <- apply_time_series_transform(
    s1, s2,
    transform_fn = function(a, b) a + b
  )
  expect_true(all(is.na(result)))
  expect_length(result, 2)
})

test_that("apply_time_series_transform handles zero-length input", {
  result <- apply_time_series_transform(
    numeric(0),
    transform_fn = sqrt
  )
  expect_length(result, 0)

  result2 <- apply_time_series_transform(
    numeric(0), numeric(0),
    transform_fn = function(a, b) a + b
  )
  expect_length(result2, 0)
})

test_that("apply_time_series_transform errors on length mismatch", {
  expect_error(
    apply_time_series_transform(
      c(1, 2, 3), c(1, 2),
      transform_fn = function(a, b) a + b
    ),
    "equal length"
  )
})

test_that("apply_time_series_transform forwards extra arguments", {
  result <- apply_time_series_transform(
    c(1, 2, 3), c(4, 5, 6),
    transform_fn = function(a, b, offset) a + b + offset,
    10
  )
  expect_equal(result, c(15, 17, 19))
})

test_that("apply_time_series_transform errors on wrong output length", {
  expect_error(
    apply_time_series_transform(
      c(1, 2, 3),
      transform_fn = sum
    ),
    "one value per valid element"
  )
})

test_that("run_pc_regression returns expected structure", {
  set.seed(42)
  y <- rnorm(50)
  pcs <- matrix(rnorm(100), 50, 2)
  result <- run_pc_regression(y, pcs, 2)

  expect_true(is.list(result))
  expect_named(
    result,
    c(
      "residuals", "fitted", "coefficients",
      "r_squared", "model", "complete_idx"
    )
  )
  expect_length(result$residuals, 50)
  expect_true(
    result$r_squared >= 0 && result$r_squared <= 1
  )
  expect_true(all(result$complete_idx))
})
