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

test_that("get_pc_column_names builds prefixed names", {
  expect_identical(get_pc_column_names(3), c("pc1", "pc2", "pc3"))
})

test_that("get_pc_column_names returns an empty vector for zero PCs", {
  expect_identical(get_pc_column_names(0), character(0))
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

test_that("run_pc_regression errors when complete observations are too few", {
  # Three complete rows with two PCs is below the n_pcs + 2 minimum,
  # which would otherwise produce a saturated fit (R-squared of one)
  y <- c(1, 2, 3, NA)
  pcs <- matrix(
    c(0.1, 0.2, 0.3, 0.4, 0.4, 0.3, 0.2, 0.1),
    nrow = 4, ncol = 2
  )
  expect_error(
    run_pc_regression(y, pcs, 2),
    class = "hetid_error_insufficient_data"
  )
})

test_that("compute_time_series_news errors on length mismatch", {
  expect_error(
    compute_time_series_news(c(1, 2, 3), c(4, 5)),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that(
  "compute_time_series_news returns empty for single element",
  {
    result <- compute_time_series_news(1.0, 2.0)
    expect_length(result, 0)
    expect_type(result, "double")
  }
)

test_that(
  "compute_time_series_news works at boundary of two elements",
  {
    result <- compute_time_series_news(
      c(1, 2), c(3, 4)
    )
    expect_equal(result, 4 - 1)
    expect_length(result, 1)
  }
)

test_that(
  "compute_time_series_news handles negation with short input",
  {
    result <- compute_time_series_news(
      c(1, 2), c(3, 4),
      negate = TRUE
    )
    expect_equal(result, -(4 - 1))
  }
)

test_that("prepare_return_data builds a generic time index when dates are NULL", {
  df <- prepare_return_data(
    result_series = c(0.1, 0.2, 0.3),
    return_df = TRUE,
    dates = NULL,
    yields = matrix(0, nrow = 3, ncol = 1),
    series_name = "x"
  )

  expect_s3_class(df, "data.frame")
  expect_named(df, c("date", "x"))
  expect_equal(df$date, seq_len(3))
  expect_equal(df$x, c(0.1, 0.2, 0.3))
})
