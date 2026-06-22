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
      "r_squared", "model", "complete_idx", "df_residual"
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

test_that("run_pc_regression errors on a rank-deficient (collinear) design", {
  # Two identical regressors make lm() alias one term to an NA coefficient;
  # the guard must fail loudly instead of propagating an under-ranked fit.
  set.seed(1)
  n <- 50
  x <- rnorm(n)
  pcs <- cbind(pc1 = x, pc2 = x)
  y <- x + rnorm(n)
  expect_error(
    run_pc_regression(y, pcs, n_pcs = 2),
    "Rank-deficient regression design",
    class = "hetid_error"
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

test_that("prepare_return_data rejects NULL dates (a Date index is required)", {
  expect_error(
    prepare_return_data(
      result_series = c(0.1, 0.2, 0.3),
      dates = NULL,
      yields = matrix(0, nrow = 3, ncol = 1),
      series_name = "x"
    ),
    class = "hetid_error_bad_argument"
  )
})

test_that("prepare_return_data rejects an integer (fabricated) index for dates", {
  expect_error(
    prepare_return_data(
      result_series = c(0.1, 0.2, 0.3),
      dates = 1:3,
      yields = matrix(0, nrow = 3, ncol = 1),
      series_name = "x"
    ),
    class = "hetid_error_bad_argument"
  )
})

test_that("prepare_return_data prepends NA for a news series", {
  dts <- seq(as.Date("1990-03-31"), by = "quarter", length.out = 3)
  df <- prepare_return_data(
    result_series = c(0.2, 0.3),
    dates = dts,
    yields = matrix(0, nrow = 3, ncol = 1),
    series_name = "x",
    is_news = TRUE
  )
  expect_s3_class(df$date, "Date")
  expect_equal(df$date, dts)
  expect_equal(df$x, c(NA, 0.2, 0.3))
})

test_that("prepare_return_data keeps a level series aligned one-to-one", {
  dts <- seq(as.Date("1990-03-31"), by = "quarter", length.out = 3)
  df <- prepare_return_data(
    result_series = c(0.1, 0.2, 0.3),
    dates = dts,
    yields = matrix(0, nrow = 3, ncol = 1),
    series_name = "x",
    is_news = FALSE
  )
  expect_s3_class(df$date, "Date")
  expect_equal(df$date, dts)
  expect_equal(df$x, c(0.1, 0.2, 0.3))
})

test_that("prepare_return_data errors on a level series of the wrong length", {
  dts <- seq(as.Date("1990-03-31"), by = "quarter", length.out = 3)
  expect_error(
    prepare_return_data(
      result_series = c(0.1, 0.2),
      dates = dts,
      yields = matrix(0, nrow = 3, ncol = 1),
      series_name = "x",
      is_news = FALSE
    ),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("prepare_return_data errors on a news series of the wrong length", {
  dts <- seq(as.Date("1990-03-31"), by = "quarter", length.out = 3)
  expect_error(
    prepare_return_data(
      result_series = c(0.1, 0.2, 0.3),
      dates = dts,
      yields = matrix(0, nrow = 3, ncol = 1),
      series_name = "x",
      is_news = TRUE
    ),
    class = "hetid_error_dimension_mismatch"
  )
})
