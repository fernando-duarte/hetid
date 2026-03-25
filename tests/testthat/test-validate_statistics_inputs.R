test_that("validate_statistics_inputs rejects non-numeric w1", {
  expect_error(
    validate_statistics_inputs("not numeric", matrix(1:6, 3, 2)),
    "w1 must be a numeric vector"
  )
})

test_that("validate_statistics_inputs rejects matrix w1", {
  expect_error(
    validate_statistics_inputs(
      matrix(1:6, 3, 2), matrix(1:6, 3, 2)
    ),
    "w1 must be a numeric vector"
  )
})

test_that("validate_statistics_inputs rejects non-matrix w2", {
  expect_error(
    validate_statistics_inputs(1:3, "not matrix"),
    "w2 must be a matrix or data frame"
  )
})

test_that(
  "validate_statistics_inputs rejects dimension mismatch",
  {
    expect_error(
      validate_statistics_inputs(
        1:5, matrix(1:12, 6, 2)
      ),
      "same number of observations"
    )
  }
)

test_that(
  "validate_statistics_inputs rejects non-integer maturities",
  {
    expect_error(
      validate_statistics_inputs(
        1:5, matrix(1:10, 5, 2),
        maturities = c(1.5, 2)
      ),
      "finite integer"
    )
    expect_error(
      validate_statistics_inputs(
        1:5, matrix(1:10, 5, 2),
        maturities = c(NA, 1)
      ),
      "finite integer"
    )
  }
)

test_that(
  "validate_statistics_inputs rejects out-of-range maturities",
  {
    expect_error(
      validate_statistics_inputs(
        1:5, matrix(1:10, 5, 2),
        maturities = c(0, 1)
      ),
      "between 1"
    )
    expect_error(
      validate_statistics_inputs(
        1:5, matrix(1:10, 5, 2),
        maturities = c(1, 3)
      ),
      "between 1"
    )
  }
)

test_that(
  "validate_statistics_inputs returns validated components",
  {
    result <- validate_statistics_inputs(
      1:5, data.frame(a = 1:5, b = 6:10)
    )
    expect_true(is.matrix(result$w2))
    expect_equal(result$t_obs, 5L)
    expect_equal(result$maturities, 1:2)
  }
)

test_that(
  "validate_statistics_inputs accepts explicit maturities",
  {
    w2 <- matrix(1:15, 5, 3)
    result <- validate_statistics_inputs(
      1:5, w2,
      maturities = c(2, 3)
    )
    expect_equal(result$maturities, c(2, 3))
  }
)
