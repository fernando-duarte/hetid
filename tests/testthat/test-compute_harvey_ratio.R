test_that("Harvey ratios divide on positive rows and keep zero rows exact", {
  x_mat <- cbind(1, c(-1, 0, 1, 2))
  theta <- c(0.2, -0.1)
  y <- c(0, 1, 2, 3)
  reference <- y / exp(drop(x_mat %*% theta))
  ratio <- compute_harvey_ratio(theta, y, x_mat)
  expect_lte(max(abs(ratio - reference)), 1e-12 * max(1, max(abs(reference))))
  expect_identical(ratio[1], 0)
  expect_null(names(compute_harvey_ratio(theta, stats::setNames(y, letters[1:4]), x_mat)))
  expect_identical(compute_harvey_ratio(theta, rep(0, 4), x_mat), rep(0, 4))
})

test_that("Harvey ratio extremes preserve underflow and overflow", {
  x_mat <- cbind(1, c(0, 0))
  expect_identical(compute_harvey_ratio(c(800, 0), c(0, 1), x_mat), c(0, exp(-800)))
  expect_identical(compute_harvey_ratio(c(-800, 0), c(0, 1), x_mat), c(0, Inf))
  expect_identical(compute_harvey_ratio(c(-800, 0), c(0, 0), x_mat), c(0, 0))
  huge <- matrix(.Machine$double.xmax, 2, 1)
  expect_identical(compute_harvey_ratio(2, c(0, 1), huge), c(0, 0))
  expect_identical(compute_harvey_ratio(-2, c(0, 1), huge), c(0, Inf))
})

test_that("Harvey ratio inputs are validated without changing positional order", {
  x_mat <- cbind(a = 1, b = c(-1, 0, 1))
  expect_identical(
    compute_harvey_ratio(c(b = 0.2, a = 0.1), c(1, 2, 3), x_mat),
    compute_harvey_ratio(c(0.2, 0.1), c(1, 2, 3), x_mat)
  )
  for (bad in list(NULL, "x", matrix(0, 1, 2), NA_real_, Inf)) {
    expect_error(compute_harvey_ratio(bad, rep(1, 3), x_mat),
      class = "hetid_error_bad_argument"
    )
  }
  for (bad in list("x", matrix(1, 3, 1), c(-1, 1, 1), c(NA, 1, 1), c(Inf, 1, 1))) {
    expect_error(compute_harvey_ratio(c(0, 0), bad, x_mat),
      class = "hetid_error_bad_argument"
    )
  }
  expect_error(compute_harvey_ratio(0, rep(1, 3), x_mat),
    class = "hetid_error_dimension_mismatch"
  )
  expect_error(compute_harvey_ratio(c(0, 0), 1, x_mat),
    class = "hetid_error_dimension_mismatch"
  )
  for (bad in list(
    1, data.frame(v = 1), matrix(0, 0, 1), matrix(0, 1, 0),
    matrix("x", 1, 1), matrix(NA_real_, 1, 1), matrix(Inf, 1, 1)
  )) {
    expect_error(compute_harvey_ratio(0, 1, bad), class = "hetid_error_bad_argument")
  }
})
