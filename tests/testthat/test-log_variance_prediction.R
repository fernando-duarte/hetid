test_that("joint cancellation is preserved instead of combining marginal corners", {
  parts <- sample_box_fixture()
  testthat::local_mocked_bindings(make_log_variance_fitter = function(...) {
    function(y, ...) {
      theta <- 2 - sqrt(y[1])
      sample_mock_fit(c("(Intercept)" = theta, v1 = theta))
    }
  })
  sampled <- sample_log_variance_set(parts$box, parts$x, n_points = 2)
  out <- predict(sampled, matrix(c(-1, 1), 2, 1))
  expect_equal(out$bounds$lower, c(0, -2))
  expect_equal(out$bounds$upper, c(0, 2))
  expect_equal(sampled$bounds$lower, c(-1, -1))
  expect_equal(sampled$bounds$upper, c(1, 1))
  for (side in c("lower", "upper")) {
    ids <- out[[paste0("candidate_", side)]]
    expect_equal(out[[paste0("arg_", side)]], sampled$candidates[ids, , drop = FALSE],
      ignore_attr = TRUE
    )
    expected <- rowSums(out$design * sampled$coefficients[ids, , drop = FALSE])
    expect_equal(out$bounds[[side]], unname(expected))
  }
  expect_equal(predict(sampled, matrix(1, 1, 1), type = "variance")$bounds$upper, exp(2))
  expect_equal(predict(sampled, matrix(1, 1, 1), type = "volatility")$bounds$upper, exp(1))
  expect_equal(predict(sampled, matrix(1, 1, 1), include_intercept = FALSE)$bounds$upper, 1)
})

test_that("a nonlinear fitted map can attain an extremum at an interior sample", {
  parts <- sample_box_fixture()
  testthat::local_mocked_bindings(make_log_variance_fitter = function(...) {
    function(y, ...) sample_mock_fit(c("(Intercept)" = (2 - sqrt(y[1]))^2, v1 = 0))
  })
  out <- predict(sample_log_variance_set(parts$box, parts$x), matrix(0, 1, 1))
  expect_equal(out$bounds$lower, 0)
  expect_equal(out$bounds$upper, 1)
  expect_equal(unname(out$arg_lower[1, ]), 0)
})

test_that("design identity, dates and empty prediction rows are explicit", {
  parts <- sample_box_fixture()
  dates <- as.Date("2000-01-31") + 0:19
  sampled <- sample_log_variance_set(parts$box, parts$x, dates = dates)
  expect_identical(predict(sampled)$bounds$date, dates)
  future <- as.Date(c("2002-03-31", "2001-12-31"))
  out <- predict(sampled, parts$x[1:2, , drop = FALSE], dates = future)
  expect_identical(out$bounds$date, future)
  expect_null(predict(sampled, parts$x[1:2, , drop = FALSE])$dates)
  expect_error(predict(sampled, matrix(1, 1, 1, dimnames = list(NULL, "wrong"))),
    "columns",
    class = "hetid_error_bad_argument"
  )
  expect_error(predict(sampled, matrix(1, 1, 2)), class = "hetid_error_dimension_mismatch")
  expect_error(predict(sampled, type = "unknown"), class = "hetid_error_bad_argument")
  expect_error(predict(sampled, extra = TRUE), class = "hetid_error_bad_argument")
  expect_error(predict(sampled, dates = rep(dates[1], 20)), class = "hetid_error_bad_argument")
  empty <- predict(sampled, matrix(numeric(0), 0, 1))
  expect_identical(nrow(empty$bounds), 0L)
  expect_identical(dim(empty$arg_lower), c(0L, 1L))
  set.seed(27)
  before <- .Random.seed
  invisible(predict(sampled))
  expect_identical(.Random.seed, before)
})

test_that("finite-eta range failures never become infinite or zero endpoints", {
  parts <- sample_box_fixture()
  testthat::local_mocked_bindings(make_log_variance_fitter = function(...) {
    function(...) sample_mock_fit(c("(Intercept)" = 1000, v1 = -2000))
  })
  sampled <- sample_log_variance_set(parts$box, parts$x)
  out <- predict(sampled, matrix(c(0, 1), 2, 1), type = "variance")
  expect_true(all(is.na(out$bounds$lower)))
  expect_identical(out$bounds$lower_status, c("transform_overflow", "transform_underflow"))
  expect_equal(out$bounds$eta_lower, c(1000, -1000))
  expect_true(all(is.finite(out$arg_lower)))
  nonfinite <- predict(sampled, matrix(1e308, 1, 1))
  expect_true(is.na(nonfinite$bounds$lower))
  expect_identical(nonfinite$bounds$lower_status, "nonfinite_projection")
  expect_true(all(is.na(nonfinite$arg_lower)))
})


test_that("a transformation failure keeps the opposite side and eta witnesses", {
  parts <- sample_box_fixture()
  testthat::local_mocked_bindings(make_log_variance_fitter = function(...) {
    function(y, ...) sample_mock_fit(c("(Intercept)" = 500 * (3 - sqrt(y[1])), v1 = 0))
  })
  sampled <- sample_log_variance_set(parts$box, parts$x)
  out <- predict(sampled, matrix(0, 1, 1), type = "variance")
  expect_equal(out$bounds$lower, 1)
  expect_true(is.na(out$bounds$upper))
  expect_identical(out$bounds$lower_status, "sampled")
  expect_identical(out$bounds$upper_status, "transform_overflow")
  expect_equal(out$bounds$eta_upper, 1000)
  expect_true(all(is.finite(out$arg_upper)))
  expect_equal(predict(sampled, matrix(0, 1, 1), type = "volatility")$bounds$upper, exp(500))
})

test_that("intercept-only designs retain dimensions for one prediction row", {
  parts <- sample_box_fixture()
  sampled <- sample_log_variance_set(parts$box, matrix(numeric(0), 20, 0))
  out <- predict(sampled, matrix(numeric(0), 1, 0))
  expect_identical(dim(out$arg_lower), c(1L, 1L))
  expect_true(all(is.finite(out$bounds$lower)))
  expect_equal(predict(sampled, matrix(numeric(0), 1, 0),
    include_intercept = FALSE
  )$bounds$lower, 0)
})
