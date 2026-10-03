test_that("Harvey prechecks retain the standalone reason order", {
  v <- seq(-1, 1, length.out = 31)
  x_mat <- cbind(1, v)
  y <- exp(0.2 + 0.3 * v) * (1.1 + 0.4 * cos(seq_along(v)))
  pairs <- list(
    good = list(y = y, start = c(log(mean(y)), 0)),
    short = list(y = y[-1], start = c(0, 0)),
    negative = list(y = -y, start = c(0, 0)),
    missing = list(y = replace(y, 1, NA_real_), start = c(0, 0)),
    missing_start = list(y = y, start = c(NA_real_, 0)),
    overflow_start = list(y = y, start = c(800, 0)),
    zero_mu = list(y = rep(0, 31), start = c(-800, 0)),
    proposal = list(y = rep(1e10, 31), start = c(0, 0))
  )
  expect_identical(
    precheck_harvey_starts(pairs, x_mat),
    stats::setNames(c(
      NA_character_, "invalid_response", "invalid_response",
      "invalid_response", "invalid_start", "nonfinite_start_eval",
      "nonpositive_mu", "proposal_nonfinite"
    ), names(pairs))
  )
  huge <- cbind(1, c(-1e150, 1e150, -1e150, 1e150))
  expect_identical(precheck_harvey_starts(
    list(info = list(y = rep(1e10, 4), start = c(0, 0))), huge
  ), c(info = "nonfinite_info"))
})

test_that("Harvey precheck success is separate from fitting", {
  x_mat <- cbind(1, c(-1, 0, 1))
  pair <- list(y = rep(0, 3), start = c(0, 0))
  expect_identical(
    precheck_harvey_starts(list(all_zero = pair), x_mat),
    c(all_zero = NA_character_)
  )
  fit <- fit_log_variance(pair$y, x_mat[, -1, drop = FALSE], "harvey")
  expect_false(log_variance_fit_ok(fit))
  testthat::local_mocked_bindings(
    fit_log_variance = function(...) stop("attempted fit"),
    make_log_variance_fitter = function(...) stop("created fitter"),
    harvey_fit_response = function(...) stop("attempted response fit"),
    harvey_scoring = function(...) stop("scoring iterations"), .package = "hetid"
  )
  expect_identical(
    precheck_harvey_starts(list(all_zero = pair), x_mat),
    c(all_zero = NA_character_)
  )
})

test_that("Harvey prechecks preserve pair names and classify vector defects", {
  x_mat <- cbind(1, c(-1, 0, 1))
  pair <- list(y = rep(1, 3), start = c(0, 0), extra = 1)
  expect_identical(
    precheck_harvey_starts(list(pair, pair), x_mat),
    rep(NA_character_, 2)
  )
  expect_identical(precheck_harvey_starts(
    stats::setNames(list(pair, pair), c("a", "a")),
    x_mat
  ), stats::setNames(rep(NA_character_, 2), c("a", "a")))
  expect_identical(precheck_harvey_starts(list(), x_mat), character(0))
  bad <- list(
    absent = list(), wrong_y = list(y = "x", start = c(0, 0)),
    matrix_y = list(y = matrix(1, 3, 1), start = c(0, 0)),
    absent_start = list(y = rep(1, 3)),
    matrix_start = list(y = rep(1, 3), start = matrix(0, 2, 1)),
    short_start = list(y = rep(1, 3), start = 0)
  )
  expect_identical(
    unname(precheck_harvey_starts(bad, x_mat)),
    c(rep("invalid_response", 3), rep("invalid_start", 3))
  )
})

test_that("unusable shared Harvey designs raise structured conditions", {
  x_mat <- cbind(1, c(-1, 0, 1))
  expect_error(precheck_harvey_starts(1, x_mat), class = "hetid_error_bad_argument")
  expect_error(precheck_harvey_starts(list(1), x_mat), class = "hetid_error_bad_argument")
  for (bad in list(
    1, data.frame(v = 1), matrix(0, 0, 1), matrix(0, 1, 0),
    matrix("x", 1, 1), matrix(NA_real_, 1, 1), matrix(Inf, 1, 1),
    cbind(1, rep(1, 3)), matrix(.Machine$double.xmax, 3, 1)
  )) {
    cond <- tryCatch(precheck_harvey_starts(list(), bad), error = identity)
    expect_s3_class(cond, "hetid_error_bad_argument")
    expect_identical(cond$arg, "x_mat")
  }
})
