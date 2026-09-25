test_that("numeric data failures return labelled unavailable matrices", {
  d <- covariance_fixture()
  cases <- list(
    list(coef = c(NA_real_, 0), y = d$y, x = d$x),
    list(coef = c(1000, 0), y = d$y, x = d$x),
    list(coef = c(-1000, 0), y = d$y, x = d$x),
    list(coef = d$coef, y = replace(d$y, 1, -1), x = d$x),
    list(coef = d$coef, y = replace(d$y, 1, Inf), x = d$x),
    list(coef = d$coef, y = d$y, x = replace(d$x, 1, NA_real_)),
    list(coef = d$coef, y = d$y[1:2], x = d$x[1:2, ]),
    list(coef = c(d$coef, 0), y = d$y, x = cbind(d$x, copy = d$x[, 2])),
    list(coef = d$coef, y = numeric(0), x = d$x[FALSE, , drop = FALSE])
  )
  for (est in c("ppml", "harvey")) {
    for (case in cases) {
      v <- compute_log_variance_vcov_at_coef(case$coef, case$y, case$x, est)
      for (m in v) {
        expect_true(all(is.na(m)))
        expect_identical(dimnames(m), rep(list(colnames(case$x)), 2))
      }
    }
  }
})

test_that("malformed arguments raise structured conditions before covariance work", {
  d <- covariance_fixture()
  run <- function(...) {
    do.call(
      compute_log_variance_vcov_at_coef,
      utils::modifyList(list(coef = d$coef, y = d$y, x_design = d$x), list(...))
    )
  }
  for (bad in list(-1, 0.5, NA_real_, Inf, "4", c(1, 2), .Machine$integer.max + 1)) {
    expect_error(run(hac_lags = bad), class = "hetid_error_bad_argument")
  }
  for (bad in list(0, -1, NA_real_, Inf, "1e-10", c(1, 2))) {
    expect_error(run(rcond_tol = bad), class = "hetid_error_bad_argument")
  }
  expect_error(run(estimator = "bogus"), class = "hetid_error_bad_argument")
  expect_error(run(coef = "wrong"), class = "hetid_error_bad_argument")
  expect_error(run(coef = matrix(d$coef)), class = "hetid_error_bad_argument")
  expect_error(run(y = matrix(d$y)), class = "hetid_error_bad_argument")
  expect_error(run(coef = 1), class = "hetid_error_dimension_mismatch")
  expect_error(run(y = 1), class = "hetid_error_dimension_mismatch")
  expect_error(run(x_design = as.data.frame(d$x)), class = "hetid_error_bad_argument")
  for (labels in list(NULL, c("x", "x"), c("", "x"), c(NA_character_, "x"))) {
    x <- d$x
    colnames(x) <- labels
    expect_error(run(x_design = x), class = "hetid_error_bad_argument")
  }
  expect_error(run(coef = stats::setNames(d$coef, rev(colnames(d$x)))),
    class = "hetid_error_bad_argument"
  )
})
