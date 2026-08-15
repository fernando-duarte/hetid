# Tests for the Harvey solver primitives: the zero-safe ratio, the guarded
# point evaluation and its gradient identity, the Cholesky solve, and the two
# gates that reject a point the scoring loop reached. Everything here is
# unexported, so it is reached via hetid:::.

harvey_parts <- function(theta = c(-0.3, 0.2, -0.1)) {
  d <- simulate_logvar_data()
  x_mat <- hetid:::log_variance_design(d$x)
  list(
    y = d$y, x_mat = x_mat, theta = theta, pos = d$y > 0,
    col_abs = colSums(abs(x_mat))
  )
}

test_that("harvey_ratio keeps zero rows exact and divides elsewhere", {
  p <- harvey_parts()
  y <- p$y
  y[c(3, 17)] <- 0
  r <- hetid:::harvey_ratio(p$theta, y, p$x_mat)
  expect_identical(r[c(3, 17)], c(0, 0))
  eta <- drop(p$x_mat %*% p$theta)
  expect_equal(r[-c(3, 17)], (y / exp(eta))[-c(3, 17)], tolerance = 1e-12)

  # an all-zero response is all-zero ratio, with no 0 * Inf anywhere
  expect_identical(
    hetid:::harvey_ratio(p$theta, rep(0, length(y)), p$x_mat),
    rep(0, length(y))
  )
})

test_that("harvey_eval rejects non-finite and overflowing points", {
  p <- harvey_parts()
  expect_null(hetid:::harvey_eval(c(NA, 0, 0), p$y, p$x_mat, p$pos, p$col_abs))
  expect_null(hetid:::harvey_eval(
    c(1e6, 1e6, 1e6), p$y, p$x_mat, p$pos, p$col_abs
  ))
  # eta below the overflow bound but far enough down to blow the ratio up
  expect_null(hetid:::harvey_eval(
    c(-750, 0, 0), p$y, p$x_mat, p$pos, p$col_abs
  ))

  ev <- hetid:::harvey_eval(p$theta, p$y, p$x_mat, p$pos, p$col_abs)
  expect_named(
    ev, c("theta", "eta", "r", "q", "moment", "score_norm")
  )
  expect_equal(
    ev$q, 0.5 * (sum(ev$eta) + sum(ev$r)),
    tolerance = 1e-12
  )
  expect_equal(
    ev$score_norm, max(abs(ev$moment) / p$col_abs),
    tolerance = 1e-12
  )
})

test_that("the moment is minus twice the criterion's gradient", {
  p <- harvey_parts()
  ev <- hetid:::harvey_eval(p$theta, p$y, p$x_mat, p$pos, p$col_abs)
  h <- 1e-5
  gradient <- vapply(seq_along(p$theta), function(j) {
    up <- p$theta
    up[j] <- up[j] + h
    down <- p$theta
    down[j] <- down[j] - h
    (hetid:::harvey_eval(up, p$y, p$x_mat, p$pos, p$col_abs)$q -
      hetid:::harvey_eval(down, p$y, p$x_mat, p$pos, p$col_abs)$q) / (2 * h)
  }, numeric(1))
  expect_equal(unname(ev$moment), -2 * gradient, tolerance = 1e-5)
})

test_that("harvey_chol_solve applies the cross-product's inverse", {
  p <- harvey_parts()
  xx <- crossprod(p$x_mat)
  rhs <- c(0.4, -1.2, 3)
  expect_equal(
    unname(drop(hetid:::harvey_chol_solve(chol(xx), rhs))),
    unname(drop(solve(xx, rhs))),
    tolerance = 1e-10
  )
})

test_that("scoring exits at code 0 when the start already solves the moment", {
  p <- harvey_parts()
  fit <- hetid:::harvey_fit_response(p$y, p$x_mat)
  ev <- hetid:::harvey_eval(fit$coef, p$y, p$x_mat, p$pos, p$col_abs)
  scored <- hetid:::harvey_scoring(
    ev, p$y, p$x_mat, p$pos, p$col_abs, chol(crossprod(p$x_mat))
  )
  expect_identical(scored$status, "converged")
  expect_identical(scored$iters, 0L)
  expect_identical(scored$halves, 0L)
})

test_that("the line search stalls when no halving can improve the point", {
  p <- harvey_parts()
  fit <- hetid:::harvey_fit_response(p$y, p$x_mat)
  ev <- hetid:::harvey_eval(fit$coef, p$y, p$x_mat, p$pos, p$col_abs)
  # every halving of a negligible direction ties on the criterion and shows no
  # score progress, which is exactly the stall the search must report
  expect_null(hetid:::harvey_line_search(
    ev, rep(1e-30, 3), p$y, p$x_mat, p$pos, p$col_abs
  ))
})

test_that("a column with no positive-response support fails both gates", {
  # the volatility regressor is nonzero only where the response is zero, so
  # its information column is exactly zero: this is the rank deficiency the
  # post-stop gate is there to catch, since rank_x_pos itself gates nothing
  set.seed(5)
  y <- c(abs(stats::rnorm(28)) + 0.1, rep(0, 12))
  x_mat <- hetid:::log_variance_design(cbind(v = c(rep(0, 28), rep(1, 12))))
  pos <- y > 0
  col_abs <- colSums(abs(x_mat))

  ev <- hetid:::harvey_eval(c(0, 0), y, x_mat, pos, col_abs)
  expect_null(hetid:::harvey_newton_dir(ev, x_mat))
  expect_null(hetid:::harvey_post_stop(c(0, 0), y, x_mat, pos, col_abs))

  # and the whole solve fails closed on it, without a recession certificate
  fit <- hetid:::harvey_fit_response(y, x_mat)
  expect_identical(fit$fit_status, "nonconvergence")
  expect_identical(fit$diagnostics$error_class, "iteration_cap")
  expect_identical(fit$diagnostics$rank_x_pos, 1L)
  expect_identical(fit$diagnostics$n_zero_response, 12L)
})
