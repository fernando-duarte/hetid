lp_check_jacobian <- function(prep, b, method) {
  f <- function(bb) {
    evaluate_log_projection(prep, bb, method, jacobian = FALSE)$coef
  }
  fit <- evaluate_log_projection(prep, b, method)
  expect_identical(fit$status, "ok")
  expect_lt(lp_fd_error(f, fit$jacobian, b), 1e-6)
}

test_that("the log Jacobian matches central differences", {
  fx <- lp_fixture()
  lp_check_jacobian(lp_prep(fx), fx$b, "log")
  same <- lp_fixture(n_vol = 80L)
  lp_check_jacobian(lp_prep(same), same$b, "log")
  no_risk <- lp_fixture(d_r = 0L)
  lp_check_jacobian(lp_prep(no_risk), no_risk$b, "log")
  one_news <- lp_fixture(d_n = 1L)
  lp_check_jacobian(lp_prep(one_news), one_news$b, "log")
})

test_that("regularized Jacobians match central differences", {
  for (method in c("log_plus", "log_fuller")) {
    fx <- lp_fixture()
    prep <- lp_prep(fx)
    lp_check_jacobian(prep, fx$b, method)
    near <- unname(c(fx$b[1], (prep$w1[1] - prep$w2[1, 1] * fx$b[1]) / prep$w2[1, 2]))
    lp_check_jacobian(prep, near, method)
    lp_check_jacobian(lp_prep(lp_fixture(n_vol = 80L)), fx$b, method)
    no_risk <- lp_fixture(d_r = 0L)
    lp_check_jacobian(lp_prep(no_risk), no_risk$b, method)
    one_news <- lp_fixture(d_n = 1L)
    lp_check_jacobian(lp_prep(one_news), one_news$b, method)
    zf <- lp_zero_fixture()
    zprep <- prepare_log_projection(zf$w1, zf$w2, zf$x_var, zf$mean_ids, zf$vol_ids)
    lp_check_jacobian(zprep, 0, method)
  }
})
