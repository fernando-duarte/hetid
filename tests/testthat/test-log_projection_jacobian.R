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
