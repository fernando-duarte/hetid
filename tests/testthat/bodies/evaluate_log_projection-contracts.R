{
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  expect_error(evaluate_log_projection(prep, fx$b, "log_minus"),
    class = "hetid_error_bad_argument"
  )
  for (m in c(0, -1, NA, Inf)) {
    expect_error(evaluate_log_projection(prep, fx$b, "log", multiplier = m),
      class = "hetid_error_bad_argument"
    )
  }
  expect_error(evaluate_log_projection(list(), fx$b, "log"),
    class = "hetid_error_bad_argument"
  )
  expect_error(evaluate_log_projection(prep, c(Inf, 1), "log"),
    class = "hetid_error_bad_argument"
  )
  expect_error(evaluate_log_projection(prep, numeric(3), "log"),
    class = "hetid_error_dimension_mismatch"
  )
}

{
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  b_mat <- matrix(fx$b, nrow = 1L)
  e <- prep$w1 - prep$w2 %*% t(b_mat)
  e_mean <- prep$w1_mean - prep$w2_mean %*% t(b_mat)
  pass <- hetid:::log_projection_fuller(prep, e_mean, e, 2 * log(abs(e)), 1)
  expect_equal(mean(exp(pass$work$log_ratio[, 1])), 1, tolerance = 1e-12)
  set.seed(9)
  conc <- fx
  pc1 <- fx$x_var[, 1] - mean(fx$x_var[, 1])
  tail_rows <- utils::tail(seq_along(fx$w1), length(pc1))
  conc$w1[tail_rows] <- fx$w1[tail_rows] * exp(3 * pc1)
  conc$w1 <- conc$w1 - mean(conc$w1)
  fit <- evaluate_log_projection(lp_prep(conc), c(0, 0), "log_fuller")
  expect_identical(fit$status, "ok")
  base <- evaluate_log_projection(lp_prep(fx), c(0, 0), "log_fuller")
  expect_gt(
    fit$diagnostics$profile_log_ratio_max,
    base$diagnostics$profile_log_ratio_max + 0.5
  )
  expect_lte(fit$diagnostics$profile_log_ratio_max, log(length(pc1)))
}
