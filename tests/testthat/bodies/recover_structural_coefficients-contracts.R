{
  set.seed(201)
  fx <- make_structural_fixture(n_lags = 3)
  lag_cols <- grep("^l[0-9]*\\.y1$", colnames(fx$beta2r))
  # The estimated lag slopes in beta2r are genuinely nonzero (estimate-B)
  expect_true(all(abs(fx$beta2r[, lag_cols]) > 0))

  # Recovered psi rows match a direct OLS of (y1 - y2'theta) on the common X,
  # i.e. psi is a set-valued linear image of theta, not a fixed point
  theta <- c(0.6, -0.5)
  direct <- coef(stats::lm((fx$y1 - fx$y2 %*% theta) ~ fx$x - 1))
  recovered <- recover_structural_coefficients(fx$beta1r, fx$beta2r, theta)
  expect_equal(unname(recovered), unname(direct))

  # The psi rows actually vary with theta (affine, not constant)
  rec0 <- recover_structural_coefficients(fx$beta1r, fx$beta2r, c(0, 0))
  expect_true(all(abs((recovered - rec0)[lag_cols]) > 0))
}

{
  set.seed(202)
  fx <- make_structural_fixture(n_lags = 3, zero_lag_block = TRUE)
  lag_cols <- grep("^l[0-9]*\\.y1$", colnames(fx$beta2r))
  expect_true(all(fx$beta2r[, lag_cols] == 0))

  rec0 <- recover_structural_coefficients(fx$beta1r, fx$beta2r, c(0, 0))
  rec1 <- recover_structural_coefficients(fx$beta1r, fx$beta2r, c(1.3, -2.1))
  # Lag rows do not move with theta; they equal beta1r exactly
  expect_equal(rec1[lag_cols], rec0[lag_cols])
  expect_equal(unname(rec1[lag_cols]), unname(fx$beta1r[lag_cols]))
  # The PC rows still move (their beta2r block is nonzero)
  pc_cols <- grep("^pc[0-9]+$", colnames(fx$beta2r))
  expect_true(all(abs((rec1 - rec0)[pc_cols]) > 0))
}
