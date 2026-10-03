{
  # d_i stays finite (tau_i^2 * V_i / sigma_i_sq), but an Inf in Q_i makes
  # the assembled A_i non-finite, tripping the redundant finiteness guard
  err <- tryCatch(
    quadratic_from_components(
      tau = c(0.5, 0.5),
      L_i = c(1, 1), V_i = c(1, 1),
      Q_i = list(c(Inf, 2), c(3, 4)),
      s_i_0 = c(1, 1),
      s_i_1 = list(c(0, 0), c(0, 0)),
      s_i_2 = list(diag(2), diag(2)),
      sigma_i_sq = c(1, 1),
      maturities = 1:2, n_components = 2
    ),
    error = function(e) e
  )

  expect_s3_class(err, "hetid_error")
  expect_match(
    conditionMessage(err),
    "Assembled quadratic form contains non-finite values",
    fixed = TRUE
  )
}
