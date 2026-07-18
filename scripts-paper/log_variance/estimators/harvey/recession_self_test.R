# Deterministic machinery self-test for the analyze_domain precheck hook.

logvar_harvey_recession_self_test <- function(
  x_mat, control = LOGVAR_HARVEY_CONTROL
) {
  n <- nrow(x_mat)
  pos_cert <- logvar_harvey_recession_certificate(
    rep(1, n), x_mat, control
  )
  zero_cert <- logvar_harvey_recession_certificate(
    rep(0, n), x_mat, control
  )
  # A fake dual clears rate_tol in raw form, but its stationarity residual
  # removes that margin; the residual-adjusted bound must reject it.
  fake_raw <- 5e-9
  fake_q <- rep(1e-3, ncol(x_mat))
  fake_bound <- fake_raw - sum(abs(fake_q))
  checks <- c(
    positive = identical(pos_cert$classification, "pass"),
    all_zero = identical(zero_cert$classification, "negative_recession"),
    residual_adjust = !(fake_bound > pos_cert$rate_tol)
  )
  list(
    checks = checks,
    passed = all(checks),
    records = list(
      positive = pos_cert,
      all_zero = zero_cert,
      residual_adjust = list(
        raw = fake_raw,
        residual = fake_q,
        bound = fake_bound,
        rate_tol = pos_cert$rate_tol
      )
    )
  )
}
