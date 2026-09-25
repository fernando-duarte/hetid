# A scalar feasible interval with a response that reveals its sampled theta.
sample_box_fixture <- function() {
  parts <- make_box_parts(n_components = 1L, n_obs = 20L)
  parts$w1 <- rep(2, 20)
  parts$w2 <- matrix(1, 20, 1, dimnames = list(NULL, "news1"))
  parts$arg_lower <- matrix(-1, 1, 1)
  parts$arg_upper <- matrix(1, 1, 1)
  list(
    box = do.call(new_hetid_theta_box, parts),
    x = matrix(seq(-1, 1, length.out = 20), ncol = 1, dimnames = list(NULL, "v1"))
  )
}

sample_mock_fit <- function(coef = NULL) {
  ok <- !is.null(coef)
  list(
    coef = coef, fit_status = if (ok) "ok" else "nonconvergence", converged = ok,
    objective = if (ok) 0 else NA_real_, score_norm = if (ok) 0 else NA_real_,
    convergence_code = if (ok) 0L else -1L, warm_start = coef,
    diagnostics = list(error_class = if (ok) NA_character_ else "fixture_failure")
  )
}
