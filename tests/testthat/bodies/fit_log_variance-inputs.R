{
  # simulate_logvar_data()'s fixture is pinned clean by the parity and
  # score-equation tests above, and the ladder's final rung falls back to
  # glm.fit's own data-driven start (mustart from y), which converges in a
  # handful of iterations for every fixture searched (bimodal responses,
  # extreme outliers, near-collinear and huge-scale regressors) -- so a
  # fixture where the full ladder fails closed while still carrying a
  # warning could not be constructed deterministically. Substituting a
  # capture_glm_conditions()-level check around a real glm.fit call that
  # does warn (a supplied start far from the data-driven optimum, which
  # hits maxit): the warning must land in the recorded list and nothing
  # may escape, including through the full ladder that then recovers
  d <- simulate_logvar_data()
  expect_no_warning(fit_log_variance(d$y, d$x))

  x_mat <- hetid:::log_variance_design(d$x)
  captured <- NULL
  expect_no_warning(
    captured <- hetid:::capture_glm_conditions(stats::glm.fit(
      x = x_mat, y = d$y, family = stats::quasipoisson(link = "log"),
      start = c(100, 0, 0),
      control = stats::glm.control(
        epsilon = LOG_VARIANCE_CONTROL$GLM_EPSILON,
        maxit = LOG_VARIANCE_CONTROL$GLM_MAXIT
      )
    ))
  )
  expect_true(any(grepl("did not converge", captured$warnings)))

  # the same bad start still recovers through the wrapper's ladder, and the
  # muffled warning from its rejected first rung never escapes to the caller
  fit <- NULL
  expect_no_warning(fit <- fit_log_variance(d$y, d$x, start = c(100, 0, 0)))
  expect_true(log_variance_fit_ok(fit))
  expect_identical(
    fit$diagnostics$start_attempts[[1]]$error_class, "irls_not_converged"
  )
}

{
  d <- simulate_logvar_data()
  local_mocked_bindings(
    LOG_VARIANCE_CONTROL = utils::modifyList(
      LOG_VARIANCE_CONTROL, list(GLM_MAXIT = 1L)
    ),
    .package = "hetid"
  )
  fit <- NULL
  expect_no_warning(fit <- fit_log_variance(d$y, d$x))
  expect_false(log_variance_fit_ok(fit))
  expect_identical(fit$diagnostics$error_class, "irls_not_converged")
  expect_true(any(grepl("did not converge", fit$diagnostics$warnings)))
}
