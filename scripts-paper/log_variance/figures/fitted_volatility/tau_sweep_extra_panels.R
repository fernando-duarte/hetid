# The two panels that sit beside the contract sweep: the OLS-benchmark overlay
# and the pair of slacks nearest tau*. Both reuse the combined renderer and both
# are drawn for one estimator, so neither belongs in the sweep's own loop.
# Split from run_tau_sweep.R for the repository line cap. Definitions only.

paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "tau_sweep_context.R"
))

# The log-variance path the two-step log-OLS map returns at the mean equation's
# OLS news coefficients, rather than anywhere in the identified set: theta_hat
# is the OLS coefficient vector of log(eps_hat^2) on (1, PC_R) with eps_hat =
# w1 - W2 b_ols. x_mat carries a zeroed intercept column, so the value is the
# same PC_R' theta_R the bands bound and the two are directly comparable.
logvar_tau_sweep_ols_line <- function(mean_eq, inputs, x_mat) {
  b_ols <- mean_eq$theta_table$ols
  stopifnot(!anyNA(b_ols), length(b_ols) == ncol(inputs$w2))
  theta <- logvar_theta_hat(
    b_ols, inputs$w1, inputs$w2, logvar_projection(inputs$pcr)
  )
  stopifnot(
    identical(names(theta), colnames(x_mat)), all(is.finite(theta))
  )
  data.frame(
    date = as.Date(inputs$qtr),
    value = drop(x_mat %*% theta),
    row.names = NULL
  )
}

# Envelopes at slacks outside the contract sweep, for the high-slack panel. The
# warm chain is solved over the display taus unioned with these, so a slack far
# above the swept range still inherits a feasible start.
logvar_tau_sweep_extra_envelopes <- function(mean_eq, registry, inputs, x_mat,
                                             sample_id, taus, fit_budget,
                                             estimator = "ppml") {
  boxes <- logvar_tau_sweep_boxes(mean_eq, taus)
  entry <- logvar_tau_sweep_entry(registry, estimator)
  envs <- list()
  for (tau in taus) {
    ctx <- logvar_tau_sweep_context(mean_eq, boxes, tau)
    envs[[paper_tau_key(tau)]] <- logvar_fitted_vol_envelope(
      entry$estimator, inputs$qtr, x_mat, ctx$qs, ctx$b_tab,
      b_seed = entry$b_seed, b_point = ctx$b_point, tau = tau,
      source_cache = entry$engine_opts$cache,
      expected_sample_id = sample_id,
      max_grid_points = entry$engine_opts$max_grid_points,
      max_fit_evals = fit_budget,
      starts_per_side = LOGVAR_SEARCH_CONTROL$fitted_vol_starts_per_side
    )
    cat(sprintf(
      "  extra-slack envelope (%s, tau = %s): %d of %d two-sided\n",
      estimator, format(tau),
      sum(
        envs[[paper_tau_key(tau)]]$data$lower_status ==
          PAPER_ENDPOINT_STATUS[["bounded"]] &
          envs[[paper_tau_key(tau)]]$data$upper_status ==
            PAPER_ENDPOINT_STATUS[["bounded"]]
      ),
      length(inputs$qtr)
    ))
  }
  envs
}
