# Per-tau context for the fitted-volatility slack sweep. The baseline figure
# (run.R) reads one warm-refined news box straight off mean_eq_bounds_tau; a
# swept tau may sit off the display grid, so its box has to be solved here from
# the same warm chain the published boxes use.

# Warm-refined theta boxes at every swept tau, keyed by paper_tau_key. The chain
# is seeded at the tau = 0 Lewbel point and walked up through the union of the
# display and sweep taus, so a swept tau inherits a still-feasible warm start
# from the next smaller one exactly as set_tables does.
logvar_tau_sweep_boxes <- function(mean_eq, solve_fn, taus) {
  set_id_display_tau_refinement(
    sort(unique(c(mean_eq$tau_display, taus))),
    mean_eq$theta_table$point,
    solve_fn,
    mean_eq$gamma, mean_eq$moments, mean_eq$beta1r, mean_eq$beta2r
  )
}

# Slacks the sweep can actually render. The identified set exists only below
# tau*, so a contract tau at or above the estimated transition has no bounded
# set to project and is dropped with a console note rather than hard-failing the
# stage on a data-dependent boundary.
logvar_tau_sweep_feasible <- function(taus, tau_star) {
  keep <- taus < tau_star
  if (any(!keep)) {
    cat(sprintf(
      "  fitted-volatility sweep: dropping tau %s (>= tau* = %s)\n",
      paste(format(taus[!keep]), collapse = ", "),
      signif(tau_star, PAPER_REPORTING_CONTROL$precision$console_significant)
    ))
  }
  taus[keep]
}

# Everything one swept tau needs downstream: its quadratic system and the
# tau = 0 Lewbel point, kept only when that point is feasible in this set (the
# red reference curve is otherwise undrawable and renders as no line at all).
logvar_tau_sweep_context <- function(mean_eq, boxes, tau) {
  b_tab <- boxes[[paper_tau_key(tau)]]
  stopifnot(!is.null(b_tab))
  qs <- tau_quadratic_system(mean_eq$gamma, tau, mean_eq$moments)
  b_point <- mean_eq$theta_table$point
  list(
    b_tab = b_tab,
    qs = qs,
    b_point = if (quadratic_point_feasible(qs, b_point)) b_point else NULL
  )
}

# Registry lookup by estimator id, matching the baseline driver's rule that
# exactly one entry owns each estimator.
logvar_tau_sweep_entry <- function(registry, estimator) {
  hit <- vapply(registry, function(entry) {
    identical(entry$estimator$metadata$estimator, estimator)
  }, logical(1))
  stopifnot(sum(hit) == 1L)
  registry[[which(hit)]]
}

# Manifest variant id for one rendered panel. Going through the manifest rather
# than rewriting the baseline figure's filename means a swept tau with no
# artifact record fails at artifact_variant_path instead of quietly writing an
# unmanifested file.
logvar_tau_sweep_variant <- function(estimator, tau = NULL, suffix = NULL) {
  tail <- if (is.null(suffix)) {
    sprintf("tau%s", sub(".", "p", format(tau), fixed = TRUE))
  } else {
    suffix
  }
  paste(estimator, tail, sep = "_")
}

logvar_tau_sweep_path <- function(estimator, tau = NULL, suffix = NULL) {
  artifact_variant_path(
    "fitted_volatility_sweep",
    logvar_tau_sweep_variant(estimator, tau, suffix)
  )
}
