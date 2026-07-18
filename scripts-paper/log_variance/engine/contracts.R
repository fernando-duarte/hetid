# Closed fit-result protocol and engine service phases.

LOGVAR_FIT_STATUS <- c(
  ok = "ok",
  nonconvergence = "nonconvergence",
  nonexistence = "nonexistence",
  domain_failure = "domain_failure",
  nonfinite_fitted_log_variance =
    "nonfinite_fitted_log_variance"
)

LOGVAR_ENGINE_PHASES <- c(
  scan = "scan",
  probe = "probe",
  refinement = "refinement",
  extra_start = "extra_start",
  polish = "polish",
  nonunique = "nonunique",
  cold_start = "cold_start",
  cache_hit = "cache_hit",
  claimed_domain_failure = "claimed_domain_failure"
)

LOGVAR_ENGINE_FIT_PHASES <- setdiff(
  LOGVAR_ENGINE_PHASES,
  LOGVAR_ENGINE_PHASES[["cache_hit"]]
)

new_logvar_fit_result <- function(
  coef,
  fit_status,
  converged,
  objective,
  score_norm,
  convergence_code,
  warm_start,
  diagnostics
) {
  stopifnot(
    is.character(fit_status),
    length(fit_status) == 1L,
    !is.na(fit_status),
    fit_status %in% LOGVAR_FIT_STATUS,
    is.logical(converged),
    length(converged) == 1L,
    !is.na(converged),
    is.list(diagnostics)
  )
  list(
    coef = coef,
    fit_status = fit_status,
    converged = converged,
    objective = objective,
    score_norm = score_norm,
    convergence_code = convergence_code,
    diagnostics = diagnostics,
    warm_start = warm_start
  )
}

logvar_fit_ok <- function(fit) {
  is.list(fit) &&
    identical(
      fit$fit_status,
      LOGVAR_FIT_STATUS[["ok"]]
    ) &&
    isTRUE(fit$converged) &&
    !is.null(fit$coef) &&
    all(is.finite(fit$coef))
}
