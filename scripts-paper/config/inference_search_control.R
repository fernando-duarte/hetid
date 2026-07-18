# Canonical inference search controls and log-variance compute budgets.

PAPER_INFERENCE_SEARCH_CONTROL <- list(
  im_root = list(
    bracket_lower = 0,
    bracket_upper = 10,
    tolerance = 1e-10
  ),
  stoye_root = list(
    bracket_lower = 1e-6,
    bracket_upper = 8,
    tolerance = 1e-8
  ),
  bvn = list(
    degenerate_correlation = 0.999,
    integration_relative_tolerance = 1e-9
  ),
  robust_endpoint_correlation = list(
    minimum_pairs = 8L,
    mad_trim_multiple = 6
  ),
  tau_star = list(
    fine_grid_points = 20L,
    bisection_iterations = 40L,
    bootstrap_bisection_iterations = 15L
  ),
  bootstrap = list(
    fatal_failure_share = 0.25,
    sensitivity_min_reps = 50L,
    sensitivity_reps_share = 0.25,
    progress_report_every = 25L
  ),
  logvar_endpoint = list(stability_share = 0.85)
)

PAPER_LOGVAR_BUDGETS <- local({
  primary <- list(
    grid_cap = 4000L,
    fit_budget = 20000L,
    sensitivity_fit_budget = 40000L
  )
  list(
    estimator = list(ppml = primary, harvey = primary),
    ppml_coverage = list(
      grid_cap = 8000L,
      fit_budget = 40000L
    ),
    bootstrap = list(
      grid_cap = 1500L,
      fit_budget = 8000L
    ),
    fitted_volatility = list(fit_budget = 80000L)
  )
})

paper_logvar_budget <- function(estimator, field) {
  budget <- PAPER_LOGVAR_BUDGETS$estimator[[estimator]]
  if (is.null(budget) || !field %in% names(budget)) {
    stop(
      sprintf("Unknown log-variance budget: %s/%s", estimator, field),
      call. = FALSE
    )
  }
  budget[[field]]
}

paper_bootstrap_failure_limit <- function(
  reps, control = PAPER_INFERENCE_SEARCH_CONTROL
) {
  floor(reps * control$bootstrap$fatal_failure_share)
}

paper_bootstrap_sensitivity_reps <- function(
  reps, control = PAPER_INFERENCE_SEARCH_CONTROL
) {
  max(
    control$bootstrap$sensitivity_min_reps,
    floor(reps * control$bootstrap$sensitivity_reps_share)
  )
}

stopifnot(
  PAPER_INFERENCE_SEARCH_CONTROL$im_root$bracket_lower <
    PAPER_INFERENCE_SEARCH_CONTROL$im_root$bracket_upper,
  PAPER_INFERENCE_SEARCH_CONTROL$stoye_root$bracket_lower <
    PAPER_INFERENCE_SEARCH_CONTROL$stoye_root$bracket_upper,
  PAPER_INFERENCE_SEARCH_CONTROL$im_root$tolerance > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$stoye_root$tolerance > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$bvn$degenerate_correlation > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$bvn$degenerate_correlation < 1,
  PAPER_INFERENCE_SEARCH_CONTROL$bvn$integration_relative_tolerance > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$robust_endpoint_correlation$minimum_pairs >= 2L,
  PAPER_INFERENCE_SEARCH_CONTROL$robust_endpoint_correlation$mad_trim_multiple > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$tau_star$fine_grid_points >= 1L,
  PAPER_INFERENCE_SEARCH_CONTROL$tau_star$bisection_iterations >= 1L,
  PAPER_INFERENCE_SEARCH_CONTROL$tau_star$bootstrap_bisection_iterations >= 1L,
  PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$fatal_failure_share > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$fatal_failure_share < 1,
  PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$sensitivity_min_reps >= 1L,
  PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$sensitivity_reps_share > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$sensitivity_reps_share <= 1,
  PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$progress_report_every >= 1L,
  PAPER_INFERENCE_SEARCH_CONTROL$logvar_endpoint$stability_share > 0,
  PAPER_INFERENCE_SEARCH_CONTROL$logvar_endpoint$stability_share < 1,
  identical(
    PAPER_LOGVAR_BUDGETS$estimator$ppml,
    PAPER_LOGVAR_BUDGETS$estimator$harvey
  ),
  all(unlist(PAPER_LOGVAR_BUDGETS$estimator$ppml) > 0),
  all(unlist(PAPER_LOGVAR_BUDGETS$ppml_coverage) > 0),
  all(unlist(PAPER_LOGVAR_BUDGETS$bootstrap) > 0),
  all(unlist(PAPER_LOGVAR_BUDGETS$fitted_volatility) > 0)
)
