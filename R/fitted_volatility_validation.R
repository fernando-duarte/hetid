fitted_volatility_validate <- function(sets, control) {
  lv_set_validate_control(control)
  lv_set_validate_aggregate(sets)
  lv_set_validate_sample(sets$sample)
  fitted_volatility_validate_source(sets)
  fitted_volatility_validate_design(sets$sample, sets$estimator)
  fitted_volatility_validate_dates(sets$sample$response_date, nrow(sets$sample$x_mat))
  metadata <- sets$estimator$metadata
  lv_set_validate_cache(sets$cache)
  if (!is.null(sets$cache$estimator)) {
    assert_bad_argument_ok(
      identical(sets$cache$estimator, metadata$estimator) &&
        identical(sets$cache$sample_id, metadata$sample_id) &&
        identical(sets$cache$spec_id, metadata$spec_id),
      "source cache identity must agree with the estimator", "sets"
    )
  }
  if (!is.null(sets$seed)) lv_set_axis(sets$seed, sets$estimator$theta_labels, "seed")
  if (!is.null(sets$grid_cap)) lv_set_positive(sets$grid_cap, "grid_cap", TRUE)
  lv_set_path_closures(sets$results)
  invisible(TRUE)
}

fitted_volatility_validate_source <- function(sets) {
  estimator <- sets$estimator
  metadata <- estimator$metadata
  assert_bad_argument_ok(
    sets$key %in% c("ppml", "harvey") && identical(sets$key, metadata$estimator) &&
      identical(metadata$response_scale, "variance"),
    "sets must contain a PPML or Harvey variance map", "sets"
  )
  assert_bad_argument_ok(
    identical(metadata$sample_id, sets$sample$sample_id),
    "estimator and prepared sample identities must agree", "sets"
  )
  assert_bad_argument_ok(
    is.function(estimator$fit_at_b) && is.function(estimator$jacobian_at_b) &&
      is.null(estimator$sides),
    "fitted volatility requires source fit and Jacobian hooks without sides", "sets"
  )
  invisible(TRUE)
}

fitted_volatility_validate_design <- function(sample_data, estimator) {
  design <- sample_data$x_mat
  assert_bad_argument_ok(
    is.matrix(design) && is.numeric(design) && all(is.finite(design)) &&
      identical(colnames(design), estimator$coef_labels) &&
      nrow(design) == length(sample_data$w1) &&
      identical(estimator$theta_labels, colnames(sample_data$w2)),
    "design and source estimator axes must agree", "sets"
  )
  intercept <- match(HETID_CONSTANTS$INTERCEPT_LABEL, colnames(design))
  assert_bad_argument_ok(
    !is.na(intercept) && all(design[, intercept] == 1),
    "prepared design must retain its intercept column of ones", "sets"
  )
  invisible(TRUE)
}

fitted_volatility_validate_dates <- function(dates, n_rows) {
  assert_bad_argument_ok(
    inherits(dates, "Date") && is.null(dim(dates)) && length(dates) == n_rows && !anyNA(dates),
    "response_date must contain nonmissing Dates on the design rows", "sets"
  )
  assert_bad_argument_ok(
    all(c(
      !anyDuplicated(dates), identical(order(dates), seq_along(dates)),
      identical(dates, to_period_end(dates, "monthly"))
    )),
    "response_date must contain unique sorted period-end Dates on the design rows", "sets"
  )
  invisible(TRUE)
}

fitted_volatility_validate_set <- function(sets, quadratic, theta_table, tau, point, control) {
  fitted_volatility_validate(sets, control)
  assert_bad_argument_ok(
    is.null(dim(tau)) && !is.complex(tau),
    "tau must be a real numeric scalar", "tau"
  )
  lv_set_positive(tau, "tau")
  lv_set_validate_search(
    sets$estimator, quadratic, theta_table, sets$seed, sets$grid_cap,
    control$search$ENVELOPE_FIT_BUDGET, control$search$ENVELOPE_STARTS_PER_SIDE, control
  )
  if (!is.null(point)) lv_set_axis(point, sets$estimator$theta_labels, "point")
  invisible(TRUE)
}

fitted_volatility_validate_path <- function(sets, fit, tau_star, taus, through, control) {
  fitted_volatility_validate(sets, control)
  validate_profile_taus(taus, "taus")
  validate_profile_taus(through, "through", allow_empty = TRUE)
  assert_bad_argument_ok(!anyDuplicated(taus), "taus must be distinct", "taus")
  assert_bad_argument_ok(
    is.null(dim(tau_star)) && !is.complex(tau_star),
    "tau_star must be a real numeric scalar", "tau_star"
  )
  lv_set_positive(tau_star, "tau_star", infinite = TRUE)
  if (any(taus >= tau_star)) {
    stop_bad_argument(sprintf(
      "tau %s is at or above the bounded cap %s",
      paste(sprintf("%.17g", sort(taus[taus >= tau_star])), collapse = ", "),
      sprintf("%.17g", tau_star)
    ), "taus")
  }
  validate_profile_fit(fit)
  assert_bad_argument_ok(
    identical(fit$w1, sets$sample$prep$w1_mean) &&
      identical(fit$w2, sets$sample$prep$w2_mean),
    "fit must use the complete prepared mean sample", "fit"
  )
  point <- sets$request$point
  assert_bad_argument_ok(!is.null(point), "path requires the original request point", "sets")
  lv_set_axis(point, colnames(fit$w2), "request point")
  tolerance <- attr(fit, "tol") * max(1, max(abs(fit$point$theta)))
  assert_bad_argument_ok(
    max(abs(point - fit$point$theta)) <= tolerance,
    "request point must agree with the current tau-zero point", "sets"
  )
  invisible(TRUE)
}
