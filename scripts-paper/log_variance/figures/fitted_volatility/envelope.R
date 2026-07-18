# Date-indexed fitted-volatility envelope over one joint mean-equation
# identified set. The engine profiles fitted log variance directly at every
# date; monotonic transformations then give conditional variance and volatility.

logvar_fitted_vol_transform <- function(eta, power) {
  out <- exp(power * eta)
  out[is.na(eta)] <- NA_real_
  out[is.finite(eta) & !is.finite(out)] <- NA_real_
  out
}

logvar_fitted_vol_point <- function(adapter, b_point, n) {
  if (is.null(b_point) || anyNA(b_point)) {
    return(rep(NA_real_, n))
  }
  fit <- adapter$fit_at_b(b_point, phase = LOGVAR_ENGINE_PHASES[["extra_start"]])
  if (!logvar_fit_ok(fit)) {
    return(rep(NA_real_, n))
  }
  unname(fit$coef)
}

logvar_fitted_vol_data <- function(qtr, schema, point_eta) {
  n <- length(qtr)
  stopifnot(nrow(schema) == n, length(point_eta) == n)
  lower_status <- schema$lower_status
  upper_status <- schema$upper_status
  lower <- schema$lower
  upper <- schema$upper
  variance_lower <- logvar_fitted_vol_transform(lower, 1)
  variance_upper <- logvar_fitted_vol_transform(upper, 1)
  volatility_lower <- logvar_fitted_vol_transform(lower, 0.5)
  volatility_upper <- logvar_fitted_vol_transform(upper, 0.5)
  lower_bad <- lower_status == "bounded" &
    (is.na(variance_lower) | is.na(volatility_lower))
  upper_bad <- upper_status == "bounded" &
    (is.na(variance_upper) | is.na(volatility_upper))
  lower_status[lower_bad] <- "unreliable"
  upper_status[upper_bad] <- "unreliable"
  data.frame(
    qtr = qtr, date = as.Date(qtr),
    log_variance_lower = lower, log_variance_upper = upper,
    variance_lower = variance_lower, variance_upper = variance_upper,
    volatility_lower = volatility_lower,
    volatility_upper = volatility_upper,
    log_variance_point = point_eta,
    variance_point = logvar_fitted_vol_transform(point_eta, 1),
    volatility_point = logvar_fitted_vol_transform(point_eta, 0.5),
    lower_status = lower_status, upper_status = upper_status,
    lower_provenance = schema$lower_provenance,
    upper_provenance = schema$upper_provenance,
    row.names = NULL
  )
}

logvar_fitted_vol_envelope <- function(
  est, qtr, x_mat, qs, b_tab,
  b_seed, b_point, tau,
  source_cache = NULL,
  expected_sample_id = NULL,
  max_grid_points = NULL,
  max_fit_evals = Inf,
  starts_per_side =
    LOGVAR_SEARCH_CONTROL$fitted_vol_starts_per_side
) {
  stopifnot(
    length(qtr) == nrow(x_mat), !anyNA(qtr), !anyDuplicated(qtr),
    identical(order(qtr), seq_along(qtr)), is.finite(tau), tau >= 0,
    length(starts_per_side) == 1L, starts_per_side >= 1L
  )
  labels <- sprintf("date_%04d", seq_along(qtr))
  adapter <- logvar_fitted_vol_adapter(
    est, x_mat, labels,
    source_cache = source_cache,
    expected_sample_id = expected_sample_id
  )
  cache <- new.env(parent = emptyenv())
  budget <- logvar_budget_state(max_fit_evals)
  result <- logvar_engine_set_at_tau(
    adapter, qs, b_tab,
    b_seed = b_seed,
    max_grid_points = max_grid_points, max_fit_evals = max_fit_evals,
    starts_per_side = starts_per_side, cache = cache, budget_state = budget,
    cold_start_check = LOGVAR_SEARCH_CONTROL$cold_start_check,
    tau = tau
  )
  stopifnot(identical(result$schema$coef, labels))
  point_eta <- logvar_fitted_vol_point(adapter, b_point, length(qtr))
  data <- logvar_fitted_vol_data(qtr, result$schema, point_eta)
  two_sided <- data$lower_status == "bounded" &
    data$upper_status == "bounded"
  has_point <- is.finite(data$volatility_point)
  tol <- LOGVAR_SEARCH_CONTROL$point_containment_rtol *
    pmax(1, abs(data$volatility_point))
  stopifnot(all(
    data$volatility_point[two_sided & has_point] >=
      data$volatility_lower[two_sided & has_point] - tol[two_sided & has_point] &
      data$volatility_point[two_sided & has_point] <=
        data$volatility_upper[two_sided & has_point] + tol[two_sided & has_point]
  ))
  source_counts <- adapter$source_budget_state
  if (!is.null(source_counts)) {
    source_counts <- list(
      n_attempted = source_counts$n_attempted,
      n_evaluated = source_counts$n_evaluated,
      n_cached = source_counts$n_cached,
      n_failed = source_counts$n_failed,
      counters = source_counts$counters
    )
  }
  list(
    metadata = list(
      estimator = est$metadata$estimator,
      sample_id = est$metadata$sample_id, tau = tau,
      estimand = paste(
        "fitted conditional standard deviation of the consumption-growth",
        "structural residual"
      ),
      envelope = "pointwise fitted-log-variance projection"
    ),
    data = data, schema = result$schema, domain_info = result$domain_info,
    diagnostics = list(engine = result$diagnostics, source = source_counts)
  )
}
