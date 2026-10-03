fitted_volatility_level <- function(eta, power) {
  out <- exp(power * eta)
  out[is.na(eta) | (is.finite(eta) & !is.finite(out))] <- NA_real_
  out
}

fitted_volatility_metadata <- function(sets, adapter) {
  list(
    estimator = sets$key, source_target_functional = sets$estimator$metadata$target_functional,
    target_functional = adapter$metadata$target_functional,
    sample_id = adapter$metadata$sample_id, spec_id = adapter$metadata$spec_id,
    source_spec_id = sets$estimator$metadata$spec_id,
    predictor_date = sets$sample$date, response_date = sets$sample$response_date,
    include_intercept = FALSE
  )
}

fitted_volatility_closures <- function(sets, results = list()) {
  diagnostics <- list()
  closures <- if (length(results)) lv_set_path_closures(results) else list()
  display <- lv_set_path_closures(sets$results)
  if (length(closures)) diagnostics$pre_grid_closures <- closures
  if (length(display)) diagnostics$display_pre_grid_closures <- display
  if (length(diagnostics)) {
    axes <- list()
    if (length(closures)) {
      axes$pre_grid_closures <- list(
        target_functional = "fitted_log_variance_path",
        coef_labels = sprintf("date_%04d", seq_along(sets$sample$response_date)),
        response_date = sets$sample$response_date
      )
    }
    if (length(display)) {
      axes$display_pre_grid_closures <- list(
        target_functional = sets$estimator$metadata$target_functional,
        coef_labels = sets$estimator$coef_labels
      )
    }
    diagnostics$closure_axes <- axes
  }
  diagnostics
}

fitted_volatility_result <- function(sets, adapter, result, tau, point_eta,
                                     point_status, control) {
  schema <- result$schema
  assert_bad_argument_ok(
    identical(schema$coef, adapter$coef_labels), "search changed the dated target axis", "result"
  )
  lower_status <- schema$lower_status
  upper_status <- schema$upper_status
  lower_status[lower_status == "bounded" &
    is.na(fitted_volatility_level(schema$lower, 1))] <- "unreliable"
  upper_status[upper_status == "bounded" &
    is.na(fitted_volatility_level(schema$upper, 1))] <- "unreliable"
  rows <- data.frame(
    date = sets$sample$response_date, tau = tau,
    log_variance_lower = schema$lower, log_variance_upper = schema$upper,
    log_variance_point = point_eta,
    variance_lower = fitted_volatility_level(schema$lower, 1),
    variance_upper = fitted_volatility_level(schema$upper, 1),
    variance_point = fitted_volatility_level(point_eta, 1),
    volatility_lower = fitted_volatility_level(schema$lower, 0.5),
    volatility_upper = fitted_volatility_level(schema$upper, 0.5),
    volatility_point = fitted_volatility_level(point_eta, 0.5),
    lower_status = lower_status, upper_status = upper_status,
    lower_source = schema$lower_source, upper_source = schema$upper_source,
    row.names = NULL, stringsAsFactors = FALSE
  )
  inside <- lower_status == "bounded" & upper_status == "bounded" & is.finite(point_eta)
  point_transform_failed <- inside & is.na(rows$volatility_point)
  slack <- control$search$point_containment_rtol * pmax(1, abs(rows$volatility_point))
  contained <- rows$volatility_point >= rows$volatility_lower - slack &
    rows$volatility_point <= rows$volatility_upper + slack
  contained[point_transform_failed] <- FALSE
  if (any(!contained[inside])) {
    stop(new_hetid_error(
      "the tau-zero fit lies outside its fitted volatility band",
      subclass = "hetid_error_numerical", tau = tau,
      date = rows$date[inside & !contained]
    ))
  }
  source_budget <- adapter$source_budget
  fields <- c("max_fit_evals", "n_attempted", "n_evaluated", "n_cached", "n_failed", "counters")
  diagnostics <- c(
    list(
      engine = result$diagnostics,
      source = stats::setNames(lapply(fields, function(field) source_budget[[field]]), fields)
    ),
    fitted_volatility_closures(sets)
  )
  list(
    tau = tau, estimator = sets$key, metadata = fitted_volatility_metadata(sets, adapter),
    data = rows, schema = schema, point_status = point_status, diagnostics = diagnostics
  )
}
