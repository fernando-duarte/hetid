lv_set_search_control <- function() {
  list(
    grid_n = 41L,
    grid_floor = 100L,
    primary_starts_per_side = 3L,
    audit_starts_per_side = 5L,
    envelope_starts_per_side = 1L,
    cold_start_check = TRUE,
    cold_start_rtol_fallback = 1e-8,
    endpoint_agreement_rtol = 1e-4,
    nearest_neighbor_limit = 5000L,
    start_separation_fraction = 0.05,
    nesting_rtol = 1e-6,
    point_containment_rtol = 1e-6,
    polish_blow_factor = 5,
    box_escape_rtol = 1e-4,
    primary_grid_cap = 4000L,
    primary_fit_budget = 20000L,
    sensitivity_fit_budget = 40000L,
    coverage_grid_cap = 8000L,
    coverage_fit_budget = 40000L,
    envelope_fit_budget = 80000L
  )
}

#' Controls for the Log-Variance Set Search
#'
#' @return Separate sets and search lists. Values preserve the donor grid,
#'   fit-budget, start-count, cold-fit, agreement and nesting schedules.
#'   sets$grid_points_limit caps each raw lattice at 2,000,000 rows before
#'   allocation; a smaller method-specific cap still applies.
#' @examples
#' control <- log_variance_search_control()
#' control$search$grid_n <- 7L
#' control$search$primary_fit_budget <- 100L
#' control$search[c("grid_n", "primary_fit_budget")]
#' @export
log_variance_search_control <- function() {
  list(sets = lv_set_solver_control(), search = lv_set_search_control())
}

lv_set_fit_result <- function(coef, fit_status, converged, objective = NA_real_,
                              score_norm = NA_real_, convergence_code = -1L,
                              warm_start = NULL, diagnostics = list()) {
  statuses <- c(
    "ok", "nonconvergence", "nonexistence", "domain_failure",
    "nonfinite_fitted_log_variance"
  )
  lv_set_assert(
    is.character(fit_status), length(fit_status) == 1L, fit_status %in% statuses,
    is.logical(converged), length(converged) == 1L, !is.na(converged), is.list(diagnostics)
  )
  list(
    coef = coef, fit_status = fit_status, converged = converged, objective = objective,
    score_norm = score_norm, convergence_code = convergence_code, diagnostics = diagnostics,
    warm_start = warm_start
  )
}

lv_set_spec_id <- function(fields) {
  leaves <- rapply(fields, function(value) {
    paste(if (is.numeric(value)) sprintf("%.17g", value) else as.character(value),
      collapse = ","
    )
  }, how = "unlist")
  lv_set_assert(
    !is.null(names(leaves)), all(nzchar(names(leaves))),
    !anyDuplicated(names(leaves))
  )
  paste(sort(paste0(names(leaves), "=", leaves), method = "radix"), collapse = "\n")
}

lv_set_budget <- function(max_fit_evals = Inf) {
  budget <- new.env(parent = emptyenv())
  budget$max_fit_evals <- max_fit_evals
  budget$counters <- c(
    scan = 0L, extra_start = 0L, polish = 0L, cold_start = 0L,
    cache_hit = 0L
  )
  budget$n_attempted <- 0L
  budget$n_evaluated <- 0L
  budget$n_cached <- 0L
  budget$n_failed <- 0L
  budget
}

lv_set_budget_stop <- function(phase, reason) {
  stop(new_hetid_error(
    sprintf("fit budget exhausted (%s): %s", phase, reason),
    subclass = "hetid_error_log_variance_budget", phase = phase
  ))
}

lv_set_cache_bind <- function(cache, metadata) {
  if (is.null(cache$estimator)) {
    cache$estimator <- metadata$estimator
    cache$sample_id <- metadata$sample_id
    cache$spec_id <- metadata$spec_id
    cache$store <- new.env(parent = emptyenv())
  }
  lv_set_assert(
    identical(cache$estimator, metadata$estimator),
    identical(cache$sample_id, metadata$sample_id),
    identical(cache$spec_id, metadata$spec_id)
  )
  cache
}

lv_set_b_key <- function(b) paste(sprintf("%.17g", unname(b)), collapse = "|")

lv_set_evaluator <- function(estimator, cache, budget) {
  function(b, phase, start = NULL, use_cache = TRUE) {
    lv_set_assert(phase %in% c("scan", "extra_start", "polish", "cold_start"))
    key <- lv_set_b_key(b)
    if (use_cache && !is.null(cache$store[[key]])) {
      budget$counters[["cache_hit"]] <- budget$counters[["cache_hit"]] + 1L
      budget$n_attempted <- budget$n_attempted + 1L
      budget$n_cached <- budget$n_cached + 1L
      return(lv_set_checked_fit(cache$store[[key]], estimator$coef_labels))
    }
    if (budget$n_evaluated >= budget$max_fit_evals) {
      lv_set_budget_stop(phase, sprintf(
        "max_fit_evals = %s reached",
        format(budget$max_fit_evals)
      ))
    }
    budget$counters[[phase]] <- budget$counters[[phase]] + 1L
    budget$n_attempted <- budget$n_attempted + 1L
    budget$n_evaluated <- budget$n_evaluated + 1L
    fit <- lv_set_checked_fit(
      estimator$fit_at_b(b, start = start, phase = phase),
      estimator$coef_labels
    )
    if (!log_variance_fit_ok(fit)) budget$n_failed <- budget$n_failed + 1L
    if (use_cache) cache$store[[key]] <- fit
    fit
  }
}

lv_set_check_nesting <- function(rows, tolerance) {
  violations <- data.frame(
    coef = character(0), side = character(0), tau = numeric(0),
    violation = numeric(0)
  )
  for (coef in unique(rows$coef)) {
    for (side in c("lower", "upper")) {
      rows_subset <- rows[rows$coef == coef & rows[[paste0(side, "_status")]] == "bounded", ]
      rows_subset <- rows_subset[order(rows_subset$tau), ]
      if (nrow(rows_subset) < 2L) next
      values <- rows_subset[[side]]
      for (k in seq_len(nrow(rows_subset) - 1L)) {
        gap <- if (side == "lower") values[k + 1L] - values[k] else values[k] - values[k + 1L]
        if (gap > tolerance * max(1, abs(values[k]))) {
          violations <- rbind(violations, data.frame(
            coef = coef, side = side,
            tau = rows_subset$tau[k + 1L], violation = gap
          ))
        }
      }
    }
  }
  violations
}

lv_set_bounded_args <- function(schema) {
  arguments <- c(
    schema$arg_lower[schema$lower_status == "bounded"],
    schema$arg_upper[schema$upper_status == "bounded"]
  )
  arguments[!vapply(arguments, anyNA, logical(1))]
}
