# Map one regularized log projection over display taus: a primary full-lattice
# batch scan, an independent audit on the same lattice with separated
# five-start pools over a fresh cache, Harvey's union-with-demotion
# reconciliation, and a nesting check on the reconciled display sets that
# demotes, never moves, a violating side. Definitions only; sourced by
# run_sets.R.

# ctx carries the shared map context: prep, sample_id, b_ref, b_point,
# point_feasible, search_seed, qs_fn, bounds_tau
logvar_log_projection_sets <- function(id, multiplier, taus, ctx) {
  control <- LOGVAR_LOG_PROJECTION_CONTROL
  est <- logvar_log_projection_estimator(
    ctx$prep, id, id, multiplier, ctx$sample_id, control
  )
  ref_fit <- est$fit_at_b(ctx$b_ref)
  if (!logvar_fit_ok(ref_fit)) {
    stop(sprintf("%s: reference fit failed (%s)", id, ref_fit$fit_status))
  }
  point <- stats::setNames(rep(NA_real_, length(est$coef_labels)), est$coef_labels)
  if (isTRUE(ctx$point_feasible)) {
    point_fit <- est$fit_at_b(ctx$b_point)
    if (logvar_fit_ok(point_fit)) {
      # set before any registry entry copies the estimator: the residual
      # diagnostics read entry$estimator$point_fit
      est$point_fit <- point_fit
      point <- point_fit$coef
    } else {
      cat(sprintf("  %s Lewbel point unavailable: %s\n", id, point_fit$fit_status))
    }
  }
  cache <- new.env(parent = emptyenv())
  mapped <- logvar_map_display_taus(
    taus = taus,
    bounds_tau = ctx$bounds_tau,
    quadratic_at_tau = ctx$qs_fn,
    map_one = logvar_engine_tau_mapper(
      estimator = est,
      b_seed = ctx$search_seed,
      max_grid_points = NULL,
      max_fit_evals = Inf,
      cache = cache,
      cold_start_check = FALSE
    )
  )
  est_audit <- logvar_log_projection_estimator(
    ctx$prep, id, id, multiplier, ctx$sample_id, control,
    pool_k = LOGVAR_SEARCH_CONTROL$audit_starts_per_side
  )
  audit <- logvar_audit_display_taus(
    estimator = est_audit,
    taus = taus,
    boxes = mapped$boxes,
    seed = ctx$search_seed,
    grid_cap = NULL,
    fit_budget = Inf,
    quadratic_at_tau = ctx$qs_fn,
    cold_start_check = FALSE
  )
  reconciled <- logvar_ppml_apply_coverage(
    mapped$results, audit,
    cache_stamp = est$metadata$spec_id
  )
  nested <- logvar_log_projection_nesting(reconciled$results)
  list(
    estimator = est, multiplier = multiplier, reference = ref_fit$coef,
    point = point, primary = mapped$results, final = nested$results,
    audit = reconciled$audit, audit_metadata = reconciled$metadata,
    nesting = nested$violations, boxes = mapped$boxes, cache = cache
  )
}

# Demote every display-set side whose bound loosens as tau grows (the exact
# image sets are nested); the value is kept and the side reported unreliable
logvar_log_projection_nesting <- function(results) {
  cols <- c("coef", "tau", "lower", "upper", "lower_status", "upper_status")
  rows <- do.call(rbind, lapply(results, function(r) r$schema[cols]))
  violations <- logvar_check_nesting(rows)
  for (k in seq_len(nrow(violations))) {
    v <- violations[k, ]
    key <- names(results)[vapply(results, function(r) {
      isTRUE(all.equal(r$schema$tau[[1L]], v$tau))
    }, logical(1))]
    sch <- results[[key]]$schema
    j <- match(v$coef, sch$coef)
    sch[[paste0(v$side, "_status")]][j] <- PAPER_ENDPOINT_STATUS[["unreliable"]]
    results[[key]]$schema <- sch
    results[[key]]$table$status <- paper_endpoint_status_reduce(
      sch$lower_status, sch$upper_status
    )
  }
  list(results = results, violations = violations)
}
