# Post-gate body of the estimator-generic set engine: precheck, feasible
# grid, selector or coarsening, batch or generic scan with budgets, and
# the hand-off to the endpoint stage. Split from api.R for
# the repository line cap; the budget condition wraps this body there.
# Definitions only; sourced by api.R.

# the post-gate engine body; split out so the budget condition wraps it
logvar_engine_run <- function(est, qs, b_tab, b_seed, grid_n, grid_floor,
                              extra_starts, max_grid_points, cold_start_check,
                              tau, meta, bs, cache, evaluate_fit, ctx, st,
                              omega, fail_closed, diag_of,
                              starts_per_side = 1L, grid_selector = NULL) {
  ad <- est$analyze_domain
  pre <- NULL
  if (!is.null(ad$precheck)) {
    pre <- logvar_call_precheck(ad$precheck, qs, b_tab, ctx)
    ctx$precheck <- pre
    if (length(pre$unresolved) > 0L) {
      return(fail_closed(PAPER_ENDPOINT_STATUS[["unreliable"]], pre$n_flagged, NA_integer_))
    }
  }
  pre_cross <- if (is.null(pre)) NA_integer_ else pre$n_flagged
  b_feas <- logvar_feasible_grid(qs, b_tab$set_lower, b_tab$set_upper, grid_n)
  if (nrow(b_feas) < grid_floor) {
    b_feas <- logvar_feasible_grid(
      qs, b_tab$set_lower, b_tab$set_upper, 2L * grid_n - 1L
    )
  }
  if (nrow(b_feas) == 0L) {
    return(fail_closed(PAPER_ENDPOINT_STATUS[["unreliable"]], pre_cross, 0L))
  }
  sel_info <- NULL
  sel_traversal <- NULL
  if (!is.null(grid_selector)) {
    sel <- logvar_engine_apply_selector(grid_selector, b_feas, max_grid_points)
    sel_info <- sel$info
    sel_traversal <- sel$traversal
    b_feas <- sel$grid
  } else {
    b_feas <- logvar_coarsen_grid(b_feas, max_grid_points)
  }
  use_fast <- !is.null(est$scan_grid)
  if (!use_fast && !identical(meta$traversal, "lattice") &&
    !identical(sel_traversal, "as_selected") &&
    is.null(max_grid_points) &&
    nrow(b_feas) > LOGVAR_SEARCH_CONTROL$nearest_neighbor_limit) {
    stop(
      "generic nearest-neighbor scan over ", nrow(b_feas), " points; set ",
      "max_grid_points (deterministic coarsening) or metadata$traversal ",
      "= \"lattice\""
    )
  }
  if (!is.null(b_seed) && !anyNA(b_seed)) {
    if (.feasibility_residual(qs, b_seed, rep(1, length(qs$A_i))) <= 0) {
      b_feas <- rbind(b_feas, b_seed)
    }
  }
  st$n_feasible <- nrow(b_feas)
  claim_fn <- NULL
  if (!is.null(ad$claim_failure)) {
    claim_fn <- function(b, fit) {
      cl <- ad$claim_failure(b, fit, ctx$precheck, ctx)
      if (isTRUE(cl$claimed)) {
        bs$counters[[LOGVAR_ENGINE_PHASES[["claimed_domain_failure"]]]] <-
          bs$counters[[LOGVAR_ENGINE_PHASES[["claimed_domain_failure"]]]] + 1L
      }
      cl
    }
  }
  scan <- if (use_fast) {
    n_pts <- nrow(b_feas)
    if (bs$n_evaluated + n_pts > bs$max_fit_evals) {
      logvar_budget_stop(LOGVAR_ENGINE_PHASES[["scan"]], sprintf(
        "batch scan of %d points exceeds max_fit_evals = %s",
        n_pts, format(bs$max_fit_evals)
      ))
    }
    # absent key means uncapped, matching logvar_make_evaluator ([[ ]] on a
    # missing name would abort on a partial caps vector)
    scan_phase <- LOGVAR_ENGINE_PHASES[["scan"]]
    scan_cap <- if (scan_phase %in% names(bs$phase_caps)) {
      bs$phase_caps[[scan_phase]]
    } else {
      NULL
    }
    if (!is.null(scan_cap) && bs$counters[[scan_phase]] + n_pts > scan_cap) {
      logvar_budget_stop(
        scan_phase,
        sprintf("phase cap %d reached", scan_cap)
      )
    }
    bs$counters[[scan_phase]] <- bs$counters[[scan_phase]] + n_pts
    bs$n_attempted <- bs$n_attempted + n_pts
    bs$n_evaluated <- bs$n_evaluated + n_pts
    est$scan_grid(b_feas)
  } else {
    logvar_engine_scan(
      est, b_feas, evaluate_fit, b_seed, claim_fn, st,
      traversal_override = if (identical(sel_traversal, "as_selected")) {
        "lattice"
      } else {
        NULL
      },
      pool_k = starts_per_side,
      pool_sep = LOGVAR_SEARCH_CONTROL$start_separation_fraction *
        sqrt(sum((b_tab$set_upper - b_tab$set_lower)^2))
    )
  }
  st$n_fail <- if (is.null(scan$n_fit_failures)) 0L else scan$n_fit_failures
  # unclaimed = total failures net of estimator-claimed ones, on either scan
  # path -- a batch scan cannot claim, so its failures always fail closed
  n_unclaimed <- st$n_fail - length(scan$claimed_failures)
  if (n_unclaimed > 0L) {
    return(fail_closed(
      PAPER_ENDPOINT_STATUS[["unreliable"]], pre_cross, st$n_feasible, NULL,
      list(unclaimed_fit_failures = n_unclaimed)
    ))
  }
  if (is.null(scan$min) || is.null(st$labels)) {
    return(fail_closed(
      PAPER_ENDPOINT_STATUS[["unreliable"]], pre_cross, st$n_feasible, NULL,
      list(no_successful_fits = TRUE)
    ))
  }
  diag_sel <- if (is.null(sel_info)) {
    diag_of
  } else {
    function(extra = list()) diag_of(c(list(selector = sel_info), extra))
  }
  logvar_engine_endpoints(
    est, qs, b_tab, b_seed, extra_starts, cold_start_check, tau, meta, bs,
    evaluate_fit, ctx, st, omega, scan, pre_cross, diag_sel
  )
}
