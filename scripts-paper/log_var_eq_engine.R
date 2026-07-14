# Estimator-generic set engine for the log-variance equation: owns the
# feasible grid, deterministic traversal and warm starts, retries, candidate
# selection, endpoint polish, feasibility checks, cold-start replication,
# and side-specific status/provenance. Estimators supply the science.
#
# an estimator object is a plain list:
#   metadata: list(estimator, target_functional, intercept_normalization,
#     sample_id, smoothness ("smooth"/"nonsmooth"), inner_solver,
#     response_scale, spec_id, cold_start_rtol; optionally
#     response_scale_value and scale_reference when the estimator applies one
#     fixed pre-scan response scale -- spec_id folds in everything that
#     changes the fitted object (estimator version, package, seed policy,
#     fixed scale) and joins sample_id in the cache stamp; optionally
#     traversal ("nearest_neighbor" default, "lattice" bypasses the NN cap)
#   coef_labels: coefficient axis labels; optional on the generic path
#     (inferred from the first successful fit) but required by estimators
#     that supply a batch scan_grid, where no per-point fit runs
#   fit_at_b(b, start = NULL) -> list(coef, fit_status, converged, objective,
#     score_norm, convergence_code, diagnostics, warm_start)
#     fit_at_b owns its retry/start ladder; diagnostics is a slim list (no
#     raw package objects -- cache entries must stay small) with standardized
#     fields where applicable: warnings, messages, error_class,
#     start_attempts, plus estimator-specific entries
#   coef_objective(j) -> list(fn = function(b) scalar, gr = function(b)
#     K-vector or NULL)                          (optional override; default:
#     the engine builds fn/gr from the cached fit_at_b evaluator and
#     jacobian_at_b, so estimators whose objective needs an inner fit get
#     engine-wide memoization for free; log-OLS overrides with its exact
#     closed-form closures to preserve the benchmark arithmetic path)
#   jacobian_at_b(b, fit = NULL) -> n_coef x K matrix            (optional)
#   scan_grid(b_feas) -> list(min, max, arg_min, arg_max, domain_info,
#     n_fit_failures, fit_statuses, arg_min_pool, arg_max_pool)  (optional
#     fast path; the pools are optional per-coefficient lists of several
#     distinct extreme args for multi-start polish -- default is the single
#     benchmark arg, and the benchmark path passes exactly that)
#   analyze_domain: list of phase hooks                          (optional)
#     $precheck(qs, b_tab, ctx) -> list(unresolved, n_flagged, info)
#     $sides(qs, b_tab, scan, ctx) -> list(lower_unbounded, upper_unbounded,
#       unresolved_endpoints, closure_diagnostics, info)
#     $claim_failure(b, fit, precheck, ctx) -> list(claimed, domain_state,
#       reason, probe_targets)                                   (optional)
#     $probe(j, side, ctx) -> diagnostics only                   (reserved;
#       status-bearing probes execute through $sides)
#
# Sourced by log_var_eq.R after log_var_eq_map.R.

source("scripts-paper/log_var_eq_engine_context.R")
source("scripts-paper/log_var_eq_engine_scan.R")
source("scripts-paper/log_var_eq_engine_result.R")
source("scripts-paper/log_var_eq_engine_sides.R")

logvar_engine_set_at_tau <- function(est, qs, b_tab, b_seed = NULL,
                                     grid_n = 41L, grid_floor = 100L,
                                     extra_starts = NULL,
                                     max_grid_points = NULL,
                                     max_fit_evals = Inf, cache = NULL,
                                     budget_state = NULL,
                                     cold_start_check = TRUE,
                                     tau = NA_real_) {
  meta <- est$metadata
  bs <- if (is.null(budget_state)) logvar_budget_state(max_fit_evals) else budget_state
  if (is.null(cache)) cache <- new.env(parent = emptyenv())
  logvar_cache_bind(cache, meta)
  delta <- .derive_theta_scale(qs)
  omega <- .derive_constraint_scales(qs, delta)
  evaluate_fit <- logvar_make_evaluator(est, cache, bs)
  ctx <- logvar_make_ctx(evaluate_fit, qs, delta, omega, cache, bs)
  st <- new.env(parent = emptyenv())
  st$labels <- est$coef_labels
  st$n_feasible <- NA_integer_
  st$n_fail <- 0L
  labels_now <- function() {
    if (!is.null(st$labels)) st$labels else paste0("coef", seq_len(ncol(qs$A_i[[1]])))
  }
  diag_of <- function(extra = list()) {
    utils::modifyList(list(
      n_attempted = bs$n_attempted, n_evaluated = bs$n_evaluated,
      n_cached = bs$n_cached, n_failed = bs$n_failed,
      counters = bs$counters, budget_exhausted = FALSE
    ), extra)
  }
  fail_closed <- function(word, n_cross, n_feasible, dinfo = NULL, extra = list()) {
    logvar_engine_result_na(
      labels_now(), word, meta, tau, qs, omega, st$n_fail,
      n_cross, n_feasible, dinfo, diag_of(extra)
    )
  }
  if (any(b_tab$status != "bounded")) {
    word <- if (any(b_tab$status == "unbounded")) "unbounded" else "unreliable"
    return(fail_closed(word, NA_integer_, NA_integer_))
  }
  tryCatch(
    logvar_engine_run(
      est, qs, b_tab, b_seed, grid_n, grid_floor, extra_starts,
      max_grid_points, cold_start_check, tau, meta, bs, cache,
      evaluate_fit, ctx, st, omega, fail_closed, diag_of
    ),
    logvar_budget_exhausted = function(e) {
      fail_closed(
        "unreliable", NA_integer_, st$n_feasible, NULL,
        list(budget_exhausted = TRUE, budget_message = conditionMessage(e))
      )
    }
  )
}

# the post-gate engine body; split out so the budget condition wraps it
logvar_engine_run <- function(est, qs, b_tab, b_seed, grid_n, grid_floor,
                              extra_starts, max_grid_points, cold_start_check,
                              tau, meta, bs, cache, evaluate_fit, ctx, st,
                              omega, fail_closed, diag_of) {
  ad <- est$analyze_domain
  pre <- NULL
  if (!is.null(ad$precheck)) {
    pre <- logvar_call_precheck(ad$precheck, qs, b_tab, ctx)
    ctx$precheck <- pre
    if (length(pre$unresolved) > 0L) {
      return(fail_closed("unreliable", pre$n_flagged, NA_integer_))
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
    return(fail_closed("unreliable", pre_cross, 0L))
  }
  b_feas <- logvar_coarsen_grid(b_feas, max_grid_points)
  use_fast <- !is.null(est$scan_grid)
  if (!use_fast && !identical(meta$traversal, "lattice") &&
    is.null(max_grid_points) && nrow(b_feas) > 5000L) {
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
        bs$counters[["claimed_domain_failure"]] <-
          bs$counters[["claimed_domain_failure"]] + 1L
      }
      cl
    }
  }
  scan <- if (use_fast) {
    n_pts <- nrow(b_feas)
    if (bs$n_evaluated + n_pts > bs$max_fit_evals) {
      logvar_budget_stop("scan", sprintf(
        "batch scan of %d points exceeds max_fit_evals = %s",
        n_pts, format(bs$max_fit_evals)
      ))
    }
    scan_cap <- bs$phase_caps[["scan"]]
    if (!is.null(scan_cap) && bs$counters[["scan"]] + n_pts > scan_cap) {
      logvar_budget_stop("scan", sprintf("phase cap %d reached", scan_cap))
    }
    bs$counters[["scan"]] <- bs$counters[["scan"]] + n_pts
    bs$n_attempted <- bs$n_attempted + n_pts
    bs$n_evaluated <- bs$n_evaluated + n_pts
    est$scan_grid(b_feas)
  } else {
    logvar_engine_scan(est, b_feas, evaluate_fit, b_seed, claim_fn, st)
  }
  st$n_fail <- if (is.null(scan$n_fit_failures)) 0L else scan$n_fit_failures
  # unclaimed = total failures net of estimator-claimed ones, on either scan
  # path -- a batch scan cannot claim, so its failures always fail closed
  n_unclaimed <- st$n_fail - length(scan$claimed_failures)
  if (n_unclaimed > 0L) {
    return(fail_closed(
      "unreliable", pre_cross, st$n_feasible, NULL,
      list(unclaimed_fit_failures = n_unclaimed)
    ))
  }
  if (is.null(scan$min) || is.null(st$labels)) {
    return(fail_closed(
      "unreliable", pre_cross, st$n_feasible, NULL,
      list(no_successful_fits = TRUE)
    ))
  }
  logvar_engine_endpoints(
    est, qs, b_tab, b_seed, extra_starts, cold_start_check, tau, meta, bs,
    evaluate_fit, ctx, st, omega, scan, pre_cross, diag_of
  )
}
