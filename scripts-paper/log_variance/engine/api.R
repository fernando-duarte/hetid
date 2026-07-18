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
#   fit_at_b(b, start = NULL, phase = NULL) -> list(coef, fit_status, converged, objective,
#     score_norm, convergence_code, diagnostics, warm_start)
#     fit_at_b owns its retry/start ladder; diagnostics is a slim list (no
#     raw package objects -- cache entries must stay small). The optional phase
#     argument receives the engine service phase; two-argument closures retain
#     their legacy call. Diagnostics use standardized
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
# Sourced by run.R after residual_map.R.

paper_source_once(paper_path("log_variance", "engine", "context.R"))
paper_source_once(paper_path("log_variance", "engine", "grid_scan.R"))
paper_source_once(paper_path("log_variance", "engine", "results.R"))
paper_source_once(paper_path("log_variance", "engine", "endpoints.R"))
paper_source_once(paper_path("log_variance", "engine", "execute.R"))
paper_source_once(paper_path(
  "log_variance", "estimators", "set_orchestration.R"
))
# cross-estimator science helpers (spec_id, bounded-args, fragility, map-context
# preamble): the front door is common to the pipeline and every estimator test
paper_source_once(paper_path("log_variance", "estimators", "shared.R"))

logvar_engine_set_at_tau <- function(est, qs, b_tab, b_seed = NULL,
                                     grid_n = LOGVAR_SEARCH_CONTROL$grid_n,
                                     grid_floor = LOGVAR_SEARCH_CONTROL$grid_floor,
                                     extra_starts = NULL,
                                     max_grid_points = NULL,
                                     max_fit_evals = Inf, cache = NULL,
                                     budget_state = NULL,
                                     cold_start_check =
                                       LOGVAR_SEARCH_CONTROL$cold_start_check,
                                     tau = NA_real_,
                                     starts_per_side =
                                       LOGVAR_SEARCH_CONTROL$primary_starts_per_side,
                                     grid_selector = NULL) {
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
      evaluate_fit, ctx, st, omega, fail_closed, diag_of,
      starts_per_side, grid_selector
    ),
    logvar_budget_exhausted = function(e) {
      fail_closed(
        "unreliable", NA_integer_, st$n_feasible, NULL,
        list(budget_exhausted = TRUE, budget_message = conditionMessage(e))
      )
    }
  )
}
