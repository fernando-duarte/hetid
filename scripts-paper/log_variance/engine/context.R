logvar_budget_state <- function(max_fit_evals = Inf, phase_caps = NULL) {
  if (!is.null(phase_caps)) {
    stopifnot(
      !is.null(names(phase_caps)),
      all(names(phase_caps) %in% LOGVAR_ENGINE_FIT_PHASES)
    )
  }
  bs <- new.env(parent = emptyenv())
  bs$max_fit_evals <- max_fit_evals
  bs$phase_caps <- phase_caps
  bs$counters <- stats::setNames(
    integer(length(LOGVAR_ENGINE_PHASES)),
    unname(LOGVAR_ENGINE_PHASES)
  )
  bs$n_attempted <- 0L
  bs$n_evaluated <- 0L
  bs$n_cached <- 0L
  bs$n_failed <- 0L
  bs
}
logvar_budget_stop <- function(phase, reason) {
  paper_stop_condition(
    "logvar_budget_exhausted",
    "logvar_engine_error",
    message = sprintf(
      "fit budget exhausted (%s): %s",
      phase,
      reason
    ),
    fields = list(phase = phase)
  )
}
logvar_cache_bind <- function(cache, meta) {
  if (is.null(cache$estimator)) {
    cache$estimator <- meta$estimator
    cache$sample_id <- meta$sample_id
    cache$spec_id <- meta$spec_id
    cache$store <- new.env(parent = emptyenv())
  }
  stopifnot(
    identical(cache$estimator, meta$estimator),
    identical(cache$sample_id, meta$sample_id),
    identical(cache$spec_id, meta$spec_id)
  )
  cache
}
logvar_b_key <- function(b) {
  paste(paper_numeric_key(unname(b)), collapse = "|")
}
logvar_make_evaluator <- function(est, cache, bs) {
  accepts_phase <- "phase" %in% names(formals(est$fit_at_b))
  function(b, phase, start = NULL, use_cache = TRUE) {
    stopifnot(
      is.character(phase),
      length(phase) == 1L,
      phase %in% LOGVAR_ENGINE_FIT_PHASES
    )
    key <- logvar_b_key(b)
    if (use_cache && !is.null(cache$store[[key]])) {
      cache_phase <- LOGVAR_ENGINE_PHASES[["cache_hit"]]
      bs$counters[[cache_phase]] <-
        bs$counters[[cache_phase]] + 1L
      bs$n_attempted <- bs$n_attempted + 1L
      bs$n_cached <- bs$n_cached + 1L
      return(cache$store[[key]])
    }
    if (bs$n_evaluated >= bs$max_fit_evals) {
      logvar_budget_stop(phase, sprintf(
        "max_fit_evals = %s reached", format(bs$max_fit_evals)
      ))
    }
    cap <- if (phase %in% names(bs$phase_caps)) bs$phase_caps[[phase]] else NULL
    if (!is.null(cap) && bs$counters[[phase]] >= cap) {
      logvar_budget_stop(phase, sprintf("phase cap %d reached", cap))
    }
    bs$counters[[phase]] <- bs$counters[[phase]] + 1L
    bs$n_attempted <- bs$n_attempted + 1L
    bs$n_evaluated <- bs$n_evaluated + 1L
    fit <- if (accepts_phase) {
      est$fit_at_b(b, start = start, phase = phase)
    } else {
      est$fit_at_b(b, start = start)
    }
    ok <- logvar_fit_ok(fit)
    if (!ok) bs$n_failed <- bs$n_failed + 1L
    if (use_cache) cache$store[[key]] <- fit
    fit
  }
}
logvar_make_ctx <- function(evaluate_fit, qs, delta, omega, cache, bs) {
  check_feasible <- function(b) {
    hin <- quadratic_constraint_values(b, qs, omega)
    list(
      feasible = max(hin) <= PAPER_QUADRATIC_CONTROL$admission_tolerance,
      max_violation = max(hin),
      hin = hin
    )
  }
  list(
    schema_version = "1.0.0", evaluate_fit = evaluate_fit,
    check_feasible = check_feasible,
    b_scales = list(delta = delta, omega = omega),
    cache = cache, budget_state = bs, precheck = NULL
  )
}
logvar_engine_apply_selector <- function(sel_fn, b_feas, max_grid_points) {
  sel <- sel_fn(b_feas, max_grid_points)
  if (!is.list(sel) || !setequal(names(sel), c("grid", "selector_id", "traversal"))) {
    stop("grid_selector must return list(grid, selector_id, traversal)")
  }
  if (!sel$traversal %in% c("engine_default", "as_selected")) {
    stop("grid_selector traversal must be engine_default or as_selected")
  }
  g <- sel$grid
  if (!is.matrix(g) || nrow(g) < 1L || ncol(g) != ncol(b_feas)) {
    stop("grid_selector returned a malformed grid")
  }
  keys_out <- apply(g, 1L, logvar_b_key)
  if (anyDuplicated(keys_out) > 0L) stop("grid_selector returned duplicate rows")
  if (!all(keys_out %in% apply(b_feas, 1L, logvar_b_key))) {
    stop("grid_selector invented rows outside the feasible grid")
  }
  list(grid = g, traversal = sel$traversal, info = list(
    selector_id = sel$selector_id, traversal = sel$traversal,
    n_selector_input = nrow(b_feas),
    n_selector_output = nrow(g)
  ))
}
logvar_call_precheck <- function(hook, qs, b_tab, ctx) {
  if (length(formals(hook)) >= 3L) hook(qs, b_tab, ctx) else hook(qs, b_tab)
}
logvar_call_sides <- function(hook, qs, b_tab, scan, ctx) {
  if (length(formals(hook)) >= 4L) {
    hook(qs, b_tab, scan, ctx)
  } else {
    hook(qs, b_tab, scan)
  }
}
logvar_bounds_tau_path <- function(metadata) {
  artifact_variant_path(
    "logvar_bounds_tau",
    metadata$estimator
  )
}
logvar_check_nesting <- function(
  rows,
  tol = LOGVAR_SEARCH_CONTROL$nesting_rtol
) {
  viol <- data.frame(
    coef = character(0), side = character(0), tau = numeric(0),
    violation = numeric(0)
  )
  for (cf in unique(rows$coef)) {
    for (side in c("lower", "upper")) {
      bounded <- PAPER_ENDPOINT_STATUS[["bounded"]]
      sub <- rows[
        rows$coef == cf & rows[[paste0(side, "_status")]] == bounded,
      ]
      sub <- sub[order(sub$tau), ]
      if (nrow(sub) < 2L) next
      vals <- sub[[side]]
      for (k in seq_len(nrow(sub) - 1L)) {
        gap <- if (side == "lower") vals[k + 1L] - vals[k] else vals[k] - vals[k + 1L]
        if (gap > tol * max(1, abs(vals[k]))) {
          viol <- rbind(viol, data.frame(
            coef = cf, side = side, tau = sub$tau[k + 1L], violation = gap
          ))
        }
      }
    }
  }
  viol
}
