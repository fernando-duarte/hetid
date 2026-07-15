# Context and budget services for the estimator-generic set engine
# (log_var_eq_engine.R): the shared per-phase evaluation budget, the exact-b
# memoization cache around fit_at_b, the versioned hook context handed to
# analyze_domain phases, arity adapters for legacy hooks, and the figure-path
# and nesting helpers shared by the bounds-by-tau drivers. Definitions only;
# sourced by log_var_eq_engine.R.

# reference-semantics budget shared across engine calls: a global fit-eval
# cap, optional per-phase caps, and a named counter per service phase
logvar_budget_state <- function(max_fit_evals = Inf, phase_caps = NULL) {
  bs <- new.env(parent = emptyenv())
  bs$max_fit_evals <- max_fit_evals
  bs$phase_caps <- phase_caps
  bs$counters <- c(
    scan = 0L, probe = 0L, refinement = 0L, extra_start = 0L, polish = 0L,
    nonunique = 0L, cold_start = 0L, cache_hit = 0L,
    claimed_domain_failure = 0L
  )
  bs$n_attempted <- 0L
  bs$n_evaluated <- 0L
  bs$n_cached <- 0L
  bs$n_failed <- 0L
  bs
}

# exhaustion is a classed condition the engine catches at the tau level:
# fail closed with full disclosure, never a silently narrowed range
logvar_budget_stop <- function(phase, reason) {
  stop(structure(
    class = c("logvar_budget_exhausted", "error", "condition"),
    list(
      message = sprintf("fit budget exhausted (%s): %s", phase, reason),
      call = NULL, phase = phase
    )
  ))
}

# a cache environment belongs to one estimator, sample, and spec: stamped on
# first use, asserted ever after, so a caller cannot share one across any of
# the three (the spec-keyed cache requirement, enforced not encoded)
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

# full-precision cache key, insensitive to options(digits)
logvar_b_key <- function(b) {
  paste(formatC(unname(b), digits = 17, format = "fg", flag = "#"),
    collapse = "|"
  )
}

# cached evaluator around est$fit_at_b: debits the named phase only on a
# cache miss, enforces the global and per-phase caps, and tallies failures;
# use_cache = FALSE (cold-start replication) always refits and never stores
logvar_make_evaluator <- function(est, cache, bs) {
  accepts_phase <- "phase" %in% names(formals(est$fit_at_b))
  function(b, phase, start = NULL, use_cache = TRUE) {
    key <- logvar_b_key(b)
    if (use_cache && !is.null(cache$store[[key]])) {
      bs$counters[["cache_hit"]] <- bs$counters[["cache_hit"]] + 1L
      bs$n_attempted <- bs$n_attempted + 1L
      bs$n_cached <- bs$n_cached + 1L
      return(cache$store[[key]])
    }
    if (bs$n_evaluated >= bs$max_fit_evals) {
      logvar_budget_stop(phase, sprintf(
        "max_fit_evals = %s reached", format(bs$max_fit_evals)
      ))
    }
    cap <- bs$phase_caps[[phase]]
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
    ok <- identical(fit$fit_status, "ok") && isTRUE(fit$converged)
    if (!ok) bs$n_failed <- bs$n_failed + 1L
    if (use_cache) cache$store[[key]] <- fit
    fit
  }
}

# versioned context handed to analyze_domain hooks; check_feasible reports
# the normalized constraint values (house hin <= 0, grid-admission scale)
logvar_make_ctx <- function(evaluate_fit, qs, delta, omega, cache, bs) {
  check_feasible <- function(b) {
    hin <- vapply(seq_along(qs$A_i), function(i) {
      (drop(t(b) %*% qs$A_i[[i]] %*% b) +
        sum(qs$b_i[[i]] * b) + qs$c_i[i]) / omega[i]
    }, numeric(1))
    list(feasible = max(hin) <= 1e-10, max_violation = max(hin), hin = hin)
  }
  list(
    schema_version = "1.0.0", evaluate_fit = evaluate_fit,
    check_feasible = check_feasible,
    b_scales = list(delta = delta, omega = omega),
    cache = cache, budget_state = bs, precheck = NULL
  )
}

# one shared definition of a usable fit: ok status, converged, and a
# finite coefficient vector
logvar_fit_ok <- function(fit) {
  identical(fit$fit_status, "ok") && isTRUE(fit$converged) &&
    !is.null(fit$coef) && all(is.finite(fit$coef))
}

# selector seam: a caller-supplied grid_selector(b_feas, max_grid_points)
# must return exactly list(grid, selector_id, traversal); the grid must be a
# unique rowwise subset of the freshly checked feasible grid (no invented
# points), and traversal is "engine_default" (normal ordering) or
# "as_selected" (scan in callback order, bypassing the nearest-neighbor cap)
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
    selector_id = sel$selector_id, n_selector_input = nrow(b_feas),
    n_selector_output = nrow(g)
  ))
}

# arity adapters: Plan 7's original two-/three-argument hooks keep working
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

# estimator-stamped figure path so two estimators' figures coexist
logvar_bounds_tau_path <- function(out_dir, metadata) {
  file.path(out_dir, sprintf("log_var_eq_bounds_tau_%s.pdf", metadata$estimator))
}

# nesting check over engine-computed grid rows: as tau grows a certified
# lower endpoint must not rise and a certified upper endpoint must not fall
# beyond tol * max(1, |endpoint|); comparisons run per coefficient and side
# over consecutive retained (bounded) rows, so they span unreliable or
# unbounded gaps; returns the violation rows
logvar_check_nesting <- function(rows, tol = 1e-6) {
  viol <- data.frame(
    coef = character(0), side = character(0), tau = numeric(0),
    violation = numeric(0)
  )
  for (cf in unique(rows$coef)) {
    for (side in c("lower", "upper")) {
      sub <- rows[rows$coef == cf & rows[[paste0(side, "_status")]] == "bounded", ]
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
