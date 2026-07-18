# Traversal, generic grid scan, extra-start candidates, and result assembly
# for the estimator-generic set engine (api.R): deterministic
# nearest-neighbor ordering, deterministic lattice-order coarsening, the
# per-point fit loop with warm-start threading and claimed/unclaimed failure
# routing, and the shared table/schema builders. Definitions only; sourced
# by api.R.

# deterministic greedy nearest-neighbor ordering from b_seed (fallback: the
# first grid row); ties break toward the lowest row index via which.min
logvar_order_grid_nn <- function(b_feas, b_seed = NULL) {
  m <- nrow(b_feas)
  if (m == 0L) {
    return(integer(0))
  }
  cur <- if (is.null(b_seed) || anyNA(b_seed)) {
    1L
  } else {
    which.min(colSums((t(b_feas) - b_seed)^2))
  }
  ord <- integer(m)
  left <- rep(TRUE, m)
  for (k in seq_len(m)) {
    ord[k] <- cur
    left[cur] <- FALSE
    if (k == m) break
    d <- colSums((t(b_feas) - b_feas[cur, ])^2)
    d[!left] <- Inf
    cur <- which.min(d)
  }
  ord
}

# generic per-point scan: fit every grid point through the cached evaluator
# in traversal order, thread warm starts along the path, record failures
# point by point (claimed ones routed onward to the sides phase, unclaimed
# ones left for the engine's fail-closed rule), and track per-coefficient
# extremes with their attaining points; st collects labels as discovered so
# a budget stop mid-scan still knows the coefficient axis
logvar_engine_scan <- function(est, b_feas, evaluate_fit, b_seed, claim_fn, st,
                               traversal_override = NULL, pool_k = 1L,
                               pool_sep = Inf) {
  traversal <- if (is.null(traversal_override)) {
    est$metadata$traversal
  } else {
    traversal_override
  }
  ord <- if (identical(traversal, "lattice")) {
    seq_len(nrow(b_feas))
  } else {
    logvar_order_grid_nn(b_feas, b_seed)
  }
  best_min <- best_max <- arg_min <- arg_max <- NULL
  warm <- NULL
  failures <- list()
  claimed <- list()
  fit_statuses <- character(length(ord))
  vals <- list()
  pts <- list()
  for (k in seq_along(ord)) {
    b <- b_feas[ord[k], ]
    fit <- evaluate_fit(b, phase = LOGVAR_ENGINE_PHASES[["scan"]], start = warm)
    fit_statuses[k] <- if (is.null(fit$fit_status)) NA_character_ else fit$fit_status
    ok <- logvar_fit_ok(fit)
    if (!ok) {
      rec <- list(b = b, fit = fit)
      cl <- if (is.null(claim_fn)) NULL else claim_fn(b, fit)
      if (isTRUE(cl$claimed)) {
        rec$claim <- cl
        claimed[[length(claimed) + 1L]] <- rec
      } else {
        failures[[length(failures) + 1L]] <- rec
      }
      next
    }
    warm <- fit$warm_start
    if (is.null(st$labels)) st$labels <- names(fit$coef)
    v <- unname(fit$coef)
    if (pool_k > 1L) {
      vals[[length(vals) + 1L]] <- v
      pts[[length(pts) + 1L]] <- b
    }
    if (is.null(best_min)) {
      best_min <- rep(Inf, length(v))
      best_max <- rep(-Inf, length(v))
      arg_min <- arg_max <- matrix(NA_real_, length(v), ncol(b_feas))
    }
    for (j in seq_along(v)) {
      min_tie <- v[j] == best_min[j] &&
        logvar_point_precedes(b, arg_min[j, ])
      max_tie <- v[j] == best_max[j] &&
        logvar_point_precedes(b, arg_max[j, ])
      if (v[j] < best_min[j] || min_tie) {
        best_min[j] <- v[j]
        arg_min[j, ] <- b
      }
      if (v[j] > best_max[j] || max_tie) {
        best_max[j] <- v[j]
        arg_max[j, ] <- b
      }
    }
  }
  pools <- if (pool_k > 1L && length(vals) > 0L) {
    logvar_engine_scan_pools(do.call(rbind, vals), pts, pool_k, pool_sep)
  } else {
    NULL
  }
  list(
    min = best_min, max = best_max, arg_min = arg_min, arg_max = arg_max,
    arg_min_pool = pools$arg_min_pool, arg_max_pool = pools$arg_max_pool,
    domain_info = NULL, n_fit_failures = length(failures) + length(claimed),
    fit_statuses = fit_statuses, failures = failures,
    claimed_failures = claimed
  )
}

# extra starts as attained candidates: flattened, deduplicated at full
# precision, feasibility-screened, then evaluated through the cached
# evaluator; an infeasible or failed start is diagnostic-only, never fatal
logvar_extra_candidates <- function(starts, evaluate_fit, check_feasible) {
  flat <- list()
  walk <- function(x) {
    if (is.numeric(x)) {
      flat[[length(flat) + 1L]] <<- unname(x)
    } else if (is.list(x)) {
      for (el in x) walk(el)
    }
  }
  walk(starts)
  flat <- unique(flat)
  points <- list()
  values <- list()
  skipped <- list()
  for (b in flat) {
    feas <- check_feasible(b)
    if (!isTRUE(feas$feasible)) {
      skipped[[length(skipped) + 1L]] <- list(
        b = b, reason = "infeasible", max_violation = feas$max_violation
      )
      next
    }
    fit <- evaluate_fit(b, phase = LOGVAR_ENGINE_PHASES[["extra_start"]])
    ok <- logvar_fit_ok(fit)
    if (!ok) {
      skipped[[length(skipped) + 1L]] <- list(b = b, reason = "fit_failure")
      next
    }
    points[[length(points) + 1L]] <- b
    values[[length(values) + 1L]] <- unname(fit$coef)
  }
  list(points = points, values = values, skipped = skipped)
}

# fold extra-start attained candidates into the running endpoints: a value
# beyond the current extreme moves the endpoint, its point, and the
# provenance label on every non-divergent side
logvar_apply_extra_candidates <- function(ec, state, lower_unb, upper_unb) {
  for (i in seq_along(ec$points)) {
    v <- ec$values[[i]]
    for (j in seq_along(state$lower)) {
      if (!lower_unb[j] && v[j] < state$lower[j]) {
        state$lower[j] <- v[j]
        state$arg_lo[j, ] <- ec$points[[i]]
        state$prov_lo[j] <- "extra-start"
      }
      if (!upper_unb[j] && v[j] > state$upper[j]) {
        state$upper[j] <- v[j]
        state$arg_up[j, ] <- ec$points[[i]]
        state$prov_up[j] <- "extra-start"
      }
    }
  }
  state
}
