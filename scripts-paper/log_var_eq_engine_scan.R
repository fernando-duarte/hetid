# Traversal, generic grid scan, extra-start candidates, and result assembly
# for the estimator-generic set engine (log_var_eq_engine.R): deterministic
# nearest-neighbor ordering, deterministic lattice-order coarsening, the
# per-point fit loop with warm-start threading and claimed/unclaimed failure
# routing, and the shared table/schema builders. Definitions only; sourced
# by log_var_eq_engine.R.

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
logvar_engine_scan <- function(est, b_feas, evaluate_fit, b_seed, claim_fn, st) {
  traversal <- est$metadata$traversal
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
  for (k in seq_along(ord)) {
    b <- b_feas[ord[k], ]
    fit <- evaluate_fit(b, phase = "scan", start = warm)
    fit_statuses[k] <- if (is.null(fit$fit_status)) NA_character_ else fit$fit_status
    ok <- identical(fit$fit_status, "ok") && isTRUE(fit$converged) &&
      !is.null(fit$coef) && all(is.finite(fit$coef))
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
    if (is.null(best_min)) {
      best_min <- rep(Inf, length(v))
      best_max <- rep(-Inf, length(v))
      arg_min <- arg_max <- matrix(NA_real_, length(v), ncol(b_feas))
    }
    for (j in seq_along(v)) {
      if (v[j] < best_min[j]) {
        best_min[j] <- v[j]
        arg_min[j, ] <- b
      }
      if (v[j] > best_max[j]) {
        best_max[j] <- v[j]
        arg_max[j, ] <- b
      }
    }
  }
  list(
    min = best_min, max = best_max, arg_min = arg_min, arg_max = arg_max,
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
    fit <- evaluate_fit(b, phase = "extra_start")
    ok <- identical(fit$fit_status, "ok") && isTRUE(fit$converged) &&
      !is.null(fit$coef) && all(is.finite(fit$coef))
    if (!ok) {
      skipped[[length(skipped) + 1L]] <- list(b = b, reason = "fit_failure")
      next
    }
    points[[length(points) + 1L]] <- b
    values[[length(values) + 1L]] <- unname(fit$coef)
  }
  list(points = points, values = values, skipped = skipped)
}

# shared table/schema assembly: the legacy table is byte-identical to the
# benchmark driver's; the schema adds the side-specific contract columns,
# tau, provenance, and the attaining points as list columns
logvar_engine_result <- function(labels, lower, upper, lo_st, up_st,
                                 prov_lo, prov_up, arg_lo, arg_up,
                                 meta, tau, qs, omega, n_fail,
                                 n_cross, n_feasible, domain_info, diag) {
  status <- ifelse(
    lo_st == "unreliable" | up_st == "unreliable", "unreliable",
    ifelse(lo_st == "unbounded" | up_st == "unbounded", "unbounded", "bounded")
  )
  resid_at <- function(arg, val) {
    if (anyNA(arg) || !is.finite(val)) {
      return(NA_real_)
    }
    .feasibility_residual(qs, arg, omega)
  }
  n <- length(labels)
  schema <- data.frame(
    coef = labels, lower = lower, upper = upper,
    lower_status = lo_st, upper_status = up_st,
    lower_fit_status = ifelse(lo_st == "bounded", "ok", NA_character_),
    upper_fit_status = ifelse(up_st == "bounded", "ok", NA_character_),
    fit_failure_count = rep(n_fail, n),
    lower_constraint_residual = vapply(
      seq_len(n), function(j) resid_at(arg_lo[j, ], lower[j]), numeric(1)
    ),
    upper_constraint_residual = vapply(
      seq_len(n), function(j) resid_at(arg_up[j, ], upper[j]), numeric(1)
    ),
    estimator = meta$estimator, target_functional = meta$target_functional,
    sample_id = meta$sample_id, tau = tau,
    lower_provenance = prov_lo, upper_provenance = prov_up,
    row.names = NULL
  )
  schema$arg_lower <- I(lapply(seq_len(n), function(j) arg_lo[j, ]))
  schema$arg_upper <- I(lapply(seq_len(n), function(j) arg_up[j, ]))
  list(
    table = data.frame(
      coef = labels, set_lower = lower, set_upper = upper, status = status,
      row.names = NULL
    ),
    schema = schema, n_cross = n_cross, n_feasible = n_feasible,
    domain_info = domain_info, diagnostics = diag
  )
}

# fail-closed variant: every endpoint NA under one propagated status word
logvar_engine_result_na <- function(labels, word, meta, tau, qs, omega,
                                    n_fail, n_cross, n_feasible,
                                    domain_info, diag) {
  n <- length(labels)
  na_arg <- matrix(NA_real_, n, length(qs$b_i[[1]]))
  logvar_engine_result(
    labels, rep(NA_real_, n), rep(NA_real_, n), rep(word, n), rep(word, n),
    rep(NA_character_, n), rep(NA_character_, n), na_arg, na_arg,
    meta, tau, qs, omega, n_fail, n_cross, n_feasible, domain_info, diag
  )
}
