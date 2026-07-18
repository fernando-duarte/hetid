# Result assembly for the estimator-generic set engine: the legacy table
# is byte-identical to the benchmark driver's; the schema adds the
# side-specific contract columns, tau, provenance, and the attaining
# points as list columns. Definitions only; sourced by
# api.R.

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

# cold-start replication of accepted endpoints: refit each certified
# side's attaining point from a cold start, bypassing the cache; a
# disagreement beyond metadata$cold_start_rtol flags the side unreliable
# and the observed disagreement is always recorded
logvar_engine_cold_check <- function(meta, labels, lower, upper, arg_lo,
                                     arg_up, lower_unb, upper_unb,
                                     lo_unrel, up_unrel, evaluate_fit) {
  rtol <- if (is.null(meta$cold_start_rtol)) {
    LOGVAR_SEARCH_CONTROL$cold_start_rtol_fallback
  } else {
    meta$cold_start_rtol
  }
  records <- list()
  for (j in seq_along(labels)) {
    for (side in c("min", "max")) {
      unb <- if (side == "min") lower_unb[j] else upper_unb[j]
      unrel <- if (side == "min") lo_unrel[j] else up_unrel[j]
      val <- if (side == "min") lower[j] else upper[j]
      arg <- if (side == "min") arg_lo[j, ] else arg_up[j, ]
      if (unb || unrel || !is.finite(val) || anyNA(arg)) next
      fitc <- evaluate_fit(arg, phase = "cold_start", start = NULL, use_cache = FALSE)
      vc <- if (logvar_fit_ok(fitc)) unname(fitc$coef[[j]]) else NaN
      if (!is.finite(vc) || abs(vc - val) > rtol * max(1, abs(val))) {
        if (side == "min") lo_unrel[j] <- TRUE else up_unrel[j] <- TRUE
        records[[length(records) + 1L]] <- list(
          coef = labels[j], side = side, value = val, cold_value = vc
        )
      }
    }
  }
  list(records = records, lo_unrel = lo_unrel, up_unrel = up_unrel)
}
# deterministic lattice-order coarsening to at most max_pts retained rows;
# runs before any ordering, never after (coarsening must not require the
# quadratic ordering it exists to make affordable)
logvar_coarsen_grid <- function(b_feas, max_pts) {
  m <- nrow(b_feas)
  if (is.null(max_pts) || m <= max_pts) {
    return(b_feas)
  }
  b_feas[seq(1L, m, by = ceiling(m / max_pts)), , drop = FALSE]
}

# k-best separated start pools from the generic scan: per coefficient and
# side, rank candidates by extremeness and accept greedily only when the
# candidate sits farther than sep from every accepted member, so a pool of
# three is never three copies of one corner; the primary arg-extreme is the
# first acceptance and is excluded from the returned pool
logvar_point_precedes <- function(x, y) {
  if (anyNA(y)) {
    return(TRUE)
  }
  differing <- which(unname(x) != unname(y))
  length(differing) > 0L && x[differing[1L]] < y[differing[1L]]
}

logvar_engine_scan_pools <- function(vals, pts, pool_k, sep) {
  point_matrix <- do.call(rbind, pts)
  rank_candidates <- function(value) {
    order_args <- c(
      list(value),
      lapply(seq_len(ncol(point_matrix)), function(j) point_matrix[, j]),
      list(method = "radix")
    )
    do.call(order, order_args)
  }
  pick <- function(ord) {
    kept <- list()
    for (i in ord) {
      b <- pts[[i]]
      far <- all(vapply(kept, function(a) sqrt(sum((a - b)^2)) > sep, logical(1)))
      if (length(kept) == 0L || far) kept[[length(kept) + 1L]] <- b
      if (length(kept) >= pool_k) break
    }
    kept[-1L]
  }
  n_coef <- ncol(vals)
  list(
    arg_min_pool = lapply(seq_len(n_coef), function(j) {
      pick(rank_candidates(vals[, j]))
    }),
    arg_max_pool = lapply(seq_len(n_coef), function(j) {
      pick(rank_candidates(-vals[, j]))
    })
  )
}
