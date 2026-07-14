# The independent PPML coverage gate: an up-to-8000-point Morton-order run
# through the engine seam with its own fresh cache and budget, and the atomic
# apply step that unions every freshly certified feasible primary and coverage
# endpoint, keeps the most extreme attained value with its provenance, and
# downgrades a side to unreliable on any status mismatch, endpoint movement, or
# coverage-run failure. Sourced by log_var_eq_ppml_driver_helpers.R; the
# selector (logvar_ppml_morton_select) and the engine are resolved at call time.
# Definitions only; no globals are read -- the caller supplies qs_fn(tau).

# Run the coverage scan for every display tau over the same fresh cache (nesting
# reuse across taus) but a fresh budget per tau, driving the engine with the
# Morton selector, five starts per side, and the recorded search seed. A failed
# tau records list(ok = FALSE, error = message) rather than aborting the gate.
logvar_ppml_coverage_run <- function(est_cov, taus, b_tabs, b_seed,
                                     grid_cap, fit_budget, qs_fn) {
  cov_cache <- new.env(parent = emptyenv())
  keys <- vapply(taus, function(tau) sprintf("%.17g", tau), character(1))
  out <- vector("list", length(taus))
  names(out) <- keys
  for (i in seq_along(taus)) {
    tau <- taus[[i]]
    key <- keys[[i]]
    bs <- logvar_budget_state(fit_budget)
    out[[key]] <- tryCatch(
      list(ok = TRUE, res = logvar_engine_set_at_tau(
        est_cov, qs_fn(tau), b_tabs[[key]],
        b_seed = b_seed, max_grid_points = grid_cap, starts_per_side = 5L,
        cache = cov_cache, budget_state = bs,
        grid_selector = logvar_ppml_morton_select,
        cold_start_check = TRUE, tau = tau
      )),
      error = function(e) list(ok = FALSE, error = conditionMessage(e))
    )
  }
  out
}

# per-coefficient aggregate status via the legacy ladder unreliable > unbounded
# > bounded (a side of the worse kind sets the coefficient status)
.logvar_status_ladder <- function(lo_st, up_st) {
  ifelse(lo_st == "unreliable" | up_st == "unreliable", "unreliable",
    ifelse(lo_st == "unbounded" | up_st == "unbounded", "unbounded", "bounded")
  )
}

# suffix a provenance string with the source it came from; a missing provenance
# collapses to the bare source label
.logvar_prov_suffix <- function(prov, source) {
  if (is.null(prov) || length(prov) != 1L || is.na(prov)) {
    return(source)
  }
  paste0(prov, "+", source)
}

# the union-extreme over the certified (bounded, finite) candidates: the most
# extreme attained value with its source, provenance, arg, fit status, and
# residual; a tie keeps the primary; no certified candidate keeps the primary
# value so an unbounded or unreliable side is never fabricated into a number
.logvar_union_extreme <- function(side, prim, cov) {
  cand <- list()
  if (identical(prim$status, "bounded") && is.finite(prim$value)) cand$primary <- prim
  if (!is.null(cov) && identical(cov$status, "bounded") && is.finite(cov$value)) {
    cand$coverage <- cov
  }
  if (length(cand) == 0L) {
    return(list(
      value = prim$value, source = "primary",
      provenance = .logvar_prov_suffix(prim$prov, "primary"),
      arg = prim$arg, fit_status = prim$fit, residual = prim$res
    ))
  }
  vals <- vapply(cand, function(z) z$value, numeric(1))
  idx <- if (side == "lower") which.min(vals) else which.max(vals)
  src <- names(cand)[[idx]]
  z <- cand[[src]]
  list(
    value = z$value, source = src,
    provenance = .logvar_prov_suffix(z$prov, src),
    arg = z$arg, fit_status = z$fit, residual = z$res
  )
}

# read one side of one coefficient out of an engine schema row into a plain list
.logvar_side_read <- function(sch, side, j) {
  list(
    value = sch[[side]][[j]], status = sch[[paste0(side, "_status")]][[j]],
    prov = sch[[paste0(side, "_provenance")]][[j]],
    arg = sch[[paste0("arg_", side)]][[j]],
    fit = sch[[paste0(side, "_fit_status")]][[j]],
    res = sch[[paste0(side, "_constraint_residual")]][[j]]
  )
}

# resolve one coefficient/side: the reliability verdict (downgrade only) and the
# union-extreme value/provenance, returned with the audit-row fields
.logvar_cov_resolve <- function(side, j, sch, cov_sch, cov_failed, tol) {
  prim <- .logvar_side_read(sch, side, j)
  if (cov_failed) {
    bounded <- identical(prim$status, "bounded")
    pick <- .logvar_union_extreme(side, prim, NULL)
    return(c(pick, list(
      final_status = if (bounded) "unreliable" else prim$status,
      primary_status = prim$status, coverage_status = "coverage_run_failed",
      primary_value = prim$value, coverage_value = NA_real_, delta = NA_real_,
      reason = if (bounded) "coverage_run_failed" else NA_character_
    )))
  }
  cov <- .logvar_side_read(cov_sch, side, j)
  both <- identical(prim$status, "bounded") && identical(cov$status, "bounded")
  delta <- if (both) abs(prim$value - cov$value) else NA_real_
  moved <- both && is.finite(delta) &&
    delta > tol * max(1, abs(prim$value), abs(cov$value))
  verdict <- if (both) {
    if (moved) list("unreliable", "endpoint_moved") else list("bounded", NA_character_)
  } else if (identical(prim$status, cov$status)) {
    list(prim$status, NA_character_)
  } else {
    list("unreliable", "status_mismatch")
  }
  pick <- .logvar_union_extreme(side, prim, cov)
  c(pick, list(
    final_status = verdict[[1L]], primary_status = prim$status,
    coverage_status = cov$status, primary_value = prim$value,
    coverage_value = cov$value, delta = delta, reason = verdict[[2L]]
  ))
}

# Atomic per-tau union/demotion apply: for every coefficient and side, form the
# reliability verdict and union-extreme, then rebuild the engine schema rows, the
# sets table (set_lower/set_upper/status), and one audit row per side. Returns
# the updated per-tau engine results, the typed audit frame, and version-stamped
# metadata (selector id, cap, budget, cache stamp). No result view is left in a
# pre-audit state, so a newly found more-extreme candidate is never discarded.
logvar_ppml_apply_coverage <- function(primary, coverage, tol = 1e-4,
                                       grid_cap = NA_integer_,
                                       fit_budget = NA_integer_,
                                       cache_stamp = NA_character_,
                                       selector_id = "morton-v1") {
  results <- list()
  audit_rows <- list()
  for (key in names(primary)) {
    prim_res <- primary[[key]]
    sch <- prim_res$schema
    cov_entry <- coverage[[key]]
    cov_failed <- is.null(cov_entry) || !isTRUE(cov_entry$ok)
    cov_sch <- if (cov_failed) NULL else cov_entry$res$schema
    n <- nrow(sch)
    tau_val <- if (n > 0L) sch$tau[[1L]] else NA_real_
    for (j in seq_len(n)) {
      for (side in c("lower", "upper")) {
        r <- .logvar_cov_resolve(side, j, sch, cov_sch, cov_failed, tol)
        sch[[side]][j] <- r$value
        sch[[paste0(side, "_status")]][j] <- r$final_status
        sch[[paste0(side, "_provenance")]][j] <- r$provenance
        sch[[paste0(side, "_fit_status")]][j] <- r$fit_status
        sch[[paste0(side, "_constraint_residual")]][j] <- r$residual
        sch[[paste0("arg_", side)]][[j]] <- r$arg
        audit_rows[[length(audit_rows) + 1L]] <- data.frame(
          tau = tau_val, coef = sch$coef[[j]], side = side,
          primary_status = r$primary_status, coverage_status = r$coverage_status,
          final_status = r$final_status, primary_value = r$primary_value,
          coverage_value = r$coverage_value, final_value = r$value,
          source = r$source, delta = r$delta, tol = tol, reason = r$reason,
          stringsAsFactors = FALSE
        )
      }
    }
    upd <- prim_res
    upd$schema <- sch
    upd$table <- data.frame(
      coef = sch$coef, set_lower = sch$lower, set_upper = sch$upper,
      status = .logvar_status_ladder(sch$lower_status, sch$upper_status),
      row.names = NULL, stringsAsFactors = FALSE
    )
    results[[key]] <- upd
  }
  list(
    results = results,
    audit = if (length(audit_rows)) do.call(rbind, audit_rows) else NULL,
    metadata = list(
      version = "1.0.0", selector_id = selector_id, grid_cap = grid_cap,
      fit_budget = fit_budget, cache_stamp = cache_stamp
    )
  )
}
