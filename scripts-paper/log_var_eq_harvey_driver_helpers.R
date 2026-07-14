# Artifact-free driver helpers for the Harvey log-variance map: the reduced
# pre-render sensitivity gate, the Design-decision-4 stability-precheck pair
# builder, and the console diagnostics block. The gate re-polishes each display
# tau with five starts per side over a fresh cache and budget and the SAME
# warm-refined boxes, then reuses Plan 1's pure union/demotion/audit apply
# (logvar_ppml_apply_coverage) so a more extreme certified endpoint is kept with
# provenance while a moved or status-mismatched bounded side is downgraded to
# unreliable; the apply's reason vocabulary (coverage_run_failed / endpoint_moved
# / status_mismatch) is inherited verbatim rather than duplicated. Definitions
# only; the engine, budget state, and apply are resolved at call time. Sourced by
# log_var_eq_harvey_sets.R after log_var_eq_harvey.R and the PPML modules.

# The reduced sensitivity gate: for every display tau, a five-start re-polish of
# the same warm-refined box over one fresh cache (nesting reuse across taus) and
# a fresh per-tau budget. A failed rerun records list(ok = FALSE, error = ...) so
# apply_coverage demotes that tau's bounded primary sides rather than aborting.
# Returns Plan 1's apply list(results, audit, metadata) directly (downgrade plus
# union-extreme provenance), so no result view is left in a pre-audit state.
logvar_harvey_sensitivity_gate <- function(est, taus, b_tabs, b_seed,
                                           grid_cap, fit_budget, qs_fn, primary) {
  gate_cache <- new.env(parent = emptyenv())
  keys <- vapply(taus, function(tau) sprintf("%.17g", tau), character(1))
  gate_results <- vector("list", length(taus))
  names(gate_results) <- keys
  for (i in seq_along(taus)) {
    tau <- taus[[i]]
    key <- keys[[i]]
    bs <- logvar_budget_state(fit_budget)
    gate_results[[key]] <- tryCatch(
      list(ok = TRUE, res = logvar_engine_set_at_tau(
        est, qs_fn(tau), b_tabs[[key]],
        b_seed = b_seed, max_grid_points = grid_cap, starts_per_side = 5L,
        cache = gate_cache, budget_state = bs,
        cold_start_check = TRUE, tau = tau
      )),
      error = function(e) list(ok = FALSE, error = conditionMessage(e))
    )
  }
  logvar_ppml_apply_coverage(
    primary, gate_results,
    tol = 1e-4,
    grid_cap = grid_cap, fit_budget = fit_budget,
    cache_stamp = est$metadata$spec_id,
    selector_id = "five-start-repolish-v1"
  )
}

# The exact Design-decision-4 response/start pairs for the pure precheck, in
# order: the reference squared residuals from their own intercept-only start; the
# anchor response from the accepted PPML original-scale coefficients when that
# start exists; and the anchor response from its own intercept-only start. A
# missing PPML pair is recorded in the ppml_pair_present attribute, never
# fabricated. intercept_only(y) = c(log(mean(y)), rep(0, ncol(X) - 1L)).
logvar_harvey_precheck_pairs <- function(y_ref, b_anchor, w1, w2, x_mat,
                                         ppml_bundle) {
  intercept_only <- function(y) c(log(mean(y)), rep(0, ncol(x_mat) - 1L))
  y_anchor <- drop(w1 - w2 %*% b_anchor)^2
  pairs <- list(
    list(y = y_ref, start = intercept_only(y_ref), label = "ref_intercept")
  )
  ppml_present <- !is.null(ppml_bundle) && !is.null(ppml_bundle$coef_original)
  if (ppml_present) {
    pairs <- c(pairs, list(list(
      y = y_anchor, start = ppml_bundle$coef_original, label = "anchor_ppml"
    )))
  }
  pairs <- c(pairs, list(list(
    y = y_anchor, start = intercept_only(y_anchor), label = "anchor_intercept"
  )))
  attr(pairs, "ppml_pair_present") <- ppml_present
  pairs
}

# Console block mirroring the PPML report: N and span, per-tau coefficient hulls
# with the tau's attempted/evaluated/cached/failed budget counters, the benchmark
# census printed as a comparability line only (never a Harvey status input), the
# winning point start rung, the sensitivity-gate outcome, and the Harvey-vs-PPML
# Lewbel-point slope comparison with a soft flag on any slope sign disagreement.
logvar_harvey_report <- function(hv, taus) {
  cat(sprintf(
    "Harvey log-variance map: N = %d over %s to %s\n",
    hv$sample$n, format(hv$sample$span[1]), format(hv$sample$span[2])
  ))
  tau_names <- names(hv$sets)
  for (i in seq_along(tau_names)) {
    tb <- hv$sets[[tau_names[i]]]
    d <- hv$counts[[tau_names[i]]]
    hull <- vapply(seq_len(nrow(tb)), function(j) {
      if (identical(tb$status[j], "bounded")) {
        sprintf("[%.3g,%.3g]", tb$set_lower[j], tb$set_upper[j])
      } else {
        tb$status[j]
      }
    }, character(1))
    cat(sprintf(
      "  tau = %.2g: %s | attempted %d evaluated %d cached %d failed %d\n",
      taus[i], paste(hull, collapse = " "),
      d$n_attempted, d$n_evaluated, d$n_cached, d$n_failed
    ))
  }
  cat(sprintf(
    "  census comparability (benchmark n_cross by tau): %s\n",
    paste(sprintf("%.2g=%d", taus, hv$census_comparability), collapse = " ")
  ))
  cat(sprintf(
    "  Lewbel-point start rung: %s\n",
    if (is.na(hv$point_start_rung)) "-- (no point fit)" else hv$point_start_rung
  ))
  audit <- hv$sensitivity_audit$audit
  if (is.null(audit)) {
    cat("  sensitivity gate: no sides evaluated\n")
  } else {
    demoted <- audit[!is.na(audit$reason), , drop = FALSE]
    if (nrow(demoted) == 0L) {
      cat(sprintf("  sensitivity gate: %d sides agree, none downgraded\n", nrow(audit)))
    } else {
      by_reason <- table(demoted$reason)
      cat(sprintf(
        "  sensitivity gate: %d of %d sides downgraded (%s)\n",
        nrow(demoted), nrow(audit),
        paste(sprintf("%s=%d", names(by_reason), as.integer(by_reason)), collapse = " ")
      ))
    }
  }
  # Harvey-vs-PPML Lewbel-point slopes: the shape-dependence diagnostic; a soft
  # flag when a slope sign flips (economics can move magnitudes, but two
  # estimators of one conditional variance flipping a loading sign reads as a bug)
  if (exists("log_var_eq_ppml", inherits = TRUE)) {
    coefs <- hv$table$coef
    h <- hv$table$point
    p <- log_var_eq_ppml$table$point
    slope <- coefs != "(Intercept)"
    cat("  Harvey vs PPML Lewbel-point slopes:\n")
    for (j in which(slope)) {
      cat(sprintf("    %-10s Harvey %8.4f  PPML %8.4f\n", coefs[j], h[j], p[j]))
    }
    flip <- slope & is.finite(h) & is.finite(p) & sign(h) != sign(p) &
      h != 0 & p != 0
    for (j in which(flip)) {
      cat(sprintf("  slope sign flip: %s\n", coefs[j]))
    }
  }
  invisible(NULL)
}
