# The published cells built from the shared reference distribution in
# endpoint_targets.R: one row builder both panels call, applying one gate policy,
# reporting Target P and carrying Target S alongside for the diagnostics. The
# full-sample per-side statuses decide the cell shape (two-sided, one live side,
# or suppressed). A cell that fails any gate condition is blank and records why.
# Consumed by mean_equation/inference/boot_results.R and
# support/inference/bootstrap_stage_result_helpers.R.

paper_source_once(paper_path(
  "support", "inference_post", "endpoint_targets.R"
))
paper_source_once(paper_path(
  "support", "inference_post", "endpoint_alternatives.R"
))

.endpoint_target_cell <- function(lc, uc, f, alpha, tolerance, min_reps) {
  bounded <- PAPER_ENDPOINT_STATUS[["bounded"]]
  unbounded <- PAPER_ENDPOINT_STATUS[["unbounded"]]
  blank <- list(
    side = "none", c_s = NA_real_, c_p_lower = NA_real_, c_p_upper = NA_real_,
    evals = NA_integer_, best_lambda = NA_real_, interior = NA,
    n_common = 0L, ci_lower = NA_real_, ci_upper = NA_real_
  )
  if (identical(f$lower_status, bounded) && identical(f$upper_status, bounded)) {
    if (!(lc$gate && uc$gate)) {
      return(c(blank, list(reason = if (!lc$gate) lc$reason else uc$reason)))
    }
    # both-bounded pool: a two-sided root needs z on BOTH sides in one draw, so a
    # draw bounded on one side only feeds that side's scale but not this pool.
    pool <- lc$ok & uc$ok
    # the absolute count applies to the PAIR of sides, not to each side alone.
    # Two side gates can both clear while their intersection does not: a cell with
    # 5,100 bounded lower draws and 8,500 bounded upper draws can have only 3,600
    # jointly bounded, and the quantile below runs on the intersection.
    if (sum(pool) < min_reps) {
      return(c(blank, list(reason = "insufficient bounded draws")))
    }
    c_s <- target_s_critical(pool, alpha, lc$z, uc$z)
    width <- f$set_upper - f$set_lower
    p <- target_p_critical(
      lc$z, uc$z, pool, width / lc$se, width / uc$se, alpha, tolerance, c_s
    )
    return(list(
      side = "two-sided", c_s = c_s, c_p_lower = p$c_p_lower,
      c_p_upper = p$c_p_upper, evals = p$evals, best_lambda = p$best_lambda,
      interior = p$interior, n_common = sum(pool),
      ci_lower = f$set_lower - p$c_p_upper * lc$se,
      ci_upper = f$set_upper + p$c_p_upper * uc$se, reason = "reported"
    ))
  }
  # One live side: the truth can sit anywhere on the infinite ray, so the worst
  # position is at the finite endpoint, the credit vanishes and both targets
  # coincide at the live side's own quantile. No lambda optimization is needed,
  # and the dead side's draw status must not exclude a bounded live-side draw.
  if (identical(f$lower_status, unbounded) && identical(f$upper_status, bounded)) {
    if (!uc$gate) {
      return(c(blank, list(reason = uc$reason)))
    }
    c_s <- target_s_critical(uc$ok, alpha, uc$z)
    return(list(
      side = "upper", c_s = c_s, c_p_lower = c_s, c_p_upper = c_s,
      evals = 0L, best_lambda = NA_real_, interior = FALSE,
      n_common = uc$n_ok, ci_lower = -Inf,
      ci_upper = f$set_upper + c_s * uc$se, reason = "reported"
    ))
  }
  if (identical(f$lower_status, bounded) && identical(f$upper_status, unbounded)) {
    if (!lc$gate) {
      return(c(blank, list(reason = lc$reason)))
    }
    c_s <- target_s_critical(lc$ok, alpha, lc$z)
    return(list(
      side = "lower", c_s = c_s, c_p_lower = c_s, c_p_upper = c_s,
      evals = 0L, best_lambda = NA_real_, interior = FALSE,
      n_common = lc$n_ok, ci_lower = f$set_lower - c_s * lc$se,
      ci_upper = Inf, reason = "reported"
    ))
  }
  if (identical(f$lower_status, unbounded) && identical(f$upper_status, unbounded)) {
    return(c(blank, list(reason = "full-sample set unbounded on both sides")))
  }
  c(blank, list(reason = "full-sample side not certified bounded (unreliable)"))
}

# A requested tau that no longer supports inference is a decision to revisit,
# not a blank to skim past. The gate reasons are the three ways a side fails
# regularity; a cell suppressed because the full-sample set is unbounded is a
# property of the data at that tolerance and passes through.
ENDPOINT_GATE_FAILURES <- c(
  "insufficient bounded draws",
  "boundedness unstable across draws",
  "degenerate endpoint scale"
)

endpoint_require_feasible <- function(tbl, tau, stability) {
  bad <- tbl[tbl$reason %in% ENDPOINT_GATE_FAILURES, , drop = FALSE]
  if (!nrow(bad)) {
    return(invisible(tbl))
  }
  stop(sprintf(
    paste0(
      "tau = %s is not feasible for the endpoint bootstrap.\n",
      "  %s\n",
      "  stability threshold %.2f; the two-sided pool needs both sides ",
      "bounded in the SAME draw, so a cell can fail while each side passes ",
      "alone.\n",
      "  Either drop this tau from the bootstrap grid or lower the threshold ",
      "deliberately -- do not let it blank silently."
    ),
    format(tau),
    paste(sprintf(
      "%s: %s (lower %.3f, upper %.3f bounded; pool %d)",
      bad$coef, bad$reason, bad$frac_lower, bad$frac_upper, bad$n_common
    ), collapse = "\n  "),
    stability
  ), call. = FALSE)
}

# One cell for one coefficient at one tau, from that coefficient's per-side draw
# columns and its full-sample row.
endpoint_target_row <- function(lower, upper, lower_status, upper_status, f,
                                alpha, min_reps, stability, tolerance) {
  lc <- endpoint_side_stat(
    lower, lower_status, f$set_lower, 1, min_reps, stability
  )
  uc <- endpoint_side_stat(
    upper, upper_status, f$set_upper, -1, min_reps, stability
  )
  cell <- .endpoint_target_cell(lc, uc, f, alpha, tolerance, min_reps)
  # diagnostics only: Target P above stays what the cell publishes
  alt <- endpoint_alternative_intervals(lower, upper, lc, uc, f, alpha)
  data.frame(
    se_lower = lc$se, se_upper = uc$se, n_lower = lc$n_ok, n_upper = uc$n_ok,
    n_common = cell$n_common, n_non_failed_lower = lc$n_valid,
    n_non_failed_upper = uc$n_valid, frac_lower = lc$frac, frac_upper = uc$frac,
    gate_lower = lc$gate, gate_upper = uc$gate, side = cell$side,
    c_s = cell$c_s, c_p_lower = cell$c_p_lower, c_p_upper = cell$c_p_upper,
    c_p_gap = cell$c_p_upper - cell$c_p_lower, c_p_evals = cell$evals,
    c_p_lambda = cell$best_lambda, c_p_interior = cell$interior,
    ci_lower = cell$ci_lower, ci_upper = cell$ci_upper, reason = cell$reason,
    alt, row.names = NULL, stringsAsFactors = FALSE
  )
}

# Every coefficient at one tau. draws carries the four B x n_coef matrices; full
# is the coef/set_lower/set_upper/lower_status/upper_status frame the draws
# re-estimate. ci_lower/ci_upper are the TARGET P interval; c_s rides along.
endpoint_target_table <- function(draws, full,
                                  alpha =
                                    PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha,
                                  min_reps = boot_min_reps(nrow(draws$lower)),
                                  stability =
                                    PAPER_ANALYSIS_CONTRACT$inference$stability_share,
                                  tolerance =
                                    PAPER_ANALYSIS_CONTRACT$inference$
                                      target_p_lambda_tolerance,
                                  tau = NULL) {
  stopifnot(
    identical(dim(draws$lower), dim(draws$upper)),
    identical(dim(draws$lower_status), dim(draws$lower)),
    identical(dim(draws$upper_status), dim(draws$upper)),
    ncol(draws$lower) == nrow(full)
  )
  rows <- lapply(seq_len(nrow(full)), function(k) {
    cbind(
      coef = full$coef[k],
      endpoint_target_row(
        draws$lower[, k], draws$upper[, k], draws$lower_status[, k],
        draws$upper_status[, k], full[k, ], alpha, min_reps, stability,
        tolerance
      ),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  # tau is a label for the message; callers that have it get the loud failure
  if (!is.null(tau)) {
    endpoint_require_feasible(out, tau, stability)
  }
  out
}
