# Published endpoint cells delegate calibration; paper owns gates and diagnostics.
paper_source_once(paper_path("support", "inference_post", "endpoint_targets.R"))
paper_source_once(paper_path("support", "inference_post", "endpoint_alternatives.R"))

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

# Keep the one-row entrypoint for paper checks and callers.
endpoint_target_row <- function(lower, upper, lower_status, upper_status, f,
                                alpha, min_reps, stability, tolerance) {
  make <- function(x) matrix(x, ncol = 1L, dimnames = list(NULL, f$coef))
  draws <- list(
    lower = make(lower), upper = make(upper),
    lower_status = make(lower_status), upper_status = make(upper_status)
  )
  out <- endpoint_target_table(draws, f, alpha, min_reps, stability, tolerance)
  out[, names(out) != "coef", drop = FALSE]
}

endpoint_target_table <- function(draws, full,
                                  alpha = PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha,
                                  min_reps = boot_min_reps(nrow(draws$lower)),
                                  stability = PAPER_ANALYSIS_CONTRACT$inference$stability_share,
                                  tolerance = PAPER_ANALYSIS_CONTRACT$inference$
                                    target_p_lambda_tolerance,
                                  tau = NULL) {
  input <- paper_endpoint_inputs(draws, full)
  fit <- hetid::bootstrap_set_interval(input$full, input$draws, "pointwise",
    alpha, min_reps, stability,
    # Preserve the paper's converge-to-tolerance policy for wide-credit cells.
    control = list(
      tolerance = tolerance,
      max_evals = PAPER_ANALYSIS_CONTRACT$inference$target_p_max_evals
    )
  )
  if (any(fit$summary$search_stop %in% c("max_evals", "precision"))) {
    stop("Endpoint calibration did not reach the paper's critical-value tolerance.")
  }
  # Preserve the existing publication schema; richer numerical diagnostics remain
  # available from the public package result.
  out <- fit$summary[, !names(fit$summary) %in%
    c("search_stop", "root_rank", "tail_resolution"), drop = FALSE]
  alt <- lapply(seq_len(nrow(full)), function(k) {
    as.data.frame(endpoint_alternative_intervals(
      draws$lower[, k], draws$upper[, k],
      fit$sides[[k]]$lower, fit$sides[[k]]$upper, full[k, ], alpha
    ))
  })
  out <- cbind(out, do.call(rbind, alt))
  if (!is.null(tau)) endpoint_require_feasible(out, tau, stability)
  out
}
