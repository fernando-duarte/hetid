# The marker-wrapped console diagnostics block for the median (LAD) log-variance
# driver, wrapped in the literal [BEGIN LOGVAR LAD] / [END LOGVAR LAD] markers so
# the pipeline console-regression gate can scope it. Per tau it prints the attained
# hulls, the phase counters, the witness/path coverage and crossing fragility, the
# tail classifications with the divergent M-slope metrics, the gate and
# nonuniqueness outcome, and the closure count. Reads only the assembled result
# list. Definitions only; sourced by the driver.

paper_source_once(paper_path(
  "log_variance", "tables", "console_formatting.R"
))

logvar_lad_console_block <- function(lad, taus) {
  cat("[BEGIN LOGVAR LAD]\n")
  cat(sprintf(
    "LAD median log-variance map: N = %d over %s to %s\n",
    lad$sample$n, format(lad$sample$span[1]), format(lad$sample$span[2])
  ))
  cat(sprintf(
    "  quantreg %s; guard ratio %.0e; e_scale_ref = %.4g\n",
    lad$quantreg_version, lad$guard$ratio, lad$guard$e_scale_ref
  ))
  cat(paste(
    "  cells are single-pass attained inner approximations; the engine only",
    "extends endpoints outward\n"
  ))
  keys <- names(lad$sets)
  for (i in seq_along(keys)) {
    k <- keys[i]
    tb <- lad$sets[[k]]
    d <- lad$counts[[k]]
    wc <- lad$witness_coverage[[k]]
    au <- lad$sensitivity_audit[[k]]
    pr <- lad$tail_classifications[[k]]
    hull <- logvar_hull_text(tb)
    cat(sprintf(
      "  tau = %s: %s\n",
      paper_format_general(
        taus[[i]],
        PAPER_REPORTING_CONTROL$precision$tau_significant
      ),
      paste(hull, collapse = " ")
    ))
    cat(sprintf(
      paste0(
        "    phase: scan %d probe %d nonunique %d polish %d ",
        "cold %d cache %d | eval %d fail %d\n"
      ),
      d$counters[[LOGVAR_ENGINE_PHASES[["scan"]]]],
      d$counters[[LOGVAR_ENGINE_PHASES[["probe"]]]],
      au$nonunique$n_probed,
      d$counters[[LOGVAR_ENGINE_PHASES[["polish"]]]],
      d$counters[[LOGVAR_ENGINE_PHASES[["cold_start"]]]],
      d$counters[[LOGVAR_ENGINE_PHASES[["cache_hit"]]]],
      d$n_evaluated, d$n_failed
    ))
    cat(sprintf(
      "    coverage: %d witnesses, %d flagged, %d unresolved, %d paths; min|eps| %s\n",
      wc$n_witnesses, wc$n_flagged, wc$n_unresolved, wc$n_paths,
      paper_format_general(
        lad$min_feasible_abs_eps[[i]],
        PAPER_REPORTING_CONTROL$precision$console_significant
      )
    ))
    if (is.null(pr) || nrow(pr) == 0L) {
      cat("    tail: no verified crossing paths\n")
    } else {
      cnt <- table(pr$status)
      cat(sprintf(
        "    tail: %s\n",
        paste(sprintf("%s=%d", names(cnt), as.integer(cnt)), collapse = " ")
      ))
      div <- pr[pr$status == "persistent_divergent_evidence", , drop = FALSE]
      for (r in seq_len(nrow(div))) {
        cat(sprintf(
          "      unbounded %s %s: M-slope %.4g over span %.4g (row %d, side %s)\n",
          div$coef[r], div$endpoint[r], div$tail_slope[r], div$m_span[r],
          div$crossing_row[r], div$residual_side[r]
        ))
      }
    }
    nd <- au$nonunique
    cat(sprintf(
      "    gate: %d better re-run, %d sides demoted (ran %s); nonunique %s%s\n",
      au$n_better, length(au$sensitive_sides), au$gate_ran,
      if (isTRUE(nd$tau_unreliable)) "TAU UNRELIABLE" else "clean",
      if (isTRUE(nd$truncated)) " [cap-truncated]" else ""
    ))
    ncl <- if (is.null(lad$closure_diagnostics[[k]])) 0L else nrow(lad$closure_diagnostics[[k]])
    cat(sprintf("    closure: %d stable one-sided limits\n", ncl))
  }
  cat("[END LOGVAR LAD]\n")
  invisible(NULL)
}
