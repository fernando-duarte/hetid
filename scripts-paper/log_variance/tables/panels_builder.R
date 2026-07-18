# Shared builder for the combined log-variance estimator panels: the PPML +
# log-OLS ordered pair (mechanical order from logvar_panel_order) plus the
# appended Harvey robustness panel. Consumed by BOTH the conservative table
# (render_panels.R, no envelope) and the inference variant
# (render_inference_panels.R, bootstrap envelope frames + set-boot
# notes). Every panel/notes block is wrapped in stable LaTeX comment markers.
# Parameterizing envelope/notes/caption/out_name here is what stops the two
# published tables from drifting. Definitions only; run via run_pipeline.R.

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "simple_table.R"))
paper_source_once(paper_path("log_variance", "tables", "table_formatting.R"))
paper_source_once(paper_path("log_variance", "tables", "ppml_captions.R"))
paper_source_once(paper_path("log_variance", "tables", "harvey_panel.R"))

# the log-OLS benchmark panel fragment: the stored mean-log cells with
# Newey-West t-statistics recomputed from the stored fit. Point
# identified, so no envelope ever applies; identical in both panel files, kept
# here so its label stays valid across the manuscript's \input swap.
logvar_logols_fragment <- function(headers, n_obs, ordering_note) {
  tab <- log_var_eq$table
  nw <- paper_newey_west_statistics(
    log_var_eq$fit_ols,
    tab$ols,
    tab$coef,
    PAPER_REPORTING_CONTROL$logvar_logols
  )
  stopifnot(!anyNA(nw$se))
  cells <- ifelse(
    nw$stars == "", fmt(tab$ols),
    sprintf("%s$%s$", fmt(tab$ols), nw$stars)
  )
  labels <- c(
    "$\\theta^{log}_0$", sprintf("$\\theta^{log}_{%d,R}$", seq_len(n_pc_r))
  )
  rows <- c(interleave(labels, ""), "$R^2$", "$N$")
  r2 <- summary(log_var_eq$fit_ols)$r.squared
  cols <- c(
    list(
      c(
        interleave(cells, sprintf("(%.2f)", nw$statistic)),
        sprintf("%.2f", r2), sprintf("%d", n_obs)
      ),
      c(interleave(fmt(tab$point), ""), "--", sprintf("%d", n_obs))
    ),
    unname(lapply(log_var_eq$sets, function(st) {
      stopifnot(identical(st$coef, tab$coef))
      c(
        interleave(set_cell(st$set_lower, st$set_upper, st$status), ""),
        "--", sprintf("%d", n_obs)
      )
    }))
  )
  build_simple_latex_table(
    rows, cols,
    col_headers = headers,
    caption = paste0(
      paste(
        "log-OLS panel: $\\theta^{log}$, the benchmark mean-log map",
        "(fragile robustness benchmark)."
      ),
      ordering_note("logols")
    ),
    label = "tab:log_var_eq_panel_logols",
    fontsize = "\\footnotesize\\setlength{\\tabcolsep}{3pt}",
    rule_after = 2L
  )
}

# Build and write the combined panels. out_name names the .tex; the PPML caption
# suffix, the PPML/Harvey envelope frames, and the appended set-boot notes are
# the only things that differ between the conservative and inference variants.
build_logvar_panels <- function(out_name, ppml_caption_suffix = ".",
                                envelope_ppml = NULL, envelope_harvey = NULL,
                                extra_notes = NULL) {
  baseline_tau <- log_var_eq$tau_baseline
  order <- logvar_panel_order(
    log_var_eq$n_cross,
    set_id_mean_eq$tau_display,
    baseline_tau
  )
  n_base <- log_var_eq$n_cross[[
    which(set_id_mean_eq$tau_display == baseline_tau)
  ]]
  n_obs <- log_var_eq$sample$n
  headers <- c(
    "OLS", "$\\tau{=}0$", sprintf("$\\tau{=}%.2g$", set_id_mean_eq$tau_display)
  )
  ordering_note <- function(est) {
    if (order[1] != est) {
      return("")
    }
    sprintf(
      paste(
        " Panel order is editorial, keyed to the benchmark crossing count",
        "(%d) at $\\tau{=}%s$, not a selection between estimators."
      ),
      n_base,
      format(baseline_tau)
    )
  }
  ppml_parts <- logvar_ppml_table_parts(
    log_var_eq_ppml, set_id_mean_eq$tau_display, n_pc_r,
    se_type = logvar_ppml_se_type, envelope = envelope_ppml
  )
  stopifnot(
    identical(ppml_parts$n_obs, n_obs),
    identical(ppml_parts$headers, headers)
  )
  ppml_fragment <- build_simple_latex_table(
    ppml_parts$rows, ppml_parts$columns,
    col_headers = headers,
    caption = paste0(
      paste(
        "PPML panel: $\\theta$, the exponential conditional-variance map over",
        "the identified news sets"
      ),
      ppml_caption_suffix, ordering_note("ppml")
    ),
    label = "tab:log_var_eq_panel_ppml",
    fontsize = "\\footnotesize\\setlength{\\tabcolsep}{3pt}",
    rule_after = 2L
  )
  blocks <- list(
    ppml = logvar_panel_block(
      ppml_fragment,
      c(
        build_ppml_panel_notes(
          log_var_eq_ppml, set_id_mean_eq$tau_baseline,
          logvar_ppml_grid_cap, logvar_ppml_fit_budget,
          se_type = logvar_ppml_se_type,
          se_hac_lags = logvar_ppml_se_hac_lags,
          set_endpoint_inference = !is.null(envelope_ppml)
        ),
        extra_notes
      ),
      "ppml"
    ),
    logols = logvar_panel_block(
      logvar_logols_fragment(headers, n_obs, ordering_note),
      build_logols_panel_notes(set_id_mean_eq$tau_baseline, n_base),
      "logols"
    )
  )
  lines <- unlist(blocks[order], use.names = FALSE)
  if (exists("log_var_eq_harvey")) {
    harvey_fragment <- logvar_harvey_build_fragment(
      log_var_eq_harvey, n_obs, set_id_mean_eq$tau_display,
      se_type = logvar_harvey_se_type, envelope = envelope_harvey
    )
    harvey_notes <- c(
      build_harvey_panel_notes(
        log_var_eq_harvey, set_id_mean_eq$tau_baseline,
        logvar_harvey_grid_cap, logvar_harvey_fit_budget,
        se_type = logvar_harvey_se_type,
        se_hac_lags = logvar_harvey_se_hac_lags,
        set_endpoint_inference = !is.null(envelope_harvey)
      ),
      extra_notes
    )
    lines <- c(lines, logvar_panel_block(harvey_fragment, harvey_notes, "harvey"))
  }
  table_id <- artifact_id(paste0(out_name, ".tex"))
  standalone_id <- artifact_id(paste0(out_name, "_standalone.tex"))
  write_latex_table(
    lines, artifact_dir(table_id),
    tools::file_path_sans_ext(artifact_basename(table_id))
  )
  compile_latex_pdf(artifact_path(standalone_id))
  cat(sprintf(
    "log-variance panels%s: %s first (n_cross[%s] = %d); wrote %s.tex\n",
    if (is.null(envelope_ppml)) "" else " (inference)",
    order[1],
    format(baseline_tau),
    n_base,
    out_name
  ))
}
