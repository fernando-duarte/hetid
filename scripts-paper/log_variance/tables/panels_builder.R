paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "simple_table.R"))
paper_source_once(paper_path("log_variance", "tables", "table_formatting.R"))
paper_source_once(paper_path("log_variance", "tables", "ppml_captions.R"))
paper_source_once(paper_path("log_variance", "tables", "harvey_panel.R"))
logvar_logols_fragment <- function(headers, n_obs, ordering_note, label) {
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
        interleave(
          cells,
          sprintf(
            "(%s)",
            paper_format_number(
              nw$statistic,
              PAPER_REPORTING_CONTROL$cells$statistic_digits,
              "na"
            )
          )
        ),
        paper_format_number(
          r2,
          PAPER_REPORTING_CONTROL$cells$statistic_digits,
          "na"
        ),
        sprintf("%d", n_obs)
      ),
      c(interleave(fmt(tab$point), ""), PAPER_NA_TOKEN, sprintf("%d", n_obs))
    ),
    unname(lapply(log_var_eq$sets, function(st) {
      stopifnot(identical(st$coef, tab$coef))
      c(
        interleave(set_cell(st$set_lower, st$set_upper, st$status), ""),
        PAPER_NA_TOKEN, sprintf("%d", n_obs)
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
    label = label,
    fontsize = PAPER_TABLE_STYLE$coefficient$fontsize,
    rule_after = PAPER_TABLE_STYLE$coefficient$row_stride
  )
}
build_logvar_panels <- function(table_id, ppml_caption_suffix = ".",
                                envelope_ppml = NULL, envelope_harvey = NULL,
                                extra_notes = NULL) {
  ppml_result <- paper_logvar_result("ppml")
  harvey_result <- paper_logvar_result("harvey", required = FALSE)
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
  headers <- paper_tau_col_headers(set_id_mean_eq$tau_display)
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
    ppml_result, set_id_mean_eq$tau_display, n_pc_r,
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
    label = artifact_latex_label(table_id, "ppml"),
    fontsize = PAPER_TABLE_STYLE$coefficient$fontsize,
    rule_after = PAPER_TABLE_STYLE$coefficient$row_stride
  )
  blocks <- list(
    ppml = logvar_panel_block(
      ppml_fragment,
      c(
        build_ppml_panel_notes(
          ppml_result, set_id_mean_eq$tau_baseline,
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
      logvar_logols_fragment(
        headers,
        n_obs,
        ordering_note,
        artifact_latex_label(table_id, "logols")
      ),
      build_logols_panel_notes(set_id_mean_eq$tau_baseline, n_base),
      "logols"
    )
  )
  lines <- unlist(blocks[order], use.names = FALSE)
  if (!is.null(harvey_result)) {
    harvey_fragment <- logvar_harvey_build_fragment(
      harvey_result, n_obs, set_id_mean_eq$tau_display,
      label = artifact_latex_label(table_id, "harvey"),
      se_type = logvar_harvey_se_type, envelope = envelope_harvey
    )
    harvey_notes <- c(
      build_harvey_panel_notes(
        harvey_result, set_id_mean_eq$tau_baseline,
        logvar_harvey_grid_cap, logvar_harvey_fit_budget,
        se_type = logvar_harvey_se_type,
        se_hac_lags = logvar_harvey_se_hac_lags,
        set_endpoint_inference = !is.null(envelope_harvey)
      ),
      extra_notes
    )
    lines <- c(lines, logvar_panel_block(harvey_fragment, harvey_notes, "harvey"))
  }
  published <- publish_latex_artifact(table_id, lines)
  cat(sprintf(
    "log-variance panels%s: %s first (n_cross[%s] = %d); wrote %s\n",
    if (is.null(envelope_ppml)) "" else " (inference)",
    order[1],
    format(baseline_tau),
    n_base,
    basename(published[["fragment_id"]])
  ))
}
