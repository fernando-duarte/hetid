# Inference variant of the combined log-variance estimator panels
# (log_var_eq_ppml_table.R): the PPML and Harvey panels gain a bootstrap outer
# confidence-envelope row beneath each set cell (log_var_eq_set_boot, from
# log_var_eq_set_bootstrap.R); the log-OLS panel is point-identified and has no
# tau > 0 set columns, so it carries no envelope and is re-rendered unchanged.
# Shares the CONSERVATIVE panels' per-panel labels so every \ref stays valid
# whichever of the two files the manuscript \input's. Writes
# log_var_eq_panels_inference.tex + compiled standalone. Run via run_all.R
# after log_var_eq_set_bootstrap.R.

source("scripts/utils/latex_table_utils.R")
source("scripts/utils/latex_simple_table.R")
source("scripts-paper/log_var_eq_table_utils.R")
source("scripts-paper/log_var_eq_ppml_notes.R")
source("scripts-paper/log_var_eq_harvey_panel.R")
source("scripts-paper/log_var_eq_set_inference_notes.R")

infp_order <- logvar_panel_order(log_var_eq$n_cross, set_id_mean_eq$tau_display)
infp_n_base <- log_var_eq$n_cross[[which(set_id_mean_eq$tau_display == 0.05)]]
infp_n_obs <- log_var_eq$sample$n
infp_headers <- c(
  "OLS", "$\\tau{=}0$",
  sprintf("$\\tau{=}%.2g$", set_id_mean_eq$tau_display)
)
# same editorial-ordering disclosure as the conservative panels (mirrored, not
# recomputed differently): only the headline panel's caption carries it
infp_ordering_note <- function(est) {
  if (infp_order[1] != est) {
    return("")
  }
  sprintf(
    paste(
      " Panel order is editorial, keyed to the benchmark crossing count",
      "(%d) at $\\tau{=}0.05$, not a selection between estimators."
    ),
    infp_n_base
  )
}

# wrap a built panel fragment in its markers and splice the marker-wrapped
# notes block, exactly as log_var_eq_ppml_table.R's local panel_block
infp_block <- function(fragment, notes_lines, est) {
  cut <- match("\\end{threeparttable}", fragment)
  stopifnot(!is.na(cut))
  notes_block <- c(
    sprintf("%% BEGIN LOGVAR NOTES %s", est),
    "\\begin{tablenotes}[flushleft]",
    "\\scriptsize",
    paste0("\\item ", notes_lines),
    "\\end{tablenotes}",
    sprintf("%% END LOGVAR NOTES %s", est)
  )
  c(
    sprintf("%% BEGIN LOGVAR PANEL %s", est),
    fragment[seq_len(cut - 1L)], notes_block, fragment[cut:length(fragment)],
    sprintf("%% END LOGVAR PANEL %s", est)
  )
}

# the PPML panel, with the bootstrap envelope threaded beneath each set cell
infp_ppml_parts <- logvar_ppml_table_parts(
  log_var_eq_ppml, set_id_mean_eq$tau_display, n_pc_r,
  se_type = logvar_ppml_se_type, envelope = log_var_eq_set_boot$ppml
)
stopifnot(
  identical(infp_ppml_parts$n_obs, infp_n_obs),
  identical(infp_ppml_parts$headers, infp_headers)
)
infp_ppml_fragment <- build_simple_latex_table(
  infp_ppml_parts$rows, infp_ppml_parts$columns,
  col_headers = infp_headers,
  caption = paste0(
    paste(
      "PPML panel: $\\theta$, the exponential conditional-variance map over",
      "the identified news sets, with a bootstrap outer confidence envelope",
      "beneath each set cell."
    ),
    infp_ordering_note("ppml")
  ),
  label = "tab:log_var_eq_panel_ppml",
  fontsize = "\\footnotesize\\setlength{\\tabcolsep}{3pt}",
  rule_after = 2L
)

# the log-OLS panel: unchanged from the conservative table (point-identified,
# no envelope applies), re-rendered here so its label stays valid across the
# manuscript's \input swap between the two panel files
lo_tab <- log_var_eq$table
lo_nw_se <- sqrt(diag(sandwich::NeweyWest(
  log_var_eq$fit_ols,
  lag = 4, prewhite = FALSE
)))[lo_tab$coef]
stopifnot(!anyNA(lo_nw_se))
lo_nw_t <- lo_tab$ols / lo_nw_se
lo_nw_p <- 2 * stats::pt(-abs(lo_nw_t), df = stats::df.residual(log_var_eq$fit_ols))
lo_stars <- sig_stars(lo_nw_p)
lo_cells <- ifelse(
  lo_stars == "", fmt(lo_tab$ols),
  sprintf("%s$%s$", fmt(lo_tab$ols), lo_stars)
)
lo_labels <- c(
  "$\\theta^{log}_0$", sprintf("$\\theta^{log}_{%d,R}$", seq_len(n_pc_r))
)
lo_rows <- c(interleave(lo_labels, ""), "$R^2$", "$N$")
lo_r2 <- summary(log_var_eq$fit_ols)$r.squared
lo_cols <- c(
  list(
    c(
      interleave(lo_cells, sprintf("(%.2f)", lo_nw_t)),
      sprintf("%.2f", lo_r2), sprintf("%d", infp_n_obs)
    ),
    c(interleave(fmt(lo_tab$point), ""), "--", sprintf("%d", infp_n_obs))
  ),
  unname(lapply(log_var_eq$sets, function(st) {
    stopifnot(identical(st$coef, lo_tab$coef))
    c(
      interleave(set_cell(st$set_lower, st$set_upper, st$status), ""),
      "--", sprintf("%d", infp_n_obs)
    )
  }))
)
lo_fragment <- build_simple_latex_table(
  lo_rows, lo_cols,
  col_headers = infp_headers,
  caption = paste0(
    paste(
      "log-OLS panel: $\\theta^{log}$, the benchmark mean-log map (fragile",
      "robustness benchmark)."
    ),
    infp_ordering_note("logols")
  ),
  label = "tab:log_var_eq_panel_logols",
  fontsize = "\\footnotesize\\setlength{\\tabcolsep}{3pt}",
  rule_after = 2L
)

infp_set_boot_notes <- build_logvar_set_inference_notes(log_var_eq_set_boot)
infp_blocks <- list(
  ppml = infp_block(
    infp_ppml_fragment,
    c(
      build_ppml_panel_notes(
        log_var_eq_ppml, set_id_mean_eq$tau_baseline,
        logvar_ppml_grid_cap, logvar_ppml_fit_budget,
        se_type = logvar_ppml_se_type, se_hac_lags = logvar_ppml_se_hac_lags
      ),
      infp_set_boot_notes
    ),
    "ppml"
  ),
  logols = infp_block(
    lo_fragment,
    build_logols_panel_notes(set_id_mean_eq$tau_baseline, infp_n_base),
    "logols"
  )
)
infp_lines <- unlist(infp_blocks[infp_order], use.names = FALSE)
# the Harvey panel appends after the ordered pair, exactly as the conservative
# table, with the same envelope threading as the PPML panel above
if (exists("log_var_eq_harvey")) {
  infp_harvey_fragment <- logvar_harvey_build_fragment(
    log_var_eq_harvey, infp_n_obs, set_id_mean_eq$tau_display,
    se_type = logvar_harvey_se_type, envelope = log_var_eq_set_boot$harvey
  )
  infp_lines <- c(infp_lines, infp_block(
    infp_harvey_fragment,
    c(
      build_harvey_panel_notes(
        log_var_eq_harvey, set_id_mean_eq$tau_baseline,
        logvar_harvey_grid_cap, logvar_harvey_fit_budget,
        se_type = logvar_harvey_se_type, se_hac_lags = logvar_harvey_se_hac_lags
      ),
      infp_set_boot_notes
    ),
    "harvey"
  ))
}
write_latex_table(infp_lines, out_dir, "log_var_eq_panels_inference")
compile_latex_pdf(file.path(out_dir, "log_var_eq_panels_inference_standalone.tex"))

cat(sprintf(
  paste(
    "log-variance panels (inference): %s first (n_cross[0.05] = %d);",
    "wrote log_var_eq_panels_inference.tex\n"
  ),
  infp_order[1], infp_n_base
))

rm(list = intersect(ls(), c(
  "infp_order", "infp_n_base", "infp_n_obs", "infp_headers", "infp_ordering_note",
  "infp_block", "infp_ppml_parts", "infp_ppml_fragment", "lo_tab", "lo_nw_se",
  "lo_nw_t", "lo_nw_p", "lo_stars", "lo_cells", "lo_labels", "lo_rows", "lo_r2",
  "lo_cols", "lo_fragment", "infp_set_boot_notes", "infp_blocks", "infp_lines",
  "infp_harvey_fragment", "build_logvar_set_inference_notes", "build_ppml_notes",
  "build_ppml_table_notes", "build_ppml_panel_notes", "build_logols_panel_notes"
)))
