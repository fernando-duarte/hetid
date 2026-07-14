# Combined log-variance estimator panels: the PPML panel (theta, the
# exponential-mean QMLE map) and the benchmark log-OLS panel (theta^log,
# re-rendered from the stored benchmark cells), in the mechanical editorial
# order from logvar_panel_order -- the benchmark crossing count at the fixed
# baseline tau = 0.05 puts the finite-hull PPML panel first when positive.
# Flipping order never recomputes a number. Every panel and notes block is
# wrapped in stable LaTeX comment markers at creation so later estimator
# plans can append and gate on block-extracted diffs. Writes
# log_var_eq_panels.tex + compiled standalone; the primary log_var_eq.tex is
# the PPML-only publication table. Run via run_all.R after log_var_eq_table.R.

source("scripts/utils/latex_table_utils.R")
source("scripts/utils/latex_simple_table.R")
source("scripts-paper/log_var_eq_table_utils.R")
source("scripts-paper/log_var_eq_ppml_notes.R")

panels_order <- logvar_panel_order(log_var_eq$n_cross, set_id_mean_eq$tau_display)
panels_n_base <- log_var_eq$n_cross[[which(set_id_mean_eq$tau_display == 0.05)]]
panels_n_obs <- log_var_eq$sample$n
# the headline (first) panel's caption carries the editorial-ordering
# disclosure; ordering never touches the panel numbers
ordering_note <- function(est) {
  if (panels_order[1] != est) {
    return("")
  }
  sprintf(
    paste(
      " Panel order is editorial, keyed to the benchmark crossing count",
      "(%d) at $\\tau{=}0.05$, not a selection between estimators."
    ),
    panels_n_base
  )
}
panels_headers <- c(
  "OLS", "$\\tau{=}0$",
  sprintf("$\\tau{=}%.2g$", set_id_mean_eq$tau_display)
)

# wrap a built panel fragment in its markers and splice the marker-wrapped
# notes block (one \item per note line) before \end{threeparttable}
panel_block <- function(fragment, notes_lines, est) {
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

# the PPML panel: reference and Lewbel-point columns plus the per-tau hulls,
# t-statistic slots blank by construction, R^2 not defined for QMLE
ppml_parts <- logvar_ppml_table_parts(
  log_var_eq_ppml, set_id_mean_eq$tau_display, n_pc_r
)
stopifnot(
  identical(ppml_parts$n_obs, panels_n_obs),
  identical(ppml_parts$headers, panels_headers)
)
ppml_fragment <- build_simple_latex_table(
  ppml_parts$rows, ppml_parts$columns,
  col_headers = panels_headers,
  caption = paste0(
    paste(
      "PPML panel: $\\theta$, the exponential conditional-variance",
      "map over the identified news sets."
    ),
    ordering_note("ppml")
  ),
  label = "tab:log_var_eq_panel_ppml",
  fontsize = "\\footnotesize\\setlength{\\tabcolsep}{3pt}",
  rule_after = 2L
)

# the log-OLS panel: the benchmark cells as stored (Newey-West t statistics
# recomputed from the stored fit object, matching the benchmark table)
lo_tab <- log_var_eq$table
lo_nw_se <- sqrt(diag(sandwich::NeweyWest(
  log_var_eq$fit_ols,
  lag = 4, prewhite = FALSE
)))[lo_tab$coef]
stopifnot(!anyNA(lo_nw_se))
lo_nw_t <- lo_tab$ols / lo_nw_se
lo_nw_p <- 2 * stats::pt(-abs(lo_nw_t), df = stats::df.residual(log_var_eq$fit_ols))
lo_stars <- ifelse(
  lo_nw_p < 0.01, "^{***}",
  ifelse(lo_nw_p < 0.05, "^{**}", ifelse(lo_nw_p < 0.10, "^{*}", ""))
)
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
      sprintf("%.2f", lo_r2), sprintf("%d", panels_n_obs)
    ),
    c(interleave(fmt(lo_tab$point), ""), "--", sprintf("%d", panels_n_obs))
  ),
  unname(lapply(log_var_eq$sets, function(st) {
    stopifnot(identical(st$coef, lo_tab$coef))
    c(
      interleave(set_cell(st$set_lower, st$set_upper, st$status), ""),
      "--", sprintf("%d", panels_n_obs)
    )
  }))
)
lo_fragment <- build_simple_latex_table(
  lo_rows, lo_cols,
  col_headers = c(
    "OLS", "$\\tau{=}0$",
    sprintf("$\\tau{=}%.2g$", set_id_mean_eq$tau_display)
  ),
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

panels_blocks <- list(
  ppml = panel_block(
    ppml_fragment,
    build_ppml_panel_notes(
      log_var_eq_ppml, set_id_mean_eq$tau_baseline,
      logvar_ppml_grid_cap, logvar_ppml_fit_budget
    ),
    "ppml"
  ),
  logols = panel_block(
    lo_fragment,
    build_logols_panel_notes(set_id_mean_eq$tau_baseline, panels_n_base),
    "logols"
  )
)
panels_lines <- unlist(panels_blocks[panels_order], use.names = FALSE)
# the Harvey robustness panel appends after the ordered pair and never
# influences the pair's mechanical order (the panel module sources its own
# notes builder)
if (exists("log_var_eq_harvey")) {
  source("scripts-paper/log_var_eq_harvey_panel.R")
  panels_lines <- logvar_harvey_append_panel(
    panels_lines, log_var_eq_harvey, panels_n_obs,
    set_id_mean_eq$tau_display, set_id_mean_eq$tau_baseline,
    logvar_harvey_grid_cap, logvar_harvey_fit_budget
  )
}
write_latex_table(panels_lines, out_dir, "log_var_eq_panels")
compile_latex_pdf(file.path(out_dir, "log_var_eq_panels_standalone.tex"))

cat(sprintf(
  "log-variance panels: %s first (n_cross[0.05] = %d); wrote log_var_eq_panels.tex\n",
  panels_order[1], panels_n_base
))

rm(
  panels_order, panels_n_base, panels_n_obs, panels_headers, ordering_note,
  panel_block, ppml_parts, ppml_fragment, lo_tab, lo_nw_se, lo_nw_t, lo_nw_p,
  lo_stars, lo_cells,
  lo_labels, lo_rows, lo_r2, lo_cols, lo_fragment, panels_blocks,
  panels_lines, build_ppml_notes, build_ppml_table_notes,
  build_ppml_panel_notes, build_logols_panel_notes
)
