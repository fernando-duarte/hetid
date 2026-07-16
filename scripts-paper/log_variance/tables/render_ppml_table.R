# Publication table for the PPML log-variance equation estimated in
# run_sets.R: the quasi-Poisson log-link reference, the PPML map at
# the closed-form Lewbel point (tau = 0), and its identified hull at every
# tau_display slack. Writes the table fragment, standalone source, and compiled
# PDF to the typed table directory after the PPML set map is complete.

source(repo_path("scripts", "utils", "latex_table_utils.R"))
source(repo_path("scripts", "utils", "latex_simple_table.R"))
source(paper_path("log_variance", "tables", "table_formatting.R"))
source(paper_path("log_variance", "tables", "ppml_captions.R"))

parts <- logvar_ppml_table_parts(
  log_var_eq_ppml, set_id_mean_eq$tau_display, n_pc_r,
  se_type = logvar_ppml_se_type
)
coef_tab <- parts$table
set_tables <- parts$sets
n_obs <- parts$n_obs
baseline_table <- set_tables[[sprintf("%.17g", set_id_mean_eq$tau_baseline)]]
stopifnot(
  !is.null(baseline_table),
  identical(log_var_eq_ppml$sample_id, log_var_eq$sample_id),
  identical(n_obs, log_var_eq$sample$n),
  identical(coef_tab$set_lower, baseline_table$set_lower),
  identical(coef_tab$set_upper, baseline_table$set_upper),
  identical(coef_tab$status, baseline_table$status)
)

# Data-derived caption: bounded theta_R hulls excluding zero at the baseline
# slack, or the fail-closed disclosure when the baseline image is not fully
# bounded and reliable.
theta_r_rows <- coef_tab$coef != "(Intercept)"
n_excl <- sum(
  coef_tab$status[theta_r_rows] == "bounded" &
    (coef_tab$set_lower[theta_r_rows] > 0 | coef_tab$set_upper[theta_r_rows] < 0)
)
caption <- if (all(coef_tab$status == "bounded")) {
  sprintf(
    paste0(
      "Quasi-Poisson PPML identified sets for the log-variance equation: ",
      "%d of %d components of $\\theta_R$ have computed ranges ",
      "excluding zero at $\\tau{=}%.2g$."
    ),
    n_excl, n_pc_r, set_id_mean_eq$tau_baseline
  )
} else {
  sprintf(
    paste0(
      "Quasi-Poisson PPML identified sets for the log-variance equation: ",
      "at $\\tau{=}%.2g$ the computed image is not reliable on every side."
    ),
    set_id_mean_eq$tau_baseline
  )
}

logvar_latex <- build_simple_latex_table(
  parts$rows, parts$columns,
  col_headers = parts$headers,
  caption = caption, label = "tab:log_var_eq_set_id",
  notes = build_ppml_table_notes(
    log_var_eq_ppml, set_id_mean_eq$tau_baseline,
    logvar_ppml_grid_cap, logvar_ppml_fit_budget,
    se_type = logvar_ppml_se_type, se_hac_lags = logvar_ppml_se_hac_lags
  ),
  fontsize = "\\footnotesize\\setlength{\\tabcolsep}{3pt}",
  rule_after = 2L
)
write_latex_table(
  logvar_latex,
  artifact_dir("log_variance_ppml_table"),
  tools::file_path_sans_ext(artifact_basename("log_variance_ppml_table"))
)

# Compile the standalone variant so a LaTeX regression fails the pipeline.
compile_latex_pdf(artifact_path("log_variance_ppml_standalone_tex"))

cat(sprintf(
  "PPML log-variance table: %d of %d theta_R sets exclude zero at tau = %.2g\n",
  n_excl, n_pc_r, set_id_mean_eq$tau_baseline
))

# The shared formatters and panel-notes builder remain available to the
# combined table sourced next in run_pipeline.R.
rm(
  parts, coef_tab, set_tables, n_obs, baseline_table, theta_r_rows, n_excl,
  caption, build_ppml_table_notes, logvar_latex
)
