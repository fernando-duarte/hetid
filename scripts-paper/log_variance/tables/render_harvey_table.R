# Publication table for the Harvey Gaussian multiplicative-variance MLE/QMLE.
# Reuses the same row and cell renderer as the Harvey block in
# log_var_eq_panels.tex, with a dedicated caption, label, and notes context.
# Run through run.R after the identified news-set map.

source(repo_path("scripts", "utils", "latex_table_utils.R"))
source(repo_path("scripts", "utils", "latex_simple_table.R"))
source(paper_path("log_variance", "tables", "table_formatting.R"))
source(paper_path("log_variance", "tables", "harvey_panel.R"))

stopifnot(
  identical(log_var_eq_harvey$sample_id, log_var_eq$sample_id),
  identical(log_var_eq_harvey$sample$n, log_var_eq$sample$n)
)

harvey_table_lines <- logvar_harvey_append_panel(
  character(0), log_var_eq_harvey, log_var_eq_harvey$sample$n,
  set_id_mean_eq$tau_display, set_id_mean_eq$tau_baseline,
  logvar_harvey_grid_cap, logvar_harvey_fit_budget,
  caption = paste(
    "Harvey MLE/QMLE identified sets for the log-variance equation:",
    "$\\theta^{H}$, the Gaussian multiplicative-variance map."
  ),
  label = "tab:log_var_eq_harvey", include_ordering = FALSE,
  se_type = logvar_harvey_se_type, se_hac_lags = logvar_harvey_se_hac_lags
)
stopifnot(
  sum(harvey_table_lines == "% BEGIN LOGVAR PANEL harvey") == 1L,
  sum(harvey_table_lines == "% END LOGVAR PANEL harvey") == 1L
)

write_latex_table(
  harvey_table_lines,
  artifact_dir("log_variance_harvey_table"),
  tools::file_path_sans_ext(artifact_basename("log_variance_harvey_table"))
)
compile_latex_pdf(artifact_path("log_variance_harvey_standalone_tex"))

cat("Harvey log-variance table: wrote log_var_eq_harvey.tex\n")

rm(harvey_table_lines)
