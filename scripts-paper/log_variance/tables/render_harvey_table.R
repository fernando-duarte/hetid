# Publication table for the Harvey Gaussian multiplicative-variance MLE/QMLE.
# Reuses the same row and cell renderer as the Harvey block in
# log_var_eq_panels.tex, with a dedicated caption, label, and notes context.
# Run through run.R after the identified news-set map.

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "simple_table.R"))
paper_source_once(paper_path("log_variance", "tables", "table_formatting.R"))
paper_source_once(paper_path("log_variance", "tables", "harvey_panel.R"))

harvey_result <- paper_logvar_result("harvey")
stopifnot(
  identical(harvey_result$sample_id, log_var_eq$sample_id),
  identical(harvey_result$sample$n, log_var_eq$sample$n)
)

harvey_table_lines <- logvar_harvey_append_panel(
  character(0), harvey_result, harvey_result$sample$n,
  set_id_mean_eq$tau_display, set_id_mean_eq$tau_baseline,
  logvar_harvey_grid_cap, logvar_harvey_fit_budget,
  caption = paste(
    "Harvey MLE/QMLE identified sets for the log-variance equation:",
    "$\\theta^{H}$, the Gaussian multiplicative-variance map."
  ),
  label = artifact_latex_label("log_variance_harvey_table"),
  include_ordering = FALSE,
  se_type = logvar_harvey_se_type, se_hac_lags = logvar_harvey_se_hac_lags
)
stopifnot(
  sum(harvey_table_lines == "% BEGIN LOGVAR PANEL harvey") == 1L,
  sum(harvey_table_lines == "% END LOGVAR PANEL harvey") == 1L
)

publish_latex_artifact(
  "log_variance_harvey_table",
  harvey_table_lines
)

cat("Harvey log-variance table: wrote log_var_eq_harvey.tex\n")

rm(harvey_table_lines)
