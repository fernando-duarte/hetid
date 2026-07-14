# Publication table for the Harvey Gaussian multiplicative-variance MLE/QMLE.
# Reuses the same row and cell renderer as the Harvey block in
# log_var_eq_panels.tex, with a dedicated caption, label, and notes context.
# Run through log_var_eq_harvey_run.R after the identified news-set map.

source("scripts/utils/latex_table_utils.R")
source("scripts/utils/latex_simple_table.R")
source("scripts-paper/log_var_eq_table_utils.R")
source("scripts-paper/log_var_eq_harvey_panel.R")

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
  label = "tab:log_var_eq_harvey", include_ordering = FALSE
)
stopifnot(
  sum(harvey_table_lines == "% BEGIN LOGVAR PANEL harvey") == 1L,
  sum(harvey_table_lines == "% END LOGVAR PANEL harvey") == 1L
)

write_latex_table(harvey_table_lines, out_dir, "log_var_eq_harvey")
compile_latex_pdf(file.path(out_dir, "log_var_eq_harvey_standalone.tex"))

cat("Harvey log-variance table: wrote log_var_eq_harvey.tex\n")

rm(harvey_table_lines)
