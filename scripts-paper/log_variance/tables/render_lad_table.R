# Standalone median (LAD) panel table: the theta^0.5 estimator panel rendered as
# its own table and compiled PDF, so the conditional-median map has a tabular
# surfacing alongside its bounds-by-tau figure without touching main's combined
# build_logvar_panels output (that table stays PPML + log-OLS + Harvey, byte for
# byte). Guarded on log_var_eq_lad, so it renders only when the quantreg-gated LAD
# map ran; a declined, unanswered, or absent gate leaves nothing to render. The
# panel fragment, notes, and formatters are the same ones the combined table would
# have appended, reused verbatim. Run via run_pipeline.R after run_sets.R.

lad_result <- paper_logvar_result("lad", required = FALSE)
if (!is.null(lad_result)) {
  paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
  paper_source_once(paper_path("support", "latex", "simple_table.R"))
  paper_source_once(paper_path("log_variance", "tables", "table_formatting.R"))
  paper_source_once(paper_path("log_variance", "tables", "lad_panel_builder.R"))

  # appending the median block to an empty line set returns exactly the marker-
  # wrapped theta^0.5 panel with its notes, ready to stand alone
  lad_panel_lines <- logvar_lad_append_panel(
    character(0), lad_result, lad_result$sample$n,
    set_id_mean_eq$tau_display, set_id_mean_eq$tau_baseline,
    LOGVAR_LAD_CONTROL$grid_cap,
    LOGVAR_LAD_CONTROL$fit_budget
  )
  publish_latex_artifact(
    "log_variance_lad_table",
    lad_panel_lines
  )

  cat(sprintf(
    "LAD median panel table: wrote log_var_eq_lad_panel.tex (theta^0.5, N = %d)\n",
    lad_result$sample$n
  ))

  rm(lad_panel_lines)
}
