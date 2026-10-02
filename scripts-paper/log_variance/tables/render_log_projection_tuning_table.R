# Appendix table: both regularized log projections at the three tuning
# multipliers, baseline-tau identified sets with the tau = 0 point value
# beside each. Rows are coefficients by method; column pairs are multipliers.
# Run via run_pipeline.R after render_estimator_pages.R.

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "simple_table.R"))
paper_source_once(paper_path("support", "latex", "table_environment.R"))
paper_source_once(paper_path("log_variance", "tables", "table_formatting.R"))
paper_source_once(paper_path("log_variance", "tables", "estimator_page_specs.R"))

logvar_log_projection_tuning_lines <- function(tuning, specs, coef_labels,
                                               multipliers, tau_baseline) {
  ids <- names(specs)
  row_labels <- unlist(lapply(ids, function(id) {
    c(
      specs[[id]]$intercept_label,
      sprintf(specs[[id]]$slope_template, seq_len(length(coef_labels) - 1L))
    )
  }))
  cell <- function(id, m, field) {
    rows <- tuning[tuning$id == id & tuning$multiplier == m, , drop = FALSE]
    rows <- rows[match(coef_labels, rows$coef), , drop = FALSE]
    logvar_assert_coef_aligned(rows$coef, coef_labels)
    if (field == "point") {
      return(logvar_fmt(rows$point))
    }
    set_cell(rows$set_lower, rows$set_upper, rows$status)
  }
  columns <- unlist(lapply(multipliers, function(m) {
    lapply(c("point", "set"), function(field) {
      unlist(lapply(ids, function(id) cell(id, m, field)))
    })
  }), recursive = FALSE)
  headers <- c(
    "$\\tau{=}0$",
    paste0("$\\tau{=}", paper_format_tau(tau_baseline), "$")
  )
  simple_tabular_lines(
    row_labels = row_labels,
    columns = columns,
    col_headers = rep(headers, length(multipliers)),
    spanners = lapply(multipliers, function(m) {
      list(label = sprintf("$m = %s$", format(m)), n = 2L)
    }),
    rule_after = length(coef_labels)
  )
}

if (exists("log_var_eq_log_projection_tuning")) {
  local({
    ids <- paper_logvar_estimator_ids(capability = "log_projection")
    specs <- lapply(stats::setNames(ids, ids), function(id) {
      logvar_estimator_page_spec(id)$panel_spec
    })
    publish_latex_artifact(
      "log_projection_tuning_table",
      latex_table_environment(
        tabular_lines = logvar_log_projection_tuning_lines(
          log_var_eq_log_projection_tuning, specs, log_var_eq$coefs,
          hetid::LOG_PROJECTION_CONTROL$SENSITIVITY_MULTIPLIERS,
          set_id_mean_eq$tau_baseline
        ),
        caption = paste(
          "Regularized log projections at three tuning multipliers:",
          "identified sets at the baseline slack and the $\\tau{=}0$ point."
        ),
        label = artifact_latex_label("log_projection_tuning_table"),
        notes = build_log_projection_tuning_notes(
          log_var_eq_log_projection_tuning, set_id_mean_eq$tau_baseline,
          log_var_eq_log_projection_endpoints
        ),
        fontsize = ""
      )
    )
  })
}
