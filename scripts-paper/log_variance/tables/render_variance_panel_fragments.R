# The Harvey and mean-log variance equations published on their own, without the
# mean panel above them. Single-panel fragments that open straight into the
# column header, in the layout of the combined pages; the paper supplies the
# float, caption, and label, as it does for every published fragment.
#
# Split out of render_estimator_pages.R, which owns the two-panel pages.

paper_source_once(paper_path("support", "latex", "overleaf_panel_table.R"))

# A single-panel table carries no R-squared row for an estimator that has none:
# in the combined pages the mean panel above supplies a real one and a row of
# dashes lines up under it, but here there is nothing to line up with.
logvar_variance_panel_fragment <- function(parts, headers, coef_rules,
                                           set_label, drop_r2 = FALSE) {
  if (isTRUE(drop_r2)) {
    parts <- paper_overleaf_drop_r2(parts)
  }
  paper_overleaf_panel_table(
    list(paper_overleaf_panel_lines(
      NULL, parts$rows, parts$columns, headers, set_label,
      blocks = coef_rules[[1L]],
      tail_after = coef_rules[[2L]]
    )),
    length(parts$columns)
  )
}

# harvey_parts is NULL exactly when the Harvey estimator did not run; its
# artifact is required, so a missing fragment fails the manifest check rather
# than passing quietly.
logvar_publish_variance_panel_fragments <- function(harvey_parts, logols_parts,
                                                    headers, coef_rules) {
  if (!is.null(harvey_parts)) {
    publish_latex_artifact(
      "log_var_eq_harvey_inference_table",
      logvar_variance_panel_fragment(
        harvey_parts, headers, coef_rules,
        PAPER_OVERLEAF_SET_LABEL,
        drop_r2 = TRUE
      )
    )
  }
  publish_latex_artifact(
    "log_var_eq_logols_inference_table",
    logvar_variance_panel_fragment(
      logols_parts, headers, coef_rules, PAPER_OVERLEAF_SET_LABEL_BARE
    )
  )
}
