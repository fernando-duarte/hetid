# Render the heteroskedasticity diagnostics as bare tabular fragments,
# standalone sources, and compiled standalone PDFs; the paper supplies the
# float, caption, and notes. Two tables of identical shape are produced, one
# conditioning on Y2 and one on W2.

paper_source_once(paper_path(
  "mean_equation",
  "diagnostics",
  "heteroskedasticity",
  "compute_tests.R"
))

hetero_render <- function(panel, artifact_id, col_group_label) {
  panel_rows <- function(idx) {
    data.frame(label = panel$row_labels[idx], panel$cells[idx, , drop = FALSE])
  }
  arch_row <- length(panel$test_names)
  panels <- list(
    "Null hypothesis of variance constant over time, against volatility clustering" =
      panel_rows(arch_row),
    "Null hypothesis of variance unrelated to $Z$, against $Z$-driven heteroskedasticity" =
      panel_rows(seq_len(arch_row - 1L)),
    "Relevance and endogeneity diagnostics" =
      panel_rows((arch_row + 1L):nrow(panel$cells))
  )
  tabular <- panel_tabular_lines(
    panels,
    col_headers = as.character(seq_len(panel$n_cols)),
    col_group_label = col_group_label
  )
  publish_latex_artifact(artifact_id, tabular)
  invisible(tabular)
}

hetero_render(panel_y2, "heteroskedasticity_table", "SDF-news PC")
hetero_render(panel_w2, "heteroskedasticity_w2_table", "SDF-news residual")

hetero_console <- function(panel, label) {
  cat(
    sprintf("hetero tests on %s (Z = %s): regime", label, z_col),
    panel$suite_cfg$regime, "suite,", n_obs, "obs\n",
    sprintf(
      "KP rk underidentification: stat = %s, p = %s (NW lag %d, sv sep %s)\n",
      paper_format_general(
        panel$rk$stat,
        PAPER_REPORTING_CONTROL$precision$console_significant
      ),
      paper_format_general(
        panel$rk$p,
        PAPER_REPORTING_CONTROL$precision$console_significant
      ),
      panel$rk$lag,
      paper_format_general(
        panel$rk$sep,
        PAPER_REPORTING_CONTROL$precision$tau_significant
      )
    )
  )
  print(
    do.call(cbind, panel$pvals),
    digits =
      PAPER_REPORTING_CONTROL$precision$diagnostic_table
  )
}

hetero_console(panel_y2, "Y2")
hetero_console(panel_w2, "W2")

# The two panels select their diagnostics suite independently, so a divergence
# is a real difference in design and must not pass unremarked.
if (!identical(panel_y2$suite_cfg$regime, panel_w2$suite_cfg$regime)) {
  cat(sprintf(
    "note: Y2 and W2 selected different diagnostics regimes (%s vs %s)\n",
    panel_y2$suite_cfg$regime, panel_w2$suite_cfg$regime
  ))
}

rm(
  w1, y1, y2, w2, z, z_mat, fmt, pcell, panel_y2, panel_w2,
  n_obs, span, hetero_render, hetero_console
)
