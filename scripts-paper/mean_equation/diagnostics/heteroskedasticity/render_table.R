# Render the heteroskedasticity diagnostics as a bare tabular fragment,
# standalone source, and compiled standalone PDF; the paper supplies the
# float, caption, and notes.

paper_source_once(paper_path(
  "mean_equation",
  "diagnostics",
  "heteroskedasticity",
  "compute_tests.R"
))

panel_rows <- function(idx) {
  data.frame(label = row_labels[idx], cells[idx, , drop = FALSE])
}
arch_row <- length(test_names)
panels <- list(
  "Null hypothesis of variance constant over time, against volatility clustering" =
    panel_rows(arch_row),
  "Null hypothesis of variance unrelated to $Z$, against $Z$-driven heteroskedasticity" =
    panel_rows(seq_len(arch_row - 1L)),
  "Relevance and endogeneity diagnostics" = panel_rows((arch_row + 1L):nrow(cells))
)
hetero_table <- panel_tabular_lines(
  panels,
  col_headers = as.character(seq_len(n_pc_tested)),
  col_group_label = "SDF-news PC"
)
publish_latex_artifact("heteroskedasticity_table", hetero_table)

cat(
  sprintf("hetero tests (Z = %s): regime", z_col),
  suite_cfg$regime, "suite,", n_obs, "obs\n",
  sprintf(
    "KP rk underidentification: stat = %s, p = %s (NW lag %d, sv sep %s)\n",
    paper_format_general(
      rk$stat,
      PAPER_REPORTING_CONTROL$precision$console_significant
    ),
    paper_format_general(
      rk$p,
      PAPER_REPORTING_CONTROL$precision$console_significant
    ),
    rk$lag,
    paper_format_general(
      rk$sep,
      PAPER_REPORTING_CONTROL$precision$tau_significant
    )
  )
)
print(
  do.call(cbind, pvals),
  digits =
    PAPER_REPORTING_CONTROL$precision$diagnostic_table
)

rm(
  w1, y1, y2, z, z_mat, fmt, pcell, suite_cfg, run_battery, pvals, test_labels,
  test_names, column_cells, cells, rk, joint_cells, row_labels,
  caption_tests, rejection_alpha, caption_p_values, reject, n_pc_tested,
  caption, n_obs, span, panel_rows, arch_row,
  panels, hetero_table
)
