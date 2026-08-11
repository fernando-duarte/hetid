# Combined inference table stacking the mean equation (Panel A) over the PPML
# log-variance equation (Panel B) under one shared OLS / tau column header:
#   Delta c_{t+1} = b_0 + PC_{E,t}' b_E + PC_{N,t+1}' b_N + eps_{t+1}   (Panel A)
#   E[eps_{t+1}^2 | PC_{R,t}] = exp(theta_0 + PC_{R,t}' theta_R)         (Panel B)
# Panel A reuses structural_equation_table_parts (mean-set estimate + endpoint
# bootstrap); Panel B reuses logvar_ppml_table_parts with the moving-block
# bootstrap outer envelope (log_var_eq_set_boot$ppml), exactly as the combined
# inference panels. Emits only the tabular in the paper's kern group; the paper
# supplies the float, caption, notes, and the dual \label. Writes
# structural_var_inference.tex + standalone. Run via run_pipeline.R after
# the estimator pages (needs set_id_mean_eq, set_id_boot, log_var_eq_set_boot).

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "overleaf_panel_table.R"))
paper_source_once(paper_path("mean_equation", "tables", "structural_table_parts.R"))
paper_source_once(paper_path("log_variance", "tables", "table_formatting.R"))

# Panel A: mean equation (keeps its R^2 and N tail).
panel_a <- structural_equation_table_parts(set_id_mean_eq, set_id_boot, n_pc)

# Panel B: PPML log-variance with the set-endpoint bootstrap envelope beneath
# each tau > 0 set cell. The R^2 row stays and renders as "--": PPML has no
# R-squared, and saying so costs one row of dashes. Excising the row instead
# meant locating it by label, asserting it appeared exactly once, and rebuilding
# every column around the surviving indices -- machinery that would have to be
# repeated for each estimator whose R-squared is absent, and that silently
# reshapes the row set the rule positions below are written against.
panel_b <- logvar_ppml_table_parts(
  paper_logvar_result("ppml"),
  set_id_mean_eq$tau_display,
  n_pc_r,
  se_type = logvar_ppml_se_type,
  envelope = log_var_eq_set_boot$ppml,
  point_stat = logvar_boot_point_stat(log_var_eq_set_boot, "ppml")
)
rows_b <- panel_b$rows
columns_b <- panel_b$columns

# Both panels share the identical OLS / tau column grid; a mismatch is a wiring
# bug, so fail loud rather than emit a misaligned header.
headers <- panel_a$headers
n_col <- length(headers)
stopifnot(
  identical(panel_b$headers, headers),
  length(panel_a$columns) == n_col,
  length(columns_b) == n_col,
  all(vapply(panel_a$columns, length, integer(1)) == length(panel_a$row_labels)),
  all(vapply(columns_b, length, integer(1)) == length(rows_b))
)

# Each panel repeats the column header under its own title, so a reader never
# has to carry the column meanings down from Panel A. Coefficient blocks are
# separated by open space and the R^2/N tail by a light rule: the intercept row
# stands alone, then the design block, then the news block.
combined_table <- paper_overleaf_panel_table(
  list(
    paper_overleaf_panel_lines(
      "Panel A: Mean equation",
      panel_a$row_labels, panel_a$columns, headers,
      PAPER_OVERLEAF_SET_LABEL,
      blocks = c(2L, panel_a$rule_after[[1L]]),
      tail_after = panel_a$rule_after[[2L]]
    ),
    paper_overleaf_panel_lines(
      "Panel B: Variance equation",
      rows_b, columns_b, headers,
      PAPER_OVERLEAF_SET_LABEL,
      blocks = 2L,
      tail_after = 2L * (1L + n_pc_r)
    )
  ),
  n_col
)
publish_latex_artifact("structural_var_inference_table", combined_table)

cat(sprintf(
  "combined inference table: Panel A (N = %d) over Panel B PPML (N = %d)\n",
  set_id_mean_eq$sample$n, panel_b$n_obs
))

rm(
  panel_a, panel_b, rows_b, columns_b, headers, n_col, combined_table
)
