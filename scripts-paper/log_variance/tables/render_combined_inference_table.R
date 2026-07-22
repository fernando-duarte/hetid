# Combined inference table stacking the mean equation (Panel A) over the PPML
# log-variance equation (Panel B) under one shared OLS / tau column header:
#   Delta c_{t+1} = b_0 + PC_{E,t}' b_E + PC_{N,t+1}' b_N + eps_{t+1}   (Panel A)
#   E[eps_{t+1}^2 | PC_{R,t}] = exp(theta_0 + PC_{R,t}' theta_R)         (Panel B)
# Panel A reuses structural_equation_table_parts (mean-set estimate + endpoint
# bootstrap); Panel B reuses logvar_ppml_table_parts with the moving-block
# bootstrap outer envelope (log_var_eq_set_boot$ppml), exactly as the combined
# inference panels. Emits only the tabular in a \begingroup that scopes the font;
# the paper supplies the float, caption, notes, and the dual \label. Writes
# structural_var_inference.tex + standalone. Run via run_pipeline.R after
# render_inference_panels.R (needs set_id_mean_eq, set_id_boot, log_var_eq_set_boot).

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("mean_equation", "tables", "structural_table_parts.R"))
paper_source_once(paper_path("log_variance", "tables", "table_formatting.R"))

# Panel A: mean equation (keeps its R^2 and N tail).
panel_a <- structural_equation_table_parts(set_id_mean_eq, set_id_boot, n_pc)

# Panel B: PPML log-variance with the set-endpoint bootstrap envelope beneath
# each tau > 0 set cell, then drop the blank R^2 row (PPML has no R^2) without
# disturbing the N row that follows it.
panel_b <- logvar_ppml_table_parts(
  paper_logvar_result("ppml"),
  set_id_mean_eq$tau_display,
  n_pc_r,
  se_type = logvar_ppml_se_type,
  envelope = log_var_eq_set_boot$ppml
)
r2_row <- which(panel_b$rows == "$R^2$")
stopifnot(length(r2_row) == 1L)
keep_b <- setdiff(seq_along(panel_b$rows), r2_row)
rows_b <- panel_b$rows[keep_b]
columns_b <- lapply(panel_b$columns, function(col) col[keep_b])

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

# Serialize one panel's rows to LaTeX body lines, inserting a \midrule after each
# rule_after index; mirrors the body loop in simple_tabular_lines so the merged
# table renders identically to the standalone panels.
panel_body <- function(row_labels, columns, rule_after) {
  out <- character(0)
  for (i in seq_along(row_labels)) {
    cells <- vapply(columns, function(col) col[[i]], character(1))
    out <- c(
      out,
      paste0(row_labels[[i]], " & ", paste(cells, collapse = " & "), " \\\\")
    )
    if (i %in% rule_after) out <- c(out, "\\midrule")
  }
  out
}

panel_head <- function(title) {
  sprintf("\\multicolumn{%d}{l}{\\textit{%s}} \\\\", n_col + 1L, title)
}

combined_table <- c(
  "\\begingroup",
  PAPER_TABLE_STYLE$combined_inference$fontsize,
  paste0("\\begin{tabular}{l", strrep("c", n_col), "}"),
  "\\toprule",
  paste0(" & ", paste(headers, collapse = " & "), " \\\\"),
  "\\midrule",
  panel_head("Panel A. Mean equation"),
  "\\midrule",
  panel_body(panel_a$row_labels, panel_a$columns, panel_a$rule_after),
  "\\midrule",
  panel_head("Panel B. Log-variance equation (PPML)"),
  "\\midrule",
  panel_body(rows_b, columns_b, c(2L, 10L)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\endgroup"
)
publish_latex_artifact("structural_var_inference_table", combined_table)

cat(sprintf(
  "combined inference table: Panel A (N = %d) over Panel B PPML (N = %d)\n",
  set_id_mean_eq$sample$n, panel_b$n_obs
))

rm(
  panel_a, panel_b, r2_row, keep_b, rows_b, columns_b, headers, n_col,
  panel_body, panel_head, combined_table
)
