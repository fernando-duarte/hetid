# Publication table for the computed variance shares: one
# row per block and per component of
# Delta c_{t+1} = b_0 + PC_{E,t}' b_E + PC_{N,t+1}' b_N + eps_{t+1},
# with the share at the OLS coefficients, at the closed-form Lewbel point at
# tau = 0, and its range over the joint identified set at each tau_display
# slack.
# The fragment, standalone source, and compiled PDF share a typed table folder.

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "simple_table.R"))
paper_source_once(paper_path("support", "reporting", "cells.R"))

fmt <- function(x) {
  policy <- PAPER_REPORTING_CONTROL$cells$variance_share
  paper_format_number(x, policy$digits, policy$numeric_missing)
}
# largest-remainder (Hamilton) rounding for the fixed-coefficient columns:
# the component shares add up to their block share exactly in the data, so
# the displayed component cells are re-rounded to sum exactly to the
# displayed block cell, each moving at most one 0.01 step from its plain
# rounding (v = c(block, components))
round_preserving_sum <- function(
  v,
  digits =
    PAPER_REPORTING_CONTROL$cells$variance_share$digits
) {
  scale <- 10^digits
  block <- round(v[1] * scale)
  base <- floor(v[-1] * scale)
  bump <- order(v[-1] * scale - base, decreasing = TRUE)
  bump <- bump[seq_len(block - sum(base))]
  base[bump] <- base[bump] + 1
  c(block, base) / scale
}
align_blocks <- function(v) {
  for (block_row in c(1L, var_share$news_row)) {
    idx <- c(block_row, block_row + seq_len(n_pc))
    if (all(is.finite(v[idx]))) {
      v[idx] <- round_preserving_sum(v[idx])
    }
  }
  v
}
# a degenerate range (point-identified quantity) is left blank, matching the
# structural-equation table's blank-cell convention; an uncertified row prints
# its status, like that table's set_cell. Status decides, not is.finite(): a
# fail-closed solve and a certified-unbounded set both arrive as a non-finite
# endpoint, and only "unbounded" is a claim about the share -- "unreliable"
# says the solve established nothing, which is neither a range nor a claim
range_cell <- function(lo, hi, status) {
  policy <- PAPER_REPORTING_CONTROL$cells$variance_share
  paper_format_set_interval(
    lo,
    hi,
    status,
    digits = policy$digits,
    status_mode = policy$status_mode,
    na_as_status = policy$na_as_status,
    infinite_bounds = policy$infinite_bounds,
    degenerate_rtol =
      PAPER_ANALYSIS_CONTRACT$variance_share$render_degenerate_rtol
  )
}

n_obs <- set_id_mean_eq$sample$n
row_labels <- c(
  "$PC_{E,t}^{T}b_{E}$", sprintf("\\quad $b_{%d,E}$", seq_len(n_pc)),
  "$PC_{N,t+1}^{T}b_{N}$", sprintf("\\quad $b_{%d,N}$", seq_len(n_pc)),
  "$N$"
)
columns <- c(
  list(
    c(fmt(align_blocks(var_share$ols)), sprintf("%d", n_obs)),
    c(fmt(align_blocks(var_share$point)), sprintf("%d", n_obs))
  ),
  lapply(
    var_share$set_cols,
    \(cc) c(range_cell(cc$lo, cc$hi, cc$status), sprintf("%d", n_obs))
  )
)

# Emit only the tabular, wrapped in a \begingroup that scopes the font size and
# column separation; the paper supplies the float, caption, and notes.
var_share_table <- c(
  "\\begingroup",
  PAPER_TABLE_STYLE$variance_share$fontsize,
  simple_tabular_lines(
    row_labels, unname(columns),
    col_headers = paper_tau_col_headers(set_id_mean_eq$tau_display),
    rule_after = c(1L + n_pc, 2L * (1L + n_pc)),
    spanners = list(list(
      label = "Share of $\\widehat{\\mathrm{Var}}(\\Delta c_{t+1})$ (\\%)",
      n = length(columns)
    ))
  ),
  "\\endgroup"
)
publish_latex_artifact("variance_share_table", var_share_table)

cat(
  "variance-share table written to",
  artifact_path("variance_share_table"),
  "\n"
)

rm(
  fmt, round_preserving_sum, align_blocks, range_cell, n_obs, row_labels,
  columns, var_share_table
)
