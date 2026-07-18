# Publication table for the computed variance shares: one
# row per block and per component of
# Delta c_{t+1} = b_0 + PC_{E,t}' b_E + PC_{N,t+1}' b_N + eps_{t+1},
# with the share at the OLS coefficients, at the closed-form Lewbel point at
# tau = 0, and its range over the joint identified set at each tau_display
# slack.
# The fragment, standalone source, and compiled PDF share a typed table folder.

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "simple_table.R"))
paper_source_once(paper_path("mean_equation", "variance_shares", "variance_share_caption.R"))

fmt <- function(x) ifelse(is.na(x), "--", sprintf("%.2f", x))
# largest-remainder (Hamilton) rounding for the fixed-coefficient columns:
# the component shares add up to their block share exactly in the data, so
# the displayed component cells are re-rounded to sum exactly to the
# displayed block cell, each moving at most one 0.01 step from its plain
# rounding (v = c(block, components))
round_preserving_sum <- function(v, digits = 2L) {
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
  ifelse(
    status != "bounded", status,
    ifelse(
      abs(hi - lo) <=
        PAPER_ANALYSIS_CONTRACT$variance_share$render_degenerate_rtol *
          (1 + abs(hi)),
      "",
      sprintf("$[%.2f,\\,%.2f]$", lo, hi)
    )
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

# data-derived caption: the baseline news-block share range. State a numeric
# range only when the baseline joint set is certified bounded; an unbounded or
# fail-closed (unreliable) set has no range to report, and printing one would
# assert a finding the solve never established -- range_cell already defers to
# status, and the headline must agree
base_rng <- var_share$set_cols[[1]]
base_status <- base_rng$status[var_share$news_row]
caption <- if (identical(base_status, "bounded")) {
  sprintf(
    paste0(
      "SDF news accounts for %.1f--%.1f\\%% of the variance of consumption ",
      "growth at $\\tau{=}%.2g$."
    ),
    base_rng$lo[var_share$news_row], base_rng$hi[var_share$news_row],
    set_id_mean_eq$tau_baseline
  )
} else {
  sprintf(
    "The SDF-news share of consumption-growth variance at $\\tau{=}%.2g$ is %s.",
    set_id_mean_eq$tau_baseline, base_status
  )
}

var_share_table <- build_simple_latex_table(
  row_labels, unname(columns),
  col_headers = c(
    "OLS", "$\\tau{=}0$",
    sprintf("$\\tau{=}%.2g$", set_id_mean_eq$tau_display)
  ),
  caption = caption, label = "tab:var_share",
  notes = build_var_share_notes(sd_c = var_share$sd_c),
  fontsize = "\\footnotesize\\setlength{\\tabcolsep}{4pt}",
  rule_after = c(1L + n_pc, 2L * (1L + n_pc)),
  spanners = list(list(
    label = "Share of $\\widehat{\\mathrm{Var}}(\\Delta c_{t+1})$ (\\%)",
    n = length(columns)
  ))
)
var_share_dir <- artifact_dir("variance_share_table")
var_share_stem <- tools::file_path_sans_ext(
  artifact_basename("variance_share_table")
)
stopifnot(
  identical(var_share_dir, artifact_dir("variance_share_standalone_tex")),
  identical(var_share_dir, artifact_dir("variance_share_standalone_pdf"))
)
write_latex_table(var_share_table, var_share_dir, var_share_stem)
compile_latex_pdf(artifact_path("variance_share_standalone_tex"))

cat(
  "variance-share table written to",
  artifact_path("variance_share_table"),
  "\n"
)

rm(
  fmt, round_preserving_sum, align_blocks, range_cell, n_obs, row_labels,
  columns, base_rng, base_status, caption, var_share_table, var_share_dir,
  var_share_stem, build_var_share_notes
)
