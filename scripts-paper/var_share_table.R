# Publication table for the variance shares computed in var_share.R: one
# row per block and per component of
# Delta c_{t+1} = b_0 + PC_{E,t}' b_E + PC_{N,t+1}' b_N + eps_{t+1},
# with the share at the OLS coefficients, at the closed-form Lewbel point at
# tau = 0, and its range over the joint identified set at each tau_display
# slack.
# Writes var_share.tex, the standalone variant, and its compiled PDF to
# scripts-paper/output/.
# Run via run_all.R after var_share.R.

source("scripts/utils/latex_table_utils.R")
source("scripts/utils/latex_simple_table.R")
source("scripts-paper/var_share_notes.R")

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
  for (block_row in c(1L, n_pc + 2L)) {
    idx <- c(block_row, block_row + seq_len(n_pc))
    if (all(is.finite(v[idx]))) {
      v[idx] <- round_preserving_sum(v[idx])
    }
  }
  v
}
# a degenerate range (point-identified quantity) is left blank, matching the
# structural-equation table's blank-cell convention; a non-finite box marks
# an unbounded set
range_cell <- function(lo, hi) {
  ifelse(
    !is.finite(lo) | !is.finite(hi), "unbounded",
    ifelse(
      abs(hi - lo) <= 1e-9 * (1 + abs(hi)), "",
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
    \(cc) c(range_cell(cc$lo, cc$hi), sprintf("%d", n_obs))
  )
)

# data-derived caption: the baseline news-block share range
base_rng <- var_share$set_cols[[1]]
caption <- sprintf(
  paste0(
    "SDF news accounts for %.1f--%.1f\\%% of the variance of consumption ",
    "growth at $\\tau{=}%.2g$."
  ),
  base_rng$lo[var_share$news_row], base_rng$hi[var_share$news_row],
  set_id_mean_eq$tau_baseline
)

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
write_latex_table(var_share_table, out_dir, "var_share")
compile_latex_pdf(file.path(out_dir, "var_share_standalone.tex"))

cat("variance-share table written to", file.path(out_dir, "var_share.tex"), "\n")

rm(
  fmt, round_preserving_sum, align_blocks, range_cell, n_obs, row_labels,
  columns, base_rng, caption, var_share_table, build_var_share_notes
)
