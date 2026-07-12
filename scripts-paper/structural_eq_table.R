# Publication table for the structural consumption-growth equation estimated
# in set_id_mean_eq.R: one row per coefficient of
# Delta c_{t+1} = b_0 + PC_{E,t}' b_E + PC_{N,t+1}' b_N + eps_{t+1},
# with the OLS benchmark (Newey-West t statistics), the closed-form Lewbel
# point at tau = 0, and the exact identified set at each tau_display slack.
# Writes structural_eq.tex, the standalone variant, and its compiled PDF to
# scripts-paper/output/.
# Run via run_all.R after set_id_mean_eq.R.

source("scripts/utils/latex_table_utils.R")
source("scripts/utils/latex_simple_table.R")
source("scripts-paper/structural_eq_notes.R")

# coefficient rows: design block (b_0, b_E) then the news block (b_N); the
# guard pins the row order the labels below assume
coef_tab <- rbind(set_id_mean_eq$beta1_table, set_id_mean_eq$theta_table)
stopifnot(identical(coef_tab$coef, c(
  "(Intercept)", paste0("lag_expected_sdf_pc", seq_len(n_pc)),
  paste0("sdf_news_pc", seq_len(n_pc))
)))

fmt <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))
# a degenerate interval (exactly equal endpoints, the point-identified case)
# is left blank: the set equals the tau = 0 point at every displayed tau
set_cell <- function(lo, hi, status) {
  ifelse(
    status != "bounded", status,
    ifelse(lo == hi, "", sprintf("$[%.3f,\\,%.3f]$", lo, hi))
  )
}

n_obs <- set_id_mean_eq$sample$n
r2 <- summary(set_id_mean_eq$ols_fit)$r.squared
tau_base <- set_id_mean_eq$tau_baseline

# Newey-West (4 lags, Bartlett kernel, no prewhitening) t statistics and
# significance stars for the OLS column
nw_se <- sqrt(diag(sandwich::NeweyWest(
  set_id_mean_eq$ols_fit,
  lag = 4, prewhite = FALSE
)))[coef_tab$coef]
nw_t <- coef_tab$ols / nw_se
nw_p <- 2 * stats::pt(-abs(nw_t), df = stats::df.residual(set_id_mean_eq$ols_fit))
nw_stars <- ifelse(
  nw_p < 0.01, "^{***}",
  ifelse(nw_p < 0.05, "^{**}", ifelse(nw_p < 0.10, "^{*}", ""))
)
ols_cells <- ifelse(
  nw_stars == "", fmt(coef_tab$ols),
  sprintf("%s$%s$", fmt(coef_tab$ols), nw_stars)
)
ols_tstats <- sprintf("(%.2f)", nw_t)

# coefficient rows interleaved with the OLS t-statistic rows (blank in the
# identification columns)
interleave <- function(a, b) as.vector(rbind(a, b))
coef_labels <- c(
  "$b_0$",
  sprintf("$b_{%d,E}$", seq_len(n_pc)),
  sprintf("$b_{%d,N}$", seq_len(n_pc))
)
row_labels <- c(interleave(coef_labels, ""), "$R^2$", "$N$")
# one identified-set column per display slack, from the stored per-tau tables
set_columns <- lapply(set_id_mean_eq$set_tables, function(st) {
  tab <- rbind(st$beta1, st$theta)
  stopifnot(identical(tab$coef, coef_tab$coef))
  c(
    interleave(set_cell(tab$set_lower, tab$set_upper, tab$status), ""),
    "--", sprintf("%d", n_obs)
  )
})
columns <- c(
  list(
    c(interleave(ols_cells, ols_tstats), sprintf("%.2f", r2), sprintf("%d", n_obs)),
    c(interleave(fmt(coef_tab$point), ""), "--", sprintf("%d", n_obs))
  ),
  unname(set_columns)
)

# data-derived caption: how many news-coefficient sets exclude zero
n_excl <- sum(with(
  set_id_mean_eq$theta_table,
  status == "bounded" & (set_lower > 0 | set_upper < 0)
))
caption <- sprintf(
  paste0(
    "Heteroskedasticity set-identifies the SDF-news coefficients: %d of %d ",
    "components of $b_N$ have identified sets excluding zero at $\\tau{=}%.2g$."
  ),
  n_excl, n_pc, tau_base
)

# caption-notes text (notation paragraph, estimation description, and the
# mode-dependent clauses) from structural_eq_notes.R
notes <- build_structural_notes()

structural_table <- build_simple_latex_table(
  row_labels, columns,
  col_headers = c(
    "OLS", "$\\tau{=}0$",
    sprintf("$\\tau{=}%.2g$", set_id_mean_eq$tau_display)
  ),
  caption = caption, label = "tab:structural_eq_set_id",
  notes = notes, fontsize = "\\footnotesize\\setlength{\\tabcolsep}{3pt}",
  rule_after = c(2L * (1L + n_pc), 2L * (1L + 2L * n_pc))
)
write_latex_table(structural_table, out_dir, "structural_eq")

# compile the standalone variant so a LaTeX regression fails the pipeline
compile_latex_pdf(file.path(out_dir, "structural_eq_standalone.tex"))

cat(
  sprintf("structural equation table: %d of %d b_N sets exclude zero", n_excl, n_pc),
  sprintf("at tau = %.2g\n", tau_base)
)

rm(
  coef_tab, fmt, set_cell, n_obs, r2, tau_base, row_labels, columns,
  set_columns, nw_se, nw_t, nw_p, nw_stars, ols_cells, ols_tstats, interleave,
  coef_labels, n_excl, caption, build_structural_notes, notes, structural_table
)
