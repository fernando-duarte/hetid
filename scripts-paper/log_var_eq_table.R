# Publication table for the log-variance equation estimated in log_var_eq.R:
# one row per coefficient of
# log eps_{t+1}^2 = theta_0 + PC_{R,t}' theta_R + xi_{t+1},
# with the naive OLS benchmark (Newey-West t statistics), the two-step map at
# the closed-form Lewbel point (tau = 0), and the identified set at each
# tau_display slack (half-infinite ranges where a certified residual-zero
# crossing makes a side diverge). Writes log_var_eq.tex, the standalone
# variant, and its compiled PDF to scripts-paper/output/.
# Run via run_all.R after log_var_eq.R.

source("scripts/utils/latex_table_utils.R")
source("scripts/utils/latex_simple_table.R")
source("scripts-paper/log_var_eq_notes.R")

coef_tab <- log_var_eq$table
stopifnot(identical(
  coef_tab$coef, c("(Intercept)", paste0("l.pc", seq_len(n_pc_r)))
))

# NA and non-finite render "--" (a non-finite tau = 0 point means the Lewbel
# point sits on a residual-zero hyperplane; the driver's min_abs_eps_point
# diagnostic surfaces it)
fmt <- function(x) ifelse(!is.finite(x), "--", sprintf("%.3f", x))
# an unreliable or upstream-propagated (NA-endpoint) cell renders its status
# word; certified one-sided divergence renders a half-infinite range; a
# degenerate interval (point-identified) is left blank as in the structural
# table
set_cell <- function(lo, hi, status) {
  ifelse(
    status == "unreliable" | is.na(lo) | is.na(hi), status,
    ifelse(
      is.infinite(lo) & is.infinite(hi), "unbounded",
      ifelse(
        is.infinite(lo), sprintf("$(-\\infty,\\,%.3f]$", hi),
        ifelse(
          is.infinite(hi), sprintf("$[%.3f,\\,\\infty)$", lo),
          ifelse(lo == hi, "", sprintf("$[%.3f,\\,%.3f]$", lo, hi))
        )
      )
    )
  )
}

n_obs <- log_var_eq$sample$n
r2 <- summary(log_var_eq$fit_ols)$r.squared

# Newey-West (4 lags, Bartlett kernel, no prewhitening) t statistics and
# significance stars for the OLS column
nw_se <- sqrt(diag(sandwich::NeweyWest(
  log_var_eq$fit_ols,
  lag = 4, prewhite = FALSE
)))[coef_tab$coef]
stopifnot(!anyNA(nw_se))
nw_t <- coef_tab$ols / nw_se
nw_p <- 2 * stats::pt(-abs(nw_t), df = stats::df.residual(log_var_eq$fit_ols))
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
coef_labels <- c("$\\theta_0$", sprintf("$\\theta_{%d,R}$", seq_len(n_pc_r)))
row_labels <- c(interleave(coef_labels, ""), "$R^2$", "$N$")
set_columns <- lapply(log_var_eq$sets, function(st) {
  stopifnot(identical(st$coef, coef_tab$coef))
  c(
    interleave(set_cell(st$set_lower, st$set_upper, st$status), ""),
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

# data-derived caption: bounded theta_R sets excluding zero at the baseline
# slack, or the divergence disclosure when the baseline image is not fully
# bounded
theta_r_rows <- coef_tab$coef != "(Intercept)"
n_excl <- sum(
  coef_tab$status[theta_r_rows] == "bounded" &
    (coef_tab$set_lower[theta_r_rows] > 0 | coef_tab$set_upper[theta_r_rows] < 0)
)
caption <- if (all(coef_tab$status == "bounded")) {
  sprintf(
    paste0(
      "Identified sets for the log-variance equation: %d of %d components ",
      "of $\\theta_R$ have computed identified ranges excluding zero at ",
      "$\\tau{=}%.2g$."
    ),
    n_excl, n_pc_r, set_id_mean_eq$tau_baseline
  )
} else {
  sprintf(
    paste0(
      "Identified sets for the log-variance equation: at $\\tau{=}%.2g$ the ",
      "log benchmark's finite-sample range is not bounded on every side."
    ),
    set_id_mean_eq$tau_baseline
  )
}

# rule_after separates the intercept pair from the theta_R block; the value
# is position-invariant in n_pc_r (one coefficient row plus its t-stat row
# precede the block), unlike the structural table's 2L * (1L + n_pc)
logvar_latex <- build_simple_latex_table(
  row_labels, columns,
  col_headers = c(
    "OLS", "$\\tau{=}0$",
    sprintf("$\\tau{=}%.2g$", set_id_mean_eq$tau_display)
  ),
  caption = caption, label = "tab:log_var_eq_set_id",
  notes = build_logvar_notes(),
  fontsize = "\\footnotesize\\setlength{\\tabcolsep}{3pt}",
  rule_after = 2L
)
write_latex_table(logvar_latex, out_dir, "log_var_eq")

# compile the standalone variant so a LaTeX regression fails the pipeline
compile_latex_pdf(file.path(out_dir, "log_var_eq_standalone.tex"))

cat(
  sprintf(
    "log-variance table: %d of %d theta_R sets exclude zero at tau = %.2g\n",
    n_excl, n_pc_r, set_id_mean_eq$tau_baseline
  )
)

rm(
  coef_tab, fmt, set_cell, n_obs, r2, nw_se, nw_t, nw_p, nw_stars, ols_cells,
  ols_tstats, interleave, coef_labels, row_labels, set_columns, columns,
  theta_r_rows, n_excl, caption, build_logvar_notes, logvar_latex
)
