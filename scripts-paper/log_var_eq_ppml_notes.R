# Notes builders for the combined log-variance estimator panels
# (log_var_eq_ppml_table.R): one marker-wrapped block per estimator, each
# note its own line so later estimator plans can append and diff blocks.
# The PPML block states the estimand, normalization, comparability, zero and
# existence handling, search-resolution disclosure, the editorial ordering
# rule, and the hull honesty clauses; the log-OLS block defers to the
# benchmark table's own notes and states the panel's fragile-benchmark role.
# Definitions only; sourced by log_var_eq_ppml_table.R.

build_ppml_panel_notes <- function(ppml, tau_baseline, grid_cap, fit_budget) {
  scale_val <- ppml$estimator$metadata$response_scale_value
  meta <- ppml$coverage_audit$meta
  c(
    paste(
      "PPML models $E[\\varepsilon^2 \\mid PC_R]$, not",
      "$E[\\log \\varepsilon^2 \\mid PC_R]$; the reported coefficients are",
      "quasi-Poisson (log link) QMLE under the exponential conditional-mean",
      "restriction."
    ),
    paste(
      "The intercept $\\theta^{var}_0$ is on the log conditional-variance",
      "scale and absorbs $2\\log|m_0|$; it is not comparable to the",
      "mean-log intercept $\\theta^{log}_0$."
    ),
    paste(
      "Slopes are comparable across the two panels only under an",
      "$R$-invariant innovation shape; the intercepts then differ by the",
      "Jensen gap $\\log E[e^{\\xi}]$ ($\\approx 1.270$ under normality)."
    ),
    paste(
      "Zero squared residuals are admissible responses;",
      "separation/existence is checked through the positive-response rank",
      "and conditioning gates, and unresolved fits fail closed."
    ),
    sprintf(
      paste(
        "Search resolution: primary grid capped at %d points with a %d-fit",
        "budget per slack; an independent Morton-ordered coverage audit",
        "(cap %d, budget %d) reruns every display slack before any cell",
        "ships, and disagreements demote cells to unreliable. The scaling",
        "pilot froze the response scale at %g."
      ),
      grid_cap, fit_budget, meta$grid_cap, meta$fit_budget, scale_val
    ),
    sprintf(
      paste(
        "Panel order is an editorial rule keyed to the benchmark crossing",
        "count at $\\tau{=}%.2g$, not a selection between estimators of one",
        "parameter."
      ),
      tau_baseline
    ),
    paste(
      "Set cells are projection hulls of an estimated plug-in image:",
      "certified feasible attained values, inner approximations; interior",
      "attainment is not established."
    ),
    "No PPML standard errors are reported (deferred)."
  )
}

build_logols_panel_notes <- function(tau_baseline, n_cross_base) {
  role <- if (n_cross_base > 0) {
    sprintf(
      paste(
        "Residual-zero crossings inside the identified news sets make",
        "log-scale sides diverge (%d crossings at $\\tau{=}%.2g$), so this",
        "panel is the fragile robustness benchmark."
      ),
      n_cross_base, tau_baseline
    )
  } else {
    sprintf(
      "No residual-zero crossing at $\\tau{=}%.2g$: the log benchmark leads.",
      tau_baseline
    )
  }
  c(
    paste(
      "The log-OLS panel re-renders the benchmark table's cells (Newey-West",
      "$t$ statistics in parentheses); see the benchmark log-variance table",
      "for its full notes."
    ),
    role
  )
}
