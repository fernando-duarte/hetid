# Notes builders for the primary PPML table and combined log-variance estimator
# panels. The shared PPML core states the estimand, column construction,
# normalization, comparability, zero and existence handling, search-resolution
# disclosure, and hull honesty clauses. Only the combined panel adds the
# editorial-ordering disclosure. Definitions only.

build_ppml_notes <- function(ppml, tau_baseline, grid_cap, fit_budget,
                             include_ordering) {
  scale_val <- ppml$estimator$metadata$response_scale_value
  meta <- ppml$coverage_audit$meta
  c(
    paste(
      "The estimated mean equation is",
      "$\\Delta c_{t+1}=b_{0}+PC_{E,t}^{T}b_{E}+",
      "PC_{N,t+1}^{T}b_{N}+\\varepsilon_{t+1}$, and the estimated",
      "variance equation is $\\varepsilon_{t+1}^{2}=",
      "\\exp(\\theta_{0}+PC_{R,t}^{T}\\theta_{R}+\\xi_{t+1})$."
    ),
    paste(
      "PPML models $E[\\varepsilon^2 \\mid PC_R]$, not",
      "$E[\\log \\varepsilon^2 \\mid PC_R]$; the reported coefficients are",
      "quasi-Poisson (log link) QMLE under the exponential conditional-mean",
      "restriction."
    ),
    paste(
      "The OLS column applies PPML to squared residuals from the",
      "exogenous-news OLS mean equation; $\\tau{=}0$ evaluates the PPML map",
      "at the Lewbel point, and each $\\tau{>}0$ cell ranges that map over",
      "the joint identified news set."
    ),
    paste(
      "The intercept $\\theta_0$ is on the log conditional-variance",
      "scale and absorbs $2\\log|m_0|$; it is not comparable to the",
      "mean-log intercept."
    ),
    paste(
      "PPML and mean-log slopes are comparable only under an $R$-invariant",
      "innovation shape; the intercepts then differ by the Jensen gap",
      "$\\log E[e^{\\xi}]$ ($\\approx 1.270$ under normality)."
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
    if (isTRUE(include_ordering)) {
      sprintf(
        paste(
          "Panel order is an editorial rule keyed to the benchmark crossing",
          "count at $\\tau{=}%.2g$, not a selection between estimators of one",
          "parameter."
        ),
        tau_baseline
      )
    } else {
      NULL
    },
    paste(
      "Set cells are projection hulls of an estimated plug-in image:",
      "certified feasible attained values, inner approximations; interior",
      "attainment is not established."
    ),
    "No PPML standard errors are reported (deferred)."
  )
}

build_ppml_table_notes <- function(ppml, tau_baseline, grid_cap, fit_budget) {
  build_ppml_notes(
    ppml, tau_baseline, grid_cap, fit_budget,
    include_ordering = FALSE
  )
}

build_ppml_panel_notes <- function(ppml, tau_baseline, grid_cap, fit_budget) {
  build_ppml_notes(
    ppml, tau_baseline, grid_cap, fit_budget,
    include_ordering = TRUE
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
      "The log-OLS panel reports the mean-log robustness benchmark; its OLS",
      "column has Newey-West $t$ statistics in parentheses, and its set cells",
      "map the mean-log estimator over the joint identified news sets."
    ),
    role
  )
}
