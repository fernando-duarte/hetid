# Notes builders for the primary PPML table and combined log-variance estimator
# panels. The shared PPML core states the estimand, column construction,
# normalization, comparability, zero and existence handling, search-resolution
# disclosure, and hull honesty clauses. Only the combined panel adds the
# editorial-ordering disclosure. Definitions only.

build_ppml_notes <- function(ppml, tau_baseline, grid_cap, fit_budget,
                             include_ordering, se_type = "hac",
                             se_hac_lags = 4L) {
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
    logvar_ppml_se_note(se_type, se_hac_lags)
  )
}

# the SE computation clause: describes each stored variant by the assumption it
# makes, names which variant is printed, and states the point-column
# conditioning caveat. The framing is neutral -- assumptions, not advice -- so it
# reads correctly whatever logvar_ppml_se_type selects.
logvar_ppml_se_note <- function(se_type, se_hac_lags) {
  key <- match.arg(se_type, LOGVAR_PPML_SE_TYPES)
  default_name <- switch(key,
    naive = "the model-based $\\hat\\varphi A^{-1}$",
    hc0 = "the Eicker--White HC0 sandwich",
    hc1 = "the Eicker--White HC1 sandwich",
    hac = sprintf("the Newey--West HAC sandwich (Bartlett, %d lags)", se_hac_lags)
  )
  c(
    sprintf(
      paste(
        "Standard errors for the OLS and $\\tau{=}0$ point columns are",
        "quasi-Poisson QMLE variances with $A = X'\\mathrm{diag}(\\hat\\mu)X$ and",
        "score residual $\\hat r = \\varepsilon^2 - \\hat\\mu$. Four variants are",
        "computed: the model-based $\\hat\\varphi A^{-1}$ ($\\hat\\varphi =",
        "(n{-}p)^{-1}\\sum_t \\hat r_t^2/\\hat\\mu_t$), valid only if",
        "$\\mathrm{Var}(\\varepsilon^2 \\mid PC_R) \\propto \\hat\\mu$; the",
        "heteroskedasticity-robust Eicker--White sandwich $A^{-1}(\\sum_t \\hat",
        "r_t^2 x_t x_t')A^{-1}$ (HC0, and HC1 with the $n/(n{-}p)$ factor); and",
        "its Newey--West Bartlett HAC extension over %d lags, consistent also",
        "under serially correlated scores. The reported statistics use %s;",
        "parenthetical values are $\\hat\\theta/\\mathrm{SE}$ with stars from the",
        # the LaTeX literal percent must be doubled in this sprintf FORMAT string:
        # `\\%%` emits `\%`, while a lone `%)` aborts sprintf
        "standard-normal (QMLE) approximation ($^{*}$/$^{**}$/$^{***}$ at 10/5/1\\%%)."
      ),
      se_hac_lags, default_name
    ),
    paste(
      "The $\\tau{=}0$ statistics condition on the plug-in Lewbel news vector",
      "$b_N$ and do not propagate its first-stage sampling error; $\\tau{>}0$",
      "set columns are identified-set ranges, not point estimates, so no",
      "standard error is attached (the moving-block bootstrap for set-endpoint",
      "uncertainty is deferred)."
    )
  )
}

build_ppml_table_notes <- function(ppml, tau_baseline, grid_cap, fit_budget,
                                   se_type = "hac", se_hac_lags = 4L) {
  build_ppml_notes(
    ppml, tau_baseline, grid_cap, fit_budget,
    include_ordering = FALSE, se_type = se_type, se_hac_lags = se_hac_lags
  )
}

build_ppml_panel_notes <- function(ppml, tau_baseline, grid_cap, fit_budget,
                                   se_type = "hac", se_hac_lags = 4L) {
  build_ppml_notes(
    ppml, tau_baseline, grid_cap, fit_budget,
    include_ordering = TRUE, se_type = se_type, se_hac_lags = se_hac_lags
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
