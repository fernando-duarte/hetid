# Notes builder for the Harvey robustness panel appended to the combined
# log-variance estimator panels (log_var_eq_ppml_table.R). One marker-wrapped
# \item per note line so later diffs stay block-scoped, matching the PPML and
# log-OLS notes-builder voice. The block states the estimand, the intercept
# scale, the normality normalization identity, the zero-safe existence
# handling, the hull honesty and failed-fit clauses, the search-resolution
# disclosure and the deferred standard errors. Combined panels optionally add
# the ordering-independence rule (Harvey never drives the log-OLS/PPML headline
# swap). Definitions only; sourced by log_var_eq_harvey_panel.R.

# `harvey` mirrors build_ppml_panel_notes()'s object-first signature for a
# predictable call site; guarded here so notes are never emitted for a missing
# panel object. Dedicated tables omit the combined-panels ordering note.
build_harvey_panel_notes <- function(harvey, tau_baseline, grid_cap, fit_budget,
                                     include_ordering = TRUE) {
  stopifnot(is.list(harvey), !is.null(harvey$table))
  notes <- c(
    paste(
      "The Harvey panel is the Gaussian multiplicative-variance MLE/QMLE of",
      "$E[\\varepsilon^2 \\mid PC_R] = \\exp(R'\\theta^{H})$, fit on the fixed",
      "squared residuals $\\varepsilon^2(b_N)$ at each news vector $b_N$."
    ),
    paste(
      "The intercept $\\theta^{H}_0$ is on the log conditional-variance scale",
      "and absorbs $2\\log|m_0|$ under a rescaling of the residuals."
    ),
    paste(
      "Under conditional normality $\\theta^{H}_0 = \\theta^{log}_0 +",
      "1.270362845$; the general offset is $-E[\\log u^2]$ for a standardized",
      "innovation with an $R$-invariant shape."
    ),
    paste(
      "Zero squared residuals are handled by the direct parameter-dependent",
      "likelihood, not by adding a constant; existence is guarded by a",
      "recession-cone certificate, never by a benchmark residual crossing."
    ),
    paste(
      "Finite endpoints are certified feasible attained inner values --",
      "projection hulls of an estimated plug-in image, with interior",
      "attainment not established -- and failed fits make results unreliable."
    ),
    sprintf(
      paste(
        "Search resolution: the grid is capped at %d points with a %d-fit",
        "budget per slack, and a five-start sensitivity re-polish reruns every",
        "side before any cell ships; disagreements demote the cell to",
        "unreliable."
      ),
      grid_cap, fit_budget
    ),
    "No Harvey standard errors are reported (deferred)."
  )
  if (include_ordering) {
    notes <- c(notes, sprintf(
      paste(
        "The log-OLS/PPML panel order is governed by the benchmark crossing",
        "rule at $\\tau{=}%.2g$; the Harvey panel is a fixed robustness panel",
        "appended after that ordered pair and never influences it."
      ),
      tau_baseline
    ))
  }
  notes
}
