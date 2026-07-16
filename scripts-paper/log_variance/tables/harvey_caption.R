# Notes builder for the Harvey robustness panel appended to the combined
# log-variance estimator panels (render_panels.R). One marker-wrapped
# \item per note line so later diffs stay block-scoped, matching the PPML and
# log-OLS notes-builder voice. The block states the estimand, the intercept
# scale, the normality normalization identity, the zero-safe existence
# handling, the hull honesty and failed-fit clauses, the search-resolution
# disclosure and the standard-error computation. Combined panels optionally
# add the ordering-independence rule (Harvey never drives the log-OLS/PPML
# headline swap). Definitions only; sourced by harvey_panel.R.

# `harvey` mirrors build_ppml_panel_notes()'s object-first signature for a
# predictable call site; guarded here so notes are never emitted for a missing
# panel object. Dedicated tables omit the combined-panels ordering note.
# se_type NULL means genuinely no SEs are attached (mirrors the renderer's
# blank point columns), so the note stays the deferred line; a validated key
# describes the printed variant via logvar_harvey_se_note.
build_harvey_panel_notes <- function(harvey, tau_baseline, grid_cap, fit_budget,
                                     include_ordering = TRUE, se_type = "hac",
                                     se_hac_lags = 4L) {
  stopifnot(is.list(harvey), !is.null(harvey$table))
  se_note <- if (is.null(se_type)) {
    "No Harvey standard errors are reported (deferred)."
  } else {
    logvar_harvey_se_note(se_type, se_hac_lags)
  }
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
    se_note
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

# the SE computation clause: describes each stored variant by its assumption,
# names which variant is printed, and states the point-column conditioning
# caveat and the set-column no-SE rule. Neutral framing (assumptions, not
# advice) so it reads correctly whatever logvar_harvey_se_type selects.
logvar_harvey_se_note <- function(se_type, se_hac_lags) {
  key <- match.arg(se_type, LOGVAR_HARVEY_SE_TYPES)
  default_name <- switch(key,
    expected = "the Gaussian working-model Fisher information $(\\frac{1}{2}R'R)^{-1}$",
    observed = paste0(
      "the Gaussian working-model observed information ",
      "$(\\frac{1}{2}R'\\mathrm{diag}(\\hat r)R)^{-1}$"
    ),
    opg = "the outer-product-of-gradients (BHHH) variance",
    robust = "the Eicker--White QMLE sandwich",
    hac = sprintf("the Newey--West HAC sandwich (Bartlett, %d lags)", se_hac_lags)
  )
  c(
    sprintf(
      paste(
        "Standard errors for the reference and $\\tau{=}0$ point columns are",
        "Gaussian multiplicative-variance QMLE variances with $H =",
        "\\frac{1}{2}R'\\mathrm{diag}(\\hat r)R$, $R_t = (1, PC_{R,t}')'$ and",
        "$\\hat r_t = \\hat\\varepsilon_t^2 / \\exp(R_t'\\hat\\theta^{H})$. Five",
        "variants are computed: the working-model Fisher $(\\frac{1}{2}R'R)^{-1}$",
        "and observed $H^{-1}$ information (valid only if the Gaussian model",
        "holds); the outer-product BHHH; the Eicker--White QMLE sandwich $H^{-1}",
        "(\\sum_t \\hat g_t \\hat g_t')H^{-1}$ with $\\hat g_t = \\frac{1}{2}(1 -",
        "\\hat r_t)R_t$; and its Newey--West Bartlett HAC extension over %d lags,",
        "consistent also under serially correlated scores. The reported",
        "statistics use %s; parenthetical values are $\\hat\\theta/\\mathrm{SE}$",
        "with stars from the standard-normal (QMLE) approximation",
        "($^{*}$/$^{**}$/$^{***}$ at 10/5/1\\%%)."
      ),
      se_hac_lags, default_name
    ),
    logvar_se_note_caveat()
  )
}
