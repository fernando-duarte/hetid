# The standard-error clause of the regularized log-projection pages: names the
# printed variant, what the plug-in variance holds fixed for each method, and
# the shared point-column caveat. Mirrors logvar_ppml_se_note's shape.
# Definitions only; sourced by log_projection_panel_notes.R.

paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path("log_variance", "tables", "ppml_table_parts.R"))
paper_source_once(paper_path(
  "log_variance", "estimators", "log_projection", "standard_errors.R"
))

logvar_log_projection_se_note <- function(method, se_type, se_hac_lags) {
  key <- match.arg(se_type, LOGVAR_LOG_PROJECTION_SE_TYPES)
  default_name <- switch(key,
    naive = "the homoskedastic least-squares variance $\\hat\\sigma^2(V'V)^{-1}$",
    hc0 = "the Eicker--White HC0 sandwich",
    hc1 = "the Eicker--White HC1 sandwich",
    hac = sprintf("the Newey--West HAC sandwich (Bartlett, %d lags)", se_hac_lags)
  )
  held <- if (identical(method, "log_plus")) {
    "the threshold $h_T$ at its estimate"
  } else {
    paste(
      "$c_T$, the candidate scale, and the first-pass adjustment profile at",
      "their estimates (the profile is fitted on the same rows, and that",
      "estimation error is not propagated)"
    )
  }
  c(
    sprintf(
      paste(
        "Standard errors for the reference and $\\tau{=}0$ point columns are",
        "plug-in least-squares variances of the transformed squared residual on",
        "$V_t = (1, PC_{R,t}')'$ at a fixed $b_N$, holding %s. Four variants",
        "are computed: the homoskedastic $\\hat\\sigma^2(V'V)^{-1}$, the",
        "Eicker--White sandwich (HC0, and HC1 with the $n/(n{-}p)$ factor), and",
        "its Newey--West Bartlett HAC extension over %d lags. Parenthetical",
        "values in the reference column are $\\hat\\theta/\\mathrm{SE}$ from %s,",
        "with stars from the standard-normal approximation (%s)."
      ),
      held, se_hac_lags, default_name,
      paper_significance_legend("ascending_percent")
    ),
    logvar_se_note_caveat(TRUE)
  )
}
