# Mode-dependent inference note for the structural-equation table.

structural_inference_note <- function(with_ci, inference_labels) {
  if (!with_ci) {
    return(c(
      "Confidence statements for the $\\tau{>}0$ sets await a coverage",
      "validation study; nominal interval diagnostics are reported in the",
      "companion inference table and set\\_id\\_inference\\_diagnostics.csv."
    ))
  }
  c(
    sprintf(
      paste(
        "Parenthesized intervals beneath the set cells are nominal",
        "%s\\%%"
      ),
      paper_format_general(
        inference_labels$coverage_percent,
        PAPER_REPORTING_CONTROL$precision$caption_percent
      )
    ),
    "Stoye (2009) intervals for the true coefficient: the exact set",
    "endpoints padded by robust (median-absolute-deviation) bootstrap",
    "endpoint standard errors, with the critical value calibrated",
    "against the joint normal distribution of the endpoint estimators",
    "at the correlation estimated from the draws (it coincides with the",
    "Imbens--Manski (2004) interpolation at the estimated correlations",
    "while not requiring the Imbens--Manski superefficient-width",
    "assumption).",
    "These are diagnostics under maintained regular endpoint asymptotics,",
    "componentwise rather than a joint confidence region, and",
    "conditional on the estimated SDF panels, their principal",
    "components, and the realized instrument, all constructed once from",
    sprintf(
      paste(
        "the full sample. An interval row is omitted when fewer than %s\\%%",
        "of the draws produce a certified bounded set at that $\\tau$ or the"
      ),
      paper_format_general(
        inference_labels$minimum_valid_draw_percent,
        PAPER_REPORTING_CONTROL$precision$caption_percent
      )
    ),
    "endpoint scale is degenerate; per-cell draw counts and omission",
    "reasons are in set\\_id\\_inference\\_diagnostics.csv."
  )
}
