# Deleted Panel A note generators, recovered for the memo

Recovered 2026-07-30 from `bdfd87e^` (the commit before "Emit bare tabular fragments for
three paper tables", 2026-07-20). These two generators were deleted when the four Panel A
.tex files became bare tabulars, so the prose below is what the **manuscript** now carries.
It is the authoritative statement of the current claims and the basis for the replacement
wording in the handoff memo. Nothing here is live code.

## structural_inference_note.R (40 lines) — the tau > 0 note

```r
# Inference note for the structural-equation table's Stoye interval rows.

structural_inference_note <- function(inference_labels) {
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
```

## structural_equation_caption.R, the tau = 0 and set-cell clauses (lines 145-185)

```r
    ),
    sprintf(
      "slack in %.0f\\%% of draws.",
      100 * set_id_boot$tau_star_share_bounded
    ),
    sprintf("$N=%d$, %s.", n_obs, span),
    "Set cells are exact identified-set ranges, not confidence intervals;",
    "a blank set cell marks a point-identified coefficient, whose set equals",
    "the $\\tau{=}0$ point at every displayed $\\tau$.",
    sprintf(
      "Parentheses beneath the $\\tau{=}0$ estimates are nominal %s\\%% intervals:",
      paper_format_general(
        inference_labels$coverage_percent,
        PAPER_REPORTING_CONTROL$precision$caption_percent
      )
    ),
    sprintf(
      paste(
        "the closed-form point plus or minus the one-sided %s\\%%",
        "normal quantile"
      ),
      paper_format_general(
        inference_labels$coverage_percent,
        PAPER_REPORTING_CONTROL$precision$caption_percent
      )
    ),
    "times a robust bootstrap standard error (median-absolute-deviation scale",
    "of the moving-block point draws); nominal under maintained regular",
    sprintf(
      paste(
        "asymptotics for the point estimator, and omitted when fewer than",
        "%s\\%% of the draws yield a full-rank $\\tau{=}0$ system."
      ),
      paper_format_general(
        inference_labels$minimum_valid_draw_percent,
        PAPER_REPORTING_CONTROL$precision$caption_percent
      )
    ),
    structural_inference_note(inference_labels)
  )
}
```
