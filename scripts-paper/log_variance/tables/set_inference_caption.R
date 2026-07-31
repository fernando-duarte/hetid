# Notes clause for the _inference variant of the combined log-variance panels
# document: the moving-block bootstrap set-inference
# disclosure appended after each estimator's own panel notes (ppml_captions.R
# and harvey_caption.R). Definitions only; reads log_var_eq_set_boot (unified
# bootstrap stage) and the contract's stability share at call time.

paper_source_once(paper_path("support", "reporting", "cells.R"))

build_logvar_set_inference_notes <- function(boot) {
  inference_labels <- paper_inference_labels(boot$inference_contract)
  c(
    sprintf(
      paste(
        paste0(
          "Parenthetical rows beneath the $\\tau{>}0$ set cells are a ",
          "%s\\%%"
        ),
        "circular moving-block bootstrap ($B=%s$ replications, %d-quarter blocks)",
        "confidence interval for the coefficient, calibrated to cover it wherever",
        "the truth sits inside the identified interval, conditional on the",
        "delivered principal-component series; coordinatewise intervals do not",
        "describe the joint geometry of the identified set."
      ),
      paper_format_general(
        inference_labels$coverage_percent,
        PAPER_REPORTING_CONTROL$precision$caption_percent
      ),
      paper_format_thousands(boot$b_reps),
      boot$block
    ),
    paste(
      "Each live endpoint is studentized by a robust",
      "(median-absolute-deviation) scale of its bootstrap draws, and the",
      "critical value is the conservative Politis--Romano--Wolf order statistic",
      "of the inward studentized root over the resampled draws, not a",
      "normal-quantile approximation. The root is credited by the estimated set",
      "width at the position of the truth and then maximized over that",
      "position, because a truth away from an endpoint leaves that endpoint",
      "room to spare before it can fail."
    ),
    sprintf(
      paste(
        "One regularity gate, declared once in the analysis contract, governs",
        "every reported endpoint. An endpoint is blanked when fewer than",
        "%s\\%% of the replications certify a bounded set, when the certified",
        "share among the draws that did not fail falls below %.0f\\%%, or when",
        "the endpoint scale is degenerate. Unbounded and unreliable draws stay",
        "in that denominator; failed draws leave it but still count against the",
        "absolute minimum."
      ),
      paper_format_general(
        inference_labels$minimum_valid_draw_percent,
        PAPER_REPORTING_CONTROL$precision$caption_percent
      ),
      100 * PAPER_ANALYSIS_CONTRACT$inference$stability_share
    ),
    paste(
      "A genuinely one-sided identified set (one side certified unbounded at",
      "the full sample) keeps that side at infinity; only the finite side is",
      "padded by the bootstrap critical value. On a half-infinite set the worst",
      "position for the truth is the finite endpoint, so the width credit",
      "vanishes and the pointwise and whole-set critical values coincide there.",
      "Both sides expand outward from the identified set, never inward."
    ),
    paste(
      "The set cell above each parenthetical row is the plug-in identified set",
      "(the conservative table's cell, unchanged); the bootstrap centers on a",
      "resample-consistent anchor that equals the plug-in set where the set",
      "is bounded, so the reported row reads as padding around that same",
      "interval, not a second, independent estimate."
    ),
    paste(
      "The critical value for containment of the entire identified interval is",
      "never smaller and is still computed: it is reported per cell as",
      "\\texttt{c\\_s} in",
      "\\texttt{log\\_var\\_eq\\_set\\_inference\\_diagnostics.csv}."
    )
  )
}
