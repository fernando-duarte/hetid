# Notes clause for the _inference variant of the combined log-variance panels
# (log_var_eq_set_inference_table.R): the moving-block bootstrap outer
# confidence-envelope disclosure appended after each estimator's own panel
# notes (log_var_eq_ppml_notes.R / log_var_eq_harvey_notes.R, both unchanged).
# Definitions only; reads log_var_eq_set_boot (log_var_eq_set_bootstrap.R) and
# the run_all.R constant logvar_boot_stability at call time.

build_logvar_set_inference_notes <- function(boot) {
  c(
    sprintf(
      paste(
        "Parenthetical rows beneath the $\\tau{>}0$ set cells are a 90\\%%",
        "moving-block bootstrap ($B=%d$ replications, %d-quarter blocks)",
        "OUTER confidence envelope covering the entire population identified",
        "interval, conditional on the delivered principal-component series;",
        "coordinatewise intervals do not describe the joint geometry of the",
        "identified set."
      ),
      boot$b_reps, boot$block
    ),
    paste(
      "The envelope is a centered one-sided max-root construction: each live",
      "endpoint is studentized by a robust (median-absolute-deviation) scale",
      "of its bootstrap draws, and the critical value is the conservative",
      "Politis--Romano--Wolf order statistic of the studentized root over the",
      "resampled draws, not a normal-quantile approximation."
    ),
    sprintf(
      paste(
        "A regularity gate blanks an endpoint when fewer than a data-driven",
        "minimum of draws certify a bounded set, when the bounded fraction",
        "across draws falls below %.0f\\%%, or when the endpoint scale is",
        "degenerate; divergent draws are always counted toward these",
        "fractions, never dropped from the draw pool."
      ),
      100 * logvar_boot_stability
    ),
    paste(
      "A genuinely one-sided identified set (one side certified unbounded at",
      "the full sample) keeps that side at infinity in the envelope; only the",
      "finite side is padded by the bootstrap critical value. Both sides",
      "expand outward from the identified set, never inward."
    ),
    paste(
      "The set cell above each envelope row is the plug-in identified set",
      "(the conservative table's cell, unchanged); the bootstrap centers on a",
      "resample-consistent anchor that equals the plug-in set where the set",
      "is bounded, so the envelope reads as padding around that same",
      "interval, not a second, independent estimate."
    )
  )
}
