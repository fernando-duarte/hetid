# Closure-diagnostics artifact for the median (LAD) log-variance map: the typed
# schema of the one-sided crossing-limit approximations and the flat CSV writer
# with its 17-significant-digit encoding and column-wise read-back guard. The
# closure rows are deliberately separate from the attained hull -- they are stable
# finite approximate limits at verified crossings, never attained cells -- so they
# live in their own machine-readable file, consumed by the panel notes and never
# merged into a set. Base R only. Definitions only; sourced by the median driver.

paper_source_once(paper_path(
  "support", "artifacts", "typed_artifacts.R"
))

# The frozen closure column order and types; an empty typed frame so a run with no
# stable one-sided limits still writes a valid header-only artifact.
logvar_lad_closure_schema <- function() {
  data.frame(
    coef = character(0), tau = numeric(0), estimator = character(0),
    sample_id = character(0), spec_id = character(0),
    crossing_row = integer(0), crossing_qtr = character(0),
    residual_side = character(0), coef_side = character(0),
    limit_value = numeric(0), path_id = integer(0),
    witness_status = character(0), M_span = numeric(0),
    tail_slope = numeric(0), tail_slope_stability = numeric(0),
    path_summary = character(0), provenance = character(0),
    stringsAsFactors = FALSE
  )
}

# Order the rows deterministically, encode, write with only character columns
# quoted, read back under the manifest colClasses, and require the round trip.
# crossing_qtr is validated against the canonical pattern (or NA) before the write.
write_logvar_lad_closure_csv <- function(df, path) {
  df <- df[order(df$tau, df$crossing_row, df$coef, df$path_id), , drop = FALSE]
  row.names(df) <- NULL
  paper_validate_yearquarter(
    df[["crossing_qtr"]],
    "crossing_qtr",
    "log_var_eq_lad_closure"
  )
  paper_write_typed_csv(
    df,
    path,
    "log_var_eq_lad_closure"
  )
  invisible(df)
}
