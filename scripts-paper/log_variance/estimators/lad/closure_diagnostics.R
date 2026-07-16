# Closure-diagnostics artifact for the median (LAD) log-variance map: the typed
# schema of the one-sided crossing-limit approximations and the flat CSV writer
# with its 17-significant-digit encoding and column-wise read-back guard. The
# closure rows are deliberately separate from the attained hull -- they are stable
# finite approximate limits at verified crossings, never attained cells -- so they
# live in their own machine-readable file, consumed by the panel notes and never
# merged into a set. Base R only. Definitions only; sourced by the median driver.

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

# Column-type manifest read off the typed frame: logicals logical, counts and rows
# integer, coefficients and metrics double, identifiers and enums character. Shared
# by the CSV write (quoting rule) and the read-back (colClasses).
.logvar_lad_closure_manifest <- function(df) {
  vapply(df, function(col) {
    if (is.logical(col)) {
      "logical"
    } else if (is.integer(col)) {
      "integer"
    } else if (is.double(col)) {
      "numeric"
    } else {
      "character"
    }
  }, character(1))
}

# Locale-independent encoding: 17 significant digits for finite doubles, the
# R-parseable Inf/-Inf/NaN tokens for the non-finite ones, "NA" for every missing
# value. Doubles read back through R's CSV parser to floating tolerance, so the
# round-trip guard checks doubles with all.equal and everything else exactly.
.logvar_lad_closure_encode <- function(df) {
  enc <- lapply(df, function(col) {
    if (is.double(col)) {
      vapply(col, function(v) {
        if (is.nan(v)) {
          "NaN"
        } else if (is.na(v)) {
          "NA"
        } else if (is.infinite(v)) {
          if (v > 0) "Inf" else "-Inf"
        } else {
          sprintf("%.17g", v)
        }
      }, character(1))
    } else if (is.integer(col)) {
      ifelse(is.na(col), "NA", as.character(col))
    } else {
      ifelse(is.na(col), "NA", as.character(col))
    }
  })
  as.data.frame(enc, stringsAsFactors = FALSE, check.names = FALSE)
}

# Column-wise agreement of the CSV read-back with the typed frame: character and
# integer columns must match exactly, double columns to floating tolerance (R's
# text-to-double parser is not correctly rounded, so a decimal CSV cannot reproduce
# every double bit for bit; this file is the flat view, not the exact channel).
.logvar_lad_closure_roundtrip <- function(flat, back) {
  if (!identical(names(flat), names(back))) {
    stop("log_var_eq_lad_closure: CSV column order changed on the round trip")
  }
  for (cn in names(flat)) {
    a <- flat[[cn]]
    b <- back[[cn]]
    ok <- if (is.double(a)) isTRUE(all.equal(a, b, tolerance = 1e-9)) else identical(a, b)
    if (!ok) stop(sprintf("log_var_eq_lad_closure: CSV round-trip mismatch in %s", cn))
  }
  invisible(TRUE)
}

# Order the rows deterministically, encode, write with only character columns
# quoted, read back under the manifest colClasses, and require the round trip.
# crossing_qtr is validated against the "%Y Q%q" pattern (or NA) before the write.
write_logvar_lad_closure_csv <- function(df, path) {
  df <- df[order(df$tau, df$crossing_row, df$coef, df$path_id), , drop = FALSE]
  row.names(df) <- NULL
  ncq <- df[["crossing_qtr"]]
  if (!all(is.na(ncq) | grepl("^[0-9]{4} Q[1-4]$", ncq))) {
    stop("log_var_eq_lad_closure: crossing_qtr is not %Y Q%q or NA")
  }
  manifest <- .logvar_lad_closure_manifest(df)
  utils::write.csv(
    .logvar_lad_closure_encode(df), path,
    row.names = FALSE, quote = which(manifest == "character"),
    fileEncoding = "UTF-8"
  )
  back <- utils::read.csv(
    path,
    colClasses = manifest, na.strings = "NA",
    check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8"
  )
  .logvar_lad_closure_roundtrip(df, back)
  invisible(df)
}
