# Versioned CSV/RDS serialization for the joint moment-compatibility artifacts
# (joint-GMM, logvar-joint-gmm). Definitions only, sourced by
# artifact_schema.R: the flat typed frame assembled from the fixed
# schema rows, its column-type manifest, the locale-independent encoding, the
# column-wise CSV round-trip guard, and the writer that requires both the RDS
# identical() round trip and the CSV agreement before it returns. It uses the
# established honest-artifact idiom so the machine-readable view and bit-exact
# RDS channel stay in step.

# Flatten the per-row result lists to one typed data.frame through the fixed
# schema assembler, so every row carries every column at its declared type.
.jg_flatten <- function(rows) {
  do.call(rbind, lapply(rows, logvar_joint_gmm_csv_row))
}

# The column-type manifest derived from the typed frame: flags logical, counts
# and ranks integer, coefficients/distances/tolerances double, identifiers and
# enums character.
.jg_manifest <- function(df) {
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

# Locale-independent character encoding: 17 significant digits for finite
# doubles, R-parseable Inf/-Inf/NaN for the non-finite ones, and the literal
# token "NA" for every missing value. Doubles read back through R's CSV parser
# to floating tolerance; the RDS is the bit-exact channel.
.jg_encode <- function(df) {
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
    } else if (is.logical(col)) {
      ifelse(is.na(col), "NA", ifelse(col, "TRUE", "FALSE"))
    } else {
      ifelse(is.na(col), "NA", as.character(col))
    }
  })
  as.data.frame(enc, stringsAsFactors = FALSE, check.names = FALSE)
}

# Column-wise agreement of the CSV read-back with the typed frame: character,
# integer, and logical columns round-trip exactly; double columns are checked
# with all.equal because R's text-to-double parser is not correctly rounded, so
# a decimal CSV cannot reproduce every double bit for bit.
.jg_check_roundtrip <- function(flat, back) {
  if (!identical(names(flat), names(back))) {
    stop("log_var_eq_joint_gmm: CSV column order changed on the round trip")
  }
  for (cn in names(flat)) {
    a <- flat[[cn]]
    b <- back[[cn]]
    ok <- if (is.double(a)) {
      isTRUE(all.equal(a, b, tolerance = 1e-9))
    } else {
      identical(a, b)
    }
    if (!ok) stop(sprintf("log_var_eq_joint_gmm: CSV round-trip mismatch in %s", cn))
  }
  invisible(TRUE)
}

# Write the versioned RDS and the flat CSV, then require both to round-trip:
# identical() for the whole RDS object and column-wise agreement for the CSV
# read back under the manifest colClasses. Only character columns are quoted so
# an id or enum with a comma stays protected while numeric fields stay bare.
write_joint_gmm_artifacts <- function(object, csv_path, rds_path) {
  saveRDS(object, rds_path, version = 3)
  if (!identical(readRDS(rds_path), object)) {
    stop("log_var_eq_joint_gmm: RDS round trip is not identical")
  }
  flat <- .jg_flatten(object$rows)
  manifest <- .jg_manifest(flat)
  utils::write.csv(
    .jg_encode(flat), csv_path,
    row.names = FALSE, quote = which(manifest == "character"),
    fileEncoding = "UTF-8"
  )
  back <- utils::read.csv(
    csv_path,
    colClasses = manifest, na.strings = "NA",
    check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8"
  )
  .jg_check_roundtrip(flat, back)
  invisible(object)
}
