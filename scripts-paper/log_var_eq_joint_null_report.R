# Console summary and artifact serialization for the joint-null theta_R = 0
# distance diagnostic. Definitions only: the compact per-tau print and the CSV
# plus versioned RDS writer with its typed round-trip guards. Sourced by the
# driver log_var_eq_joint_null.R (which the offline suite sources too), so these
# functions are available whether or not the pipeline objects exist.

# Compact console summary: the frozen sd-effect scales once, then one block per
# tau with the attained scaled distance, numerical status, membership verdict,
# and soft warnings (edge-riding arg-min, sparse grid, L-infinity sensitivity)
# beside the min |eps_hat| and attained intercept that flag a crossing-driven
# norm. Accepts either one flat row or a list of rows.
print_logvar_joint_null <- function(rows, scales) {
  if (!is.null(rows$status)) rows <- list(rows)
  cat("Joint-null theta_R = 0 distance diagnostic (estimator logols)\n")
  cat(sprintf(
    "  frozen scales d_j = 1/sd(PC_R,j): %s\n",
    paste(sprintf("%.4g", scales$d), collapse = " ")
  ))
  for (row in rows) {
    warn <- character(0)
    if (isTRUE(row$box_active)) warn <- c(warn, "arg-min on a display-box edge")
    if (isTRUE(row$sparse_grid)) warn <- c(warn, "sparse feasible grid")
    if (isTRUE(row$linf_trigger)) {
      warn <- c(warn, sprintf("L-inf sensitivity %s", row$linf_sensitivity_status))
    }
    tag <- if (length(warn)) sprintf("  [%s]", paste(warn, collapse = "; ")) else ""
    cat(sprintf(
      "  tau = %.3g: scaled d2 = %.4g (d_inf = %.4g) | %s | %s%s\n",
      row$tau_display, row$scaled_l2, row$scaled_linf,
      row$status, row$membership_result, tag
    ))
    qtr_txt <- if (is.null(row$nearest_crossing_qtr) ||
      is.na(row$nearest_crossing_qtr)) {
      "--"
    } else {
      row$nearest_crossing_qtr
    }
    cat(sprintf(
      "    arg-min b = (%s); min |eps_hat| = %.3g at %s; intercept theta0 = %.4g\n",
      paste(sprintf("%.4g", c(row$b1, row$b2, row$b3)), collapse = ", "),
      row$min_abs_eps, qtr_txt, row$theta0
    ))
  }
  cat(paste0(
    "  Note: 'witness' means one feasible b_N makes all four slopes numerically ",
    "zero; 'not demonstrated' is not a test rejection or a globally certified ",
    "exclusion.\n"
  ))
  invisible(rows)
}

# Flatten the per-tau rows to one typed data.frame; a missing scalar becomes a
# typed NA promoted to the column type by the populated rows. Column order is
# the first row's field order, shared by the CSV write and the read-back.
.logvar_joint_null_flatten <- function(rows) {
  cols <- unique(unlist(lapply(rows, names)))
  vals <- lapply(cols, function(cn) {
    do.call(c, lapply(rows, function(r) if (is.null(r[[cn]])) NA else r[[cn]]))
  })
  names(vals) <- cols
  as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
}

# The normative column-type manifest derived from the typed frame: flags
# logical, counts and ranks integer, coefficients/distances/tolerances double,
# identifiers/qtr/enums character.
.logvar_joint_null_manifest <- function(df) {
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
# doubles, the R-parseable tokens Inf/-Inf/NaN for the non-finite ones, and the
# literal token "NA" for every missing value. Doubles read back through R's CSV
# parser to floating tolerance, not bit for bit; the RDS is the exact channel.
.logvar_joint_null_encode <- function(df) {
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

# Column-wise agreement of the CSV read-back with the typed frame. Character,
# integer, and logical columns must round-trip exactly (they can, and a mismatch
# there is a real encoding bug). Double columns are checked with all.equal:
# R's own text-to-double parser (as.numeric/read.csv) is not correctly rounded,
# so a decimal CSV cannot reproduce every double bit for bit -- the versioned
# RDS is the bit-exact channel, the CSV is the flat machine-readable view, and a
# double that read back off by more than floating tolerance is a genuine fault.
.logvar_joint_null_check_roundtrip <- function(flat, back) {
  if (!identical(names(flat), names(back))) {
    stop("log_var_eq_joint_null: CSV column order changed on the round trip")
  }
  for (cn in names(flat)) {
    a <- flat[[cn]]
    b <- back[[cn]]
    ok <- if (is.double(a)) {
      isTRUE(all.equal(a, b, tolerance = 1e-9))
    } else {
      identical(a, b)
    }
    if (!ok) stop(sprintf("log_var_eq_joint_null: CSV round-trip mismatch in %s", cn))
  }
  invisible(TRUE)
}

# Write the versioned RDS and the flat CSV, then require both to round-trip:
# identical() for the whole RDS object and column-wise agreement for the CSV read
# back under the manifest colClasses. nearest_crossing_qtr is validated against
# the "%Y Q%q" pattern (or NA) before the write.
write_logvar_joint_null_artifacts <- function(object, csv_path, rds_path) {
  saveRDS(object, rds_path, version = 3)
  if (!identical(readRDS(rds_path), object)) {
    stop("log_var_eq_joint_null: RDS round trip is not identical")
  }
  flat <- .logvar_joint_null_flatten(object$rows)
  ncq <- flat[["nearest_crossing_qtr"]]
  if (!is.null(ncq) && !all(is.na(ncq) | grepl("^[0-9]{4} Q[1-4]$", ncq))) {
    stop("log_var_eq_joint_null: nearest_crossing_qtr is not %Y Q%q or NA")
  }
  manifest <- .logvar_joint_null_manifest(flat)
  # quote only the character columns so a comma or space in an id or qtr label
  # is protected, while numeric/integer/logical fields stay bare for colClasses
  utils::write.csv(
    .logvar_joint_null_encode(flat), csv_path,
    row.names = FALSE, quote = which(manifest == "character"),
    fileEncoding = "UTF-8"
  )
  back <- utils::read.csv(
    csv_path,
    colClasses = manifest, na.strings = "NA",
    check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8"
  )
  .logvar_joint_null_check_roundtrip(flat, back)
  invisible(object)
}
