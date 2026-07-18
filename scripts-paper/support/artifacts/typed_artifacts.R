# Shared typed CSV and exact-RDS artifact serialization.

paper_typed_manifest <- function(df) {
  vapply(df, function(column) {
    if (is.logical(column)) {
      "logical"
    } else if (is.integer(column)) {
      "integer"
    } else if (is.double(column)) {
      "numeric"
    } else {
      "character"
    }
  }, character(1))
}

paper_encode_character <- function(
  values,
  control = PAPER_SERIALIZATION_CONTROL
) {
  encoded <- as.character(values)
  present <- !is.na(values)
  reserved <- present & (
    encoded == control$na_token |
      startsWith(encoded, control$character_escape)
  )
  encoded[reserved] <- paste0(
    control$character_escape,
    encoded[reserved]
  )
  encoded[!present] <- control$na_token
  encoded
}

paper_decode_character <- function(
  values,
  control = PAPER_SERIALIZATION_CONTROL
) {
  escaped <- !is.na(values) &
    startsWith(values, control$character_escape)
  values[escaped] <- substring(
    values[escaped],
    nchar(control$character_escape) + 1L
  )
  values
}

paper_encode_typed_frame <- function(
  df,
  control = PAPER_SERIALIZATION_CONTROL
) {
  encoded <- lapply(df, function(column) {
    if (is.double(column)) {
      vapply(column, function(value) {
        if (is.nan(value)) {
          "NaN"
        } else if (is.na(value)) {
          control$na_token
        } else if (is.infinite(value)) {
          if (value > 0) "Inf" else "-Inf"
        } else {
          sprintf(control$numeric_format, value)
        }
      }, character(1))
    } else if (is.logical(column)) {
      ifelse(
        is.na(column),
        control$na_token,
        ifelse(column, "TRUE", "FALSE")
      )
    } else {
      paper_encode_character(column, control)
    }
  })
  as.data.frame(
    encoded,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

paper_decode_typed_frame <- function(
  df,
  manifest,
  control = PAPER_SERIALIZATION_CONTROL
) {
  character_columns <- names(df)[manifest == "character"]
  for (column in character_columns) {
    df[[column]] <- paper_decode_character(df[[column]], control)
  }
  df
}

paper_check_typed_roundtrip <- function(
  expected,
  actual,
  error_prefix,
  tolerance = PAPER_SERIALIZATION_CONTROL$roundtrip_tolerance
) {
  if (!identical(names(expected), names(actual))) {
    stop(
      error_prefix,
      ": CSV column order changed on the round trip",
      call. = FALSE
    )
  }
  for (column in names(expected)) {
    before <- expected[[column]]
    after <- actual[[column]]
    ok <- if (is.double(before)) {
      isTRUE(all.equal(before, after, tolerance = tolerance))
    } else {
      identical(before, after)
    }
    if (!ok) {
      stop(
        error_prefix,
        ": CSV round-trip mismatch in ",
        column,
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

paper_validate_yearquarter <- function(
  values,
  field,
  error_prefix,
  control = PAPER_SERIALIZATION_CONTROL
) {
  ok <- is.na(values) | grepl(control$yearquarter_pattern, values)
  if (!all(ok)) {
    stop(
      error_prefix,
      ": ",
      field,
      " does not match the canonical year-quarter format or NA",
      call. = FALSE
    )
  }
  invisible(values)
}

paper_write_typed_csv <- function(
  frame,
  path,
  error_prefix,
  control = PAPER_SERIALIZATION_CONTROL
) {
  manifest <- paper_typed_manifest(frame)
  utils::write.csv(
    paper_encode_typed_frame(frame, control),
    path,
    row.names = FALSE,
    quote = which(manifest == "character"),
    fileEncoding = control$encoding
  )
  actual <- utils::read.csv(
    path,
    colClasses = manifest,
    na.strings = control$na_token,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    encoding = control$encoding
  )
  actual <- paper_decode_typed_frame(actual, manifest, control)
  paper_check_typed_roundtrip(
    frame,
    actual,
    error_prefix,
    control$roundtrip_tolerance
  )
  invisible(frame)
}

paper_write_exact_rds <- function(
  object,
  path,
  error_prefix,
  control = PAPER_SERIALIZATION_CONTROL
) {
  saveRDS(object, path, version = control$rds_version)
  if (!identical(readRDS(path), object)) {
    stop(
      error_prefix,
      ": RDS round trip is not identical",
      call. = FALSE
    )
  }
  invisible(object)
}
