#' Validation Helpers for ACM Extraction
#'
#' Input validation for \code{\link{extract_acm_data}}: argument
#' contracts plus the source-capability guard for non-annual
#' maturity nodes.
#'
#' @name validate_acm_extract
#' @keywords internal
NULL

#' Validate ACM Extraction Inputs
#'
#' Checks the data-type, bond-maturity, and incomplete-quarter arguments
#' before ACM data is loaded. Inputs are validated without modification.
#'
#' @param data_types Non-empty character vector with no missing values or
#'   duplicates. Allowed keys are \code{"yields"}, \code{"term_premia"},
#'   and \code{"risk_neutral_yields"}, from \code{HETID_ACM_SCHEMA}.
#' @param maturities Non-empty numeric vector of distinct, finite integer
#'   bond maturities in months, between \code{HETID_CONSTANTS$MIN_MATURITY}
#'   and \code{HETID_CONSTANTS$MAX_MATURITY}, inclusive.
#' @param use_incomplete_quarters A single non-missing logical value.
#'   Defaults to \code{TRUE}; this helper checks the flag without applying
#'   the quarterly-conversion policy.
#' @return Invisible \code{TRUE} if valid; otherwise signals a
#'   \code{hetid_error_bad_argument} condition identifying the argument.
#' @keywords internal
validate_acm_extract_inputs <- function(data_types, maturities,
                                        use_incomplete_quarters = TRUE) {
  assert_flag(use_incomplete_quarters, "use_incomplete_quarters")

  assert_acm_data_types(data_types)

  validate_maturities(
    maturities,
    max_value = HETID_CONSTANTS$MAX_MATURITY,
    max_label = "MAX_MATURITY (months)",
    min_value = HETID_CONSTANTS$MIN_MATURITY
  )
}

#' Assert the Loaded Source Covers Non-Annual Maturities
#'
#' Checks that raw yield-column names exist for every requested maturity
#' that is not a whole number of years. The NY Fed fallback source carries
#' only annual nodes. Missing non-annual nodes produce a structured error
#' listing the absent columns and maturities. When the source contains only
#' annual nodes, the error directs the caller to the GitHub source.
#'
#' Only column names are checked: values, including missing values, are
#' not inspected. Annual nodes and other data types are checked separately
#' by \code{\link{extract_acm_data}}.
#'
#' @param acm_data Raw loaded ACM data frame with source column names.
#' @param maturities Numeric vector of requested bond maturities in months,
#'   already validated by \code{\link{validate_acm_extract_inputs}}.
#' @return Invisible \code{TRUE} when all requested non-annual yield columns
#'   exist or no non-annual maturities are requested; otherwise signals a
#'   \code{hetid_error_insufficient_data} condition. The data is not modified.
#' @keywords internal
assert_subannual_available <- function(acm_data, maturities) {
  units_per_year <- HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  sub_annual <- maturities[maturities %% units_per_year != 0L]
  if (length(sub_annual) == 0) {
    return(invisible(TRUE))
  }
  requested <- acm_raw_column_name("yields", sub_annual)
  absent_idx <- !requested %in% names(acm_data)
  absent <- requested[absent_idx]
  if (length(absent) > 0) {
    nonannual <- HETID_CONSTANTS$ALL_ACM_MATURITIES
    nonannual <- nonannual[nonannual %% units_per_year != 0L]
    nonannual_columns <- unlist(lapply(names(HETID_ACM_SCHEMA), function(type) {
      acm_raw_column_name(type, nonannual)
    }), use.names = FALSE)
    annual_columns <- unlist(lapply(names(HETID_ACM_SCHEMA), function(type) {
      acm_raw_column_name(type, HETID_CONSTANTS$DEFAULT_ACM_MATURITIES)
    }), use.names = FALSE)
    annual_only <- any(annual_columns %in% names(acm_data)) &&
      !any(nonannual_columns %in% names(acm_data))
    diagnosis <- if (annual_only) {
      "The loaded ACM source provides only annual maturities; missing yield column(s): "
    } else {
      "The loaded ACM source is missing required yield column(s): "
    }
    stop_insufficient_data(paste0(
      diagnosis, paste(absent, collapse = ", "),
      " (months: ", paste(sub_annual[absent_idx], collapse = ", "), ").",
      if (annual_only) {
        paste0(
          " Month-level maturities require the GitHub source: ",
          "download_term_premia(source = \"github\")."
        )
      } else {
        " The source file may be incomplete or corrupt."
      }
    ))
  }
  invisible(TRUE)
}
