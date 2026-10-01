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
#' only annual nodes; missing non-annual nodes produce a structured error
#' directing the caller to the GitHub source.
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
  absent <- setdiff(
    acm_raw_column_name("yields", sub_annual), names(acm_data)
  )
  if (length(absent) > 0) {
    stop_insufficient_data(paste0(
      "The loaded ACM source provides only annual maturities (",
      paste(HETID_CONSTANTS$DEFAULT_ACM_MATURITIES, collapse = ", "),
      " months), but sub-annual months were requested: ",
      paste(sub_annual, collapse = ", "),
      ". Month-level maturities require the GitHub source: ",
      "download_term_premia(source = \"github\")."
    ))
  }
  invisible(TRUE)
}
