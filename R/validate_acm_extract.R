#' Validation Helpers for ACM Extraction
#'
#' Input validation for \code{\link{extract_acm_data}}: argument
#' contracts plus the source-capability guard for sub-annual
#' maturities.
#'
#' @name validate_acm_extract
#' @keywords internal
NULL

#' Validate ACM extraction inputs
#'
#' @param data_types Character vector of data type keys
#' @param maturities Numeric vector of maturities in months
#' @param use_incomplete_quarters Logical flag from the caller
#' @return Invisible TRUE if valid, stops with informative error otherwise
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

#' Assert the Loaded Source Covers Sub-Annual Maturities
#'
#' The NY Fed fallback source carries only annual nodes; requesting
#' month-level maturities against it gets a structured error naming
#' the fix instead of a pile of missing-column warnings.
#'
#' @param acm_data Raw loaded ACM data frame
#' @param maturities Requested maturities in months
#' @return Invisible TRUE
#' @keywords internal
assert_subannual_available <- function(acm_data, maturities) {
  units_per_year <- HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  sub_annual <- maturities[maturities %% units_per_year != 0L]
  if (length(sub_annual) == 0) {
    return(invisible(TRUE))
  }
  # Probe the exact requested columns: a source with some sub-annual nodes (e.g. 3M)
  # but not the requested one (e.g. 9M) must still fail here.
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
