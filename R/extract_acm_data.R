#' Extract ACM Term Structure Data
#'
#' Extracts and processes ACM (Adrian, Crump, and Moench) term structure data,
#' allowing selection of specific data types, maturities, and date ranges.
#' Optionally converts monthly data to quarterly frequency.
#'
#' @param data_types Character vector specifying which data to extract.
#'   Options: "yields", "term_premia", "risk_neutral_yields".
#'   Default is c("yields", "term_premia").
#' @param maturities Numeric vector of maturities (1-10 years).
#'   Default is 1:10 (all maturities).
#' @param start_date Date or character string (YYYY-MM-DD format) for start of sample.
#'   Default is NULL (earliest available date).
#' @param end_date Date or character string (YYYY-MM-DD format) for end of sample.
#'   Default is NULL (latest available date).
#' @param frequency Character string, either "monthly" (default) or "quarterly".
#'   Quarterly data uses the last observation of each quarter.
#' @param auto_download Logical. If TRUE and data doesn't exist, automatically
#'   downloads it. Default is FALSE.
#' @param use_incomplete_quarters Logical, only used when
#'   \code{frequency = "quarterly"}. Quarters whose last available
#'   observation is not in the terminal month (March, June, September,
#'   December) raise a classed warning. If TRUE (the default, from
#'   \code{HETID_CONSTANTS$USE_INCOMPLETE_QUARTERS}), such quarters keep
#'   their latest observation, re-dated to the end of the terminal
#'   quarter month so the quarterly series is uniformly dated. If FALSE,
#'   such quarters are dropped.
#'
#' @return A data frame with date column and selected variables.
#'   Column naming convention:
#'   - Yields: y1, y2, ..., y10
#'   - Term premia: tp1, tp2, ..., tp10
#'   - Risk-neutral yields: rny1, rny2, ..., rny10
#'
#' @details
#' The ACM data contains:
#' - ACMY01-ACMY10: Zero-coupon Treasury yields (1-10 years)
#' - ACMTP01-ACMTP10: Term premium estimates (1-10 years)
#' - ACMRNY01-ACMRNY10: Risk-neutral yields (1-10 years)
#'
#' All values are in annualized percentage points.
#'
#' By construction: Term Premium = Yield - Risk-Neutral Yield
#'
#' @export
#'
#' @examples
#' # Extract all yields and term premia
#' data <- extract_acm_data()
#'
#' # Extract only 2-year and 10-year yields for specific period
#' data <- extract_acm_data(
#'   data_types = "yields",
#'   maturities = c(2, 10),
#'   start_date = "2010-01-01",
#'   end_date = "2020-12-31"
#' )
#'
#' # Get quarterly term premia for all maturities
#' data <- extract_acm_data(
#'   data_types = "term_premia",
#'   frequency = "quarterly"
#' )
#'
#' # Extract all three data types for 5-year maturity
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia", "risk_neutral_yields"),
#'   maturities = 5
#' )
extract_acm_data <- function(data_types = c("yields", "term_premia"),
                             maturities = HETID_CONSTANTS$MIN_MATURITY:HETID_CONSTANTS$MAX_MATURITY,
                             start_date = NULL,
                             end_date = NULL,
                             frequency = c("monthly", "quarterly"),
                             auto_download = FALSE,
                             use_incomplete_quarters =
                               HETID_CONSTANTS$USE_INCOMPLETE_QUARTERS) {
  # Validate inputs
  frequency <- match.arg(frequency)
  validate_acm_extract_inputs(data_types, maturities, use_incomplete_quarters)

  # Load ACM data
  acm_data <- load_term_premia(auto_download = auto_download)
  assert_insufficient_data_ok(
    !is.null(acm_data),
    paste0(
      "ACM data not available. Run download_term_premia()",
      " first or set auto_download = TRUE"
    )
  )

  # The load_term_premia function now returns lowercase 'date' column
  # Ensure date column is properly converted to Date type
  acm_data <- normalize_acm_date_column(acm_data)

  # Convert dates if provided as strings
  start_date <- coerce_optional_date(start_date)
  end_date <- coerce_optional_date(end_date)

  # Filter by date range
  acm_data <- filter_acm_date_range(
    acm_data,
    start_date,
    end_date
  )

  # Start with date column
  result <- data.frame(date = acm_data$date)

  # Build column mapping for selected data types and maturities
  col_mapping <- build_acm_col_mapping(data_types, maturities) # nolint: object_usage_linter

  # Extract selected columns
  for (new_name in names(col_mapping)) {
    old_name <- col_mapping[[new_name]]
    if (old_name %in% names(acm_data)) {
      result[[new_name]] <- acm_data[[old_name]]
    } else {
      warning("Column ", old_name, " not found in data", call. = FALSE)
    }
  }

  # Convert to quarterly if requested
  if (frequency == "quarterly") {
    result <- convert_to_quarterly(result, use_incomplete_quarters) # nolint: object_usage_linter
  }

  # Sort by date; drop = FALSE keeps single-column results as data frames
  result <- result[order(result$date), , drop = FALSE]

  # Reset row names
  rownames(result) <- NULL

  result
}

#' Validate ACM extraction inputs
#'
#' @keywords internal
#' @noRd
validate_acm_extract_inputs <- function(data_types, maturities,
                                        use_incomplete_quarters = TRUE) {
  assert_bad_argument_ok(
    isTRUE(use_incomplete_quarters) || isFALSE(use_incomplete_quarters),
    "use_incomplete_quarters must be TRUE or FALSE",
    arg = "use_incomplete_quarters"
  )

  valid_types <- names(HETID_ACM_SCHEMA)
  assert_bad_argument_ok(
    is.character(data_types) && length(data_types) >= 1 &&
      all(data_types %in% valid_types),
    paste0(
      "Invalid data_types. Must be one or more of: ",
      paste(valid_types, collapse = ", ")
    ),
    arg = "data_types"
  )

  assert_bad_argument_ok(
    is.numeric(maturities) && length(maturities) >= 1,
    "maturities must be a non-empty numeric vector",
    arg = "maturities"
  )
  assert_bad_argument_ok(
    all(is.finite(maturities)) && all(maturities == trunc(maturities)),
    "maturities must be whole numbers",
    arg = "maturities"
  )
  assert_bad_argument_ok(
    all(
      maturities %in%
        HETID_CONSTANTS$MIN_MATURITY:HETID_CONSTANTS$MAX_MATURITY
    ),
    paste0(
      "Maturities must be integers between ",
      HETID_CONSTANTS$MIN_MATURITY,
      " and ", HETID_CONSTANTS$MAX_MATURITY
    ),
    arg = "maturities"
  )
}
