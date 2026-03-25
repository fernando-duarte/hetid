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
#' \dontrun{
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
#' }
extract_acm_data <- function(data_types = c("yields", "term_premia"),
                             maturities = HETID_CONSTANTS$MIN_MATURITY:HETID_CONSTANTS$MAX_MATURITY,
                             start_date = NULL,
                             end_date = NULL,
                             frequency = c("monthly", "quarterly"),
                             auto_download = FALSE) {
  # Validate inputs
  frequency <- match.arg(frequency)
  validate_acm_extract_inputs(data_types, maturities)

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
    result <- convert_to_quarterly(result) # nolint: object_usage_linter
  }

  # Sort by date
  result <- result[order(result$date), ]

  # Reset row names
  rownames(result) <- NULL

  result
}

#' Validate ACM extraction inputs
#'
#' @keywords internal
#' @noRd
validate_acm_extract_inputs <- function(data_types, maturities) {
  valid_types <- names(HETID_ACM_SCHEMA)
  assert_bad_argument_ok(
    all(data_types %in% valid_types),
    paste0(
      "Invalid data_types. Must be one or more of: ",
      paste(valid_types, collapse = ", ")
    ),
    arg = "data_types"
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

#' Coerce optional date input
#'
#' @keywords internal
#' @noRd
coerce_optional_date <- function(x) {
  if (!is.null(x) && is.character(x)) {
    return(as.Date(x))
  }

  x
}

#' Normalize ACM date column
#'
#' @keywords internal
#' @noRd
normalize_acm_date_column <- function(acm_data) {
  if ("date" %in% names(acm_data) && !inherits(acm_data$date, "Date")) {
    acm_data$date <- as.Date(
      acm_data$date,
      format = HETID_CONSTANTS$ACM_DATE_FORMAT
    )
  }

  acm_data
}

#' Filter ACM data by optional date bounds
#'
#' @keywords internal
#' @noRd
filter_acm_date_range <- function(acm_data, start_date, end_date) {
  if (!is.null(start_date)) {
    acm_data <- acm_data[acm_data$date >= start_date, ]
  }
  if (!is.null(end_date)) {
    acm_data <- acm_data[acm_data$date <= end_date, ]
  }

  acm_data
}
