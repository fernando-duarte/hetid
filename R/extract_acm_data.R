#' Extract ACM Term Structure Data
#'
#' Extracts and processes ACM (Adrian, Crump, and Moench) term structure data,
#' allowing selection of specific data types, maturities, and date ranges.
#' Optionally converts monthly data to quarterly frequency.
#'
#' @param data_types Character vector specifying which data to extract.
#'   Options: "yields", "term_premia", "risk_neutral".
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
#' @return A data frame with Date column and selected variables.
#'   Column naming convention:
#'   - Yields: y1, y2, ..., y10
#'   - Term premia: tp1, tp2, ..., tp10
#'   - Risk-neutral yields: rn1, rn2, ..., rn10
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
#'   data_types = c("yields", "term_premia", "risk_neutral"),
#'   maturities = 5
#' )
#' }
#'
extract_acm_data <- function(data_types = c("yields", "term_premia"),
                             maturities = 1:10,
                             start_date = NULL,
                             end_date = NULL,
                             frequency = c("monthly", "quarterly"),
                             auto_download = FALSE) {
  # Validate inputs
  frequency <- match.arg(frequency)

  valid_types <- c("yields", "term_premia", "risk_neutral")
  if (!all(data_types %in% valid_types)) {
    stop(
      "Invalid data_types. Must be one or more of: ",
      paste(valid_types, collapse = ", ")
    )
  }

  if (!all(maturities %in% 1:10)) {
    stop("Maturities must be integers between 1 and 10")
  }

  # Load ACM data
  acm_data <- load_term_premia(auto_download = auto_download)

  if (is.null(acm_data)) {
    stop(
      "ACM data not available. Run download_term_premia() first or ",
      "set auto_download = TRUE"
    )
  }

  # Ensure DATE column is properly converted to Date type
  if ("DATE" %in% names(acm_data) && !inherits(acm_data$DATE, "Date")) {
    acm_data$DATE <- as.Date(acm_data$DATE, format = "%d-%b-%Y")
  }

  # Convert dates if provided as strings
  if (!is.null(start_date) && is.character(start_date)) {
    start_date <- as.Date(start_date)
  }
  if (!is.null(end_date) && is.character(end_date)) {
    end_date <- as.Date(end_date)
  }

  # Filter by date range
  if (!is.null(start_date)) {
    acm_data <- acm_data[acm_data$DATE >= start_date, ]
  }
  if (!is.null(end_date)) {
    acm_data <- acm_data[acm_data$DATE <= end_date, ]
  }

  # Start with Date column
  result <- data.frame(date = acm_data$DATE)

  # Build column mapping for selected data types and maturities
  col_mapping <- list()

  if ("yields" %in% data_types) {
    for (mat in maturities) {
      old_col <- sprintf("ACMY%02d", mat)
      new_col <- sprintf("y%d", mat)
      col_mapping[[new_col]] <- old_col
    }
  }

  if ("term_premia" %in% data_types) {
    for (mat in maturities) {
      old_col <- sprintf("ACMTP%02d", mat)
      new_col <- sprintf("tp%d", mat)
      col_mapping[[new_col]] <- old_col
    }
  }

  if ("risk_neutral" %in% data_types) {
    for (mat in maturities) {
      old_col <- sprintf("ACMRNY%02d", mat)
      new_col <- sprintf("rn%d", mat)
      col_mapping[[new_col]] <- old_col
    }
  }

  # Extract selected columns
  for (new_name in names(col_mapping)) {
    old_name <- col_mapping[[new_name]]
    if (old_name %in% names(acm_data)) {
      result[[new_name]] <- acm_data[[old_name]]
    } else {
      warning("Column ", old_name, " not found in data")
    }
  }

  # Convert to quarterly if requested
  if (frequency == "quarterly") {
    # Add year and month to identify quarter ends
    result$year <- as.numeric(format(result$date, "%Y"))
    result$month <- as.numeric(format(result$date, "%m"))
    result$quarter <- ceiling(result$month / 3)

    # For each quarter, keep only the last observation
    result <- result[order(result$date), ]

    # Group by year-quarter and take last observation
    last_in_quarter <- aggregate(
      date ~ year + quarter,
      data = result,
      FUN = max
    )

    # Merge to get full data for last observation in each quarter
    result <- merge(
      last_in_quarter[, "date", drop = FALSE],
      result[, setdiff(names(result), c("year", "month", "quarter"))],
      by = "date",
      all.x = TRUE
    )
  }

  # Sort by date
  result <- result[order(result$date), ]

  # Reset row names
  rownames(result) <- NULL

  result
}
