#' Extract ACM Term Structure Data
#'
#' Extracts and processes ACM (Adrian, Crump, and Moench) term structure data,
#' allowing selection of specific data types, maturities, and date ranges.
#' Optionally converts monthly data to quarterly frequency.
#'
#' @param data_types Character vector specifying which data to extract.
#'   Options: "yields", "term_premia", "risk_neutral_yields".
#'   Default is c("yields", "term_premia").
#' @param maturities Numeric vector of maturities in months (3-120).
#'   Default is the annual nodes \code{seq(12, 120, by = 12)}
#'   (\code{HETID_CONSTANTS$DEFAULT_ACM_MATURITIES}); pass
#'   \code{HETID_CONSTANTS$ALL_ACM_MATURITIES} for the full monthly
#'   grid (GitHub source only).
#' @param start_date Date or character string (YYYY-MM-DD format) for start of sample.
#'   Default is NULL (earliest available date).
#' @param end_date Date or character string (YYYY-MM-DD format) for end of sample.
#'   Default is NULL (latest available date).
#' @param frequency Character string, either "monthly" (default) or "quarterly".
#'   Quarterly data uses the last observation of each quarter.
#' @param auto_download Logical. If TRUE and data doesn't exist, automatically
#'   downloads it. Default is FALSE.
#' @param use_incomplete_quarters Logical, only used when
#'   \code{frequency = "quarterly"}. Governs quarters whose last
#'   available observation is not in the terminal month (March, June,
#'   September, December). If TRUE (the default, from
#'   \code{HETID_CONSTANTS$USE_INCOMPLETE_QUARTERS}), such quarters keep
#'   their latest observation, re-dated to the end of the terminal
#'   quarter month so the quarterly series is uniformly dated; a classed
#'   warning (\code{hetid_warning_incomplete_quarter}) reports them
#'   because incomplete data enters the output. If FALSE, such quarters
#'   are dropped, announced by an informational message.
#' @param source Data source passed to \code{\link{load_term_premia}}:
#'   \code{"auto"} (default; GitHub user cache, then bundled copy),
#'   \code{"github"}, or \code{"nyfed"} (explicitly downloaded NY Fed
#'   cache only, annual maturities).
#'
#' @return A data frame with date column and selected variables.
#'   Column naming convention (maturity suffix in months):
#'   - Yields: y12, y24, ..., y120 (plus e.g. y6, y18 when requested)
#'   - Term premia: tp12, tp24, ..., tp120
#'   - Risk-neutral yields: rny12, rny24, ..., rny120
#'
#' @details
#' The raw ACM data carries maturities at one-month steps from 3 to
#' 120 months. Whole-year maturities keep the official column names
#' (ACMY01-ACMY10, ACMTP01-ACMTP10, ACMRNY01-ACMRNY10); sub-annual
#' months use names like ACMY003M. The NY Fed fallback source provides
#' only the annual nodes; requesting sub-annual maturities against it
#' raises a structured error.
#'
#' All values are in annualized percentage points.
#'
#' By construction: Term Premium = Yield - Risk-Neutral Yield
#'
#' @export
#'
#' @examples
#' # Extract yields and term premia at the annual nodes
#' data <- extract_acm_data()
#'
#' # Extract only 2-year and 10-year yields for specific period
#' data <- extract_acm_data(
#'   data_types = "yields",
#'   maturities = c(24, 120),
#'   start_date = "2010-01-01",
#'   end_date = "2020-12-31"
#' )
#'
#' # Get quarterly term premia at the annual nodes
#' data <- extract_acm_data(
#'   data_types = "term_premia",
#'   frequency = "quarterly"
#' )
#'
#' # Extract all three data types for the 5-year (60-month) maturity
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia", "risk_neutral_yields"),
#'   maturities = 60
#' )
#'
#' # Sub-annual maturities from the monthly grid
#' data <- extract_acm_data(
#'   data_types = "yields",
#'   maturities = c(6, 18, 30)
#' )
extract_acm_data <- function(data_types = c("yields", "term_premia"),
                             maturities = HETID_CONSTANTS$DEFAULT_ACM_MATURITIES,
                             start_date = NULL,
                             end_date = NULL,
                             frequency = c("monthly", "quarterly"),
                             auto_download = FALSE,
                             use_incomplete_quarters =
                               HETID_CONSTANTS$USE_INCOMPLETE_QUARTERS,
                             source = c("auto", "github", "nyfed")) {
  # Validate inputs
  frequency <- match.arg(frequency)
  source <- match.arg(source)
  validate_acm_extract_inputs(data_types, maturities, use_incomplete_quarters)

  # Load ACM data
  acm_data <- load_term_premia(auto_download = auto_download, source = source)
  assert_insufficient_data_ok(
    !is.null(acm_data),
    paste0(
      "ACM data not available. Run download_term_premia()",
      " first or set auto_download = TRUE"
    )
  )

  # Annual-only sources cannot serve month-level requests
  assert_subannual_available(acm_data, maturities)

  # The load_term_premia function now returns lowercase 'date' column
  # Ensure date column is properly converted to Date type
  acm_data <- normalize_acm_date_column(acm_data)

  # Convert dates if provided as strings
  start_date <- coerce_optional_date(start_date, "start_date")
  end_date <- coerce_optional_date(end_date, "end_date")

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
