#' Extract ACM Term Structure Data
#'
#' Extracts and processes ACM (Adrian, Crump, and Moench) term structure data,
#' allowing selection of specific data types, maturities, and date ranges.
#' Optionally converts monthly data to quarterly frequency.
#'
#' @param data_types Character vector specifying which data to extract.
#'   Options: "yields", "term_premia", "risk_neutral_yields".
#'   Default is c("yields", "term_premia").
#' @param maturities Numeric vector of maturities in months
#'   (\code{HETID_CONSTANTS$MIN_MATURITY} to
#'   \code{HETID_CONSTANTS$MAX_MATURITY}).
#'   Default is the annual nodes \code{seq(12, 120, by = 12)}
#'   (\code{HETID_CONSTANTS$DEFAULT_ACM_MATURITIES}); pass
#'   \code{HETID_CONSTANTS$ALL_ACM_MATURITIES} for the full monthly
#'   grid (GitHub source only).
#' @param start_date Date or character string (YYYY-MM-DD format) for start of sample.
#'   Default is NULL (earliest available date).
#' @param end_date Date or character string (YYYY-MM-DD format) for end of sample.
#'   Default is NULL (latest available date).
#' @param frequency Character string: "monthly" (default), "quarterly",
#'   or "daily". Quarterly data uses the last observation of each
#'   quarter (derived from the monthly source asset). "daily" extracts
#'   the release's business-day asset instead; it is download-only
#'   (~40 MB; run \code{download_term_premia(frequency = "daily")} or
#'   set \code{auto_download = TRUE}) and not available from the NY Fed
#'   source.
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
#' The raw ACM data carries maturities at one-month steps from
#' \code{HETID_CONSTANTS$MIN_MATURITY} to
#' \code{HETID_CONSTANTS$MAX_MATURITY} months. Whole-year maturities keep
#' the official column names (ACMY01-ACMY10, ACMTP01-ACMTP10,
#' ACMRNY01-ACMRNY10); sub-annual
#' months use names like ACMY003M. The NY Fed fallback source provides
#' only the annual nodes; requesting sub-annual maturities against it
#' raises a structured error. The daily asset carries the identical
#' column schema at business-day frequency; there is no daily-to-monthly
#' or daily-to-quarterly aggregation.
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
                             frequency = c("monthly", "quarterly", "daily"),
                             auto_download = FALSE,
                             use_incomplete_quarters =
                               HETID_CONSTANTS$USE_INCOMPLETE_QUARTERS,
                             source = c("auto", "github", "nyfed")) {
  frequency <- match.arg(frequency)
  source <- match.arg(source)
  validate_acm_extract_inputs(data_types, maturities, use_incomplete_quarters)

  # load_term_premia raises hetid_error_insufficient_data when unavailable
  acm_data <- load_term_premia(
    auto_download = auto_download, source = source,
    frequency = if (frequency == "daily") "daily" else "monthly"
  )

  # Annual-only sources cannot serve month-level requests
  assert_subannual_available(acm_data, maturities)

  acm_data <- normalize_acm_date_column(acm_data)

  start_date <- coerce_optional_date(start_date, "start_date")
  end_date <- coerce_optional_date(end_date, "end_date")

  acm_data <- filter_acm_date_range(
    acm_data,
    start_date,
    end_date
  )

  col_mapping <- build_acm_col_mapping(data_types, maturities) # nolint: object_usage_linter

  # Fail closed rather than silently returning a narrower frame
  old_names <- unlist(col_mapping, use.names = FALSE)
  missing_cols <- old_names[!old_names %in% names(acm_data)]
  if (length(missing_cols) > 0) {
    stop_insufficient_data(paste0(
      "ACM data is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      ". The source file may be incomplete or corrupt."
    ))
  }

  selected <- acm_data[old_names]
  names(selected) <- names(col_mapping)
  result <- data.frame(
    date = acm_data$date, selected,
    check.names = FALSE, stringsAsFactors = FALSE
  )

  if (frequency == "quarterly") {
    result <- convert_to_quarterly(result, use_incomplete_quarters) # nolint: object_usage_linter
  }

  # Idempotent for the quarterly path (convert_to_quarterly already
  # normalized); the identity for the daily path
  result$date <- to_period_end(result$date, frequency)

  result <- result[order(result$date), , drop = FALSE]
  rownames(result) <- NULL

  result
}
