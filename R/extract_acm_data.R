#' Extract ACM Term Structure Data
#'
#' Extracts and processes ACM (Adrian, Crump, and Moench) term structure data,
#' allowing selection of specific data types, maturities, and date ranges.
#' Optionally converts monthly data to quarterly frequency.
#'
#' @param data_types Nonempty character vector without duplicates or missing values.
#'   Options: \code{"yields"}, \code{"term_premia"}, \code{"risk_neutral_yields"}.
#'   Default is \code{c("yields", "term_premia")}.
#' @param maturities Nonempty numeric vector of distinct, finite integer maturities in months
#'   (\code{HETID_CONSTANTS$MIN_MATURITY} to
#'   \code{HETID_CONSTANTS$MAX_MATURITY}).
#'   Default is the annual nodes in \code{HETID_CONSTANTS$DEFAULT_ACM_MATURITIES}; pass
#'   \code{HETID_CONSTANTS$ALL_ACM_MATURITIES} for the full monthly
#'   grid (GitHub source only).
#' @param start_date Single \code{Date} or character string (YYYY-MM-DD format) for
#'   the inclusive sample start, compared with month-end labels (daily dates for daily data).
#'   Default is \code{NULL} (earliest available date).
#' @param end_date Single \code{Date} or character string (YYYY-MM-DD format) for
#'   the inclusive sample end, compared with raw observation dates before relabeling.
#'   Default is \code{NULL} (latest available date).
#' @param frequency Character string: "monthly" (default), "quarterly",
#'   or "daily". Quarterly data uses the last observation of each
#'   quarter (derived from the monthly source asset). "daily" extracts
#'   the release's business-day asset instead; it is download-only
#'   (run \code{download_term_premia(frequency = "daily")} or
#'   set \code{auto_download = TRUE}) and not available from the NY Fed
#'   source.
#' @param auto_download Nonmissing logical scalar. If \code{TRUE} and data doesn't exist,
#'   downloads it to the per-user data directory. Default is \code{FALSE}.
#' @param use_incomplete_quarters Nonmissing logical scalar, only used when
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
#'   \code{"github"} (same resolution), or \code{"nyfed"} (explicitly downloaded NY Fed
#'   cache only, annual maturities).
#'
#' @template acm-pin
#'
#' @return A data frame sorted by \code{date}, with a \code{Date} column followed by
#'   numeric columns grouped in \code{data_types} order, then \code{maturities} order.
#'   Names use \code{y}, \code{tp}, or \code{rny} followed by the maturity in months
#'   (for example, \code{y60}, \code{tp60}, \code{rny60}). Monthly and quarterly dates
#'   are calendar period ends; daily dates retain their observation dates.
#'   A zero-row data frame with the same columns is returned if no dates survive filtering
#'   or all quarters are dropped. Missing numeric values are retained.
#'
#' @details
#' The raw ACM data carries maturities at one-month steps from
#' \code{HETID_CONSTANTS$MIN_MATURITY} to
#' \code{HETID_CONSTANTS$MAX_MATURITY} months. Whole-year maturities keep
#' the official column names (ACMY01-ACMY10, ACMTP01-ACMTP10,
#' ACMRNY01-ACMRNY10); other maturities use month-suffixed names
#' like ACMY003M. The NY Fed fallback source provides
#' only the annual nodes; requesting other maturities against it
#' raises a structured error. The daily asset carries the identical
#' column schema at business-day frequency; there is no daily-to-monthly
#' or daily-to-quarterly aggregation.
#'
#' All values are in annualized percentage points.
#'
#' Date bounds are applied before quarterly conversion. Relabeling can place an output
#' date after \code{end_date}, including when an incomplete quarter is retained.
#' A missing (\code{NA}) date bound selects no rows.
#' Rows with missing dates are dropped; partially unparseable source dates also raise
#' a \code{hetid_warning_unparsed_dates} warning. Pinned reads reject missing dates.
#' Missing data or requested columns raise \code{hetid_error_insufficient_data};
#' invalid maturity, data-type, or logical inputs raise \code{hetid_error_bad_argument}.
#'
#' By construction: Term Premium = Yield - Risk-Neutral Yield
#'
#' @export
#'
#' @examples
#' # Yields and term premia at one annual maturity
#' data <- extract_acm_data(maturities = 12)
#' head(data)
#'
#' # Extract only 2-year and 10-year yields for specific period
#' data <- extract_acm_data(
#'   data_types = "yields",
#'   maturities = c(24, 120),
#'   start_date = "2010-01-01",
#'   end_date = "2020-12-31"
#' )
#'
#' # Quarterly term premia for a complete calendar year
#' data <- extract_acm_data(
#'   data_types = "term_premia",
#'   maturities = 60,
#'   start_date = "2020-01-01",
#'   end_date = "2020-12-31",
#'   frequency = "quarterly",
#'   use_incomplete_quarters = FALSE
#' )
#' data
#'
#' # Extract all three data types for the 5-year (60-month) maturity
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia", "risk_neutral_yields"),
#'   maturities = 60
#' )
#'
#' # Non-whole-year maturities from the monthly grid
#' data <- extract_acm_data(
#'   data_types = "yields",
#'   maturities = c(6, 18)
#' )
#' names(data)
extract_acm_data <- function(data_types = c("yields", "term_premia"),
                             maturities = HETID_CONSTANTS$DEFAULT_ACM_MATURITIES,
                             start_date = NULL,
                             end_date = NULL,
                             frequency = c("monthly", "quarterly", "daily"),
                             auto_download = FALSE,
                             use_incomplete_quarters =
                               HETID_CONSTANTS$USE_INCOMPLETE_QUARTERS,
                             source = c("auto", "github", "nyfed"),
                             release = NULL, expected_sha256 = NULL) {
  frequency <- match.arg(frequency)
  source <- match.arg(source)
  validate_acm_extract_inputs(data_types, maturities, use_incomplete_quarters)

  acm_data <- load_term_premia(
    auto_download = auto_download, source = source,
    frequency = if (frequency == "daily") "daily" else "monthly",
    release = release, expected_sha256 = expected_sha256
  )

  assert_subannual_available(acm_data, maturities)

  acm_data <- normalize_acm_date_column(acm_data)

  start_date <- coerce_optional_date(start_date, "start_date")
  end_date <- coerce_optional_date(end_date, "end_date")

  acm_data <- filter_acm_date_range(
    acm_data,
    start_date,
    end_date,
    frequency = if (frequency == "daily") "daily" else "monthly"
  )

  col_mapping <- build_acm_col_mapping(data_types, maturities) # nolint: object_usage_linter

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

  # Dates already normalized by quarterly conversion stay unchanged here
  result$date <- to_period_end(result$date, frequency)

  result <- result[order(result$date), , drop = FALSE]
  rownames(result) <- NULL

  result
}
