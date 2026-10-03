#' Compute a Dated SDF Horizon Panel
#'
#' Assembles expected SDF or SDF-news series over explicit horizons using the
#' existing scalar estimators, retaining each horizon's dates and missing values.
#'
#' @param yields,term_premia Data frames with their own \code{Date} column
#'   named \code{date} and numeric ACM columns named \code{yN} and \code{tpN}.
#'   Supply annualized percentage points and identical, unique, ordered date keys.
#' @param horizons A nonempty numeric vector of distinct finite integer horizons
#'   in months, in the desired output order. Expected SDF admits zero; news does not.
#' @template param-step
#' @param type A character string: \code{"expected"} (default) or \code{"news"}.
#' @param paired A nonmissing logical scalar. For expected SDF, \code{TRUE}
#'   selects the paired bias correction and requires positive horizons to be step
#'   multiples. The default \code{FALSE} uses the unpaired correction.
#'   News requires \code{FALSE}.
#'
#' @return A list with \code{data}, \code{horizons}, \code{step}, \code{type},
#'   and \code{paired}. The data frame contains \code{date} followed by
#'   \code{expected_sdf_mN} or \code{sdf_news_mN} columns in horizon order.
#'   Horizon names are removed from the metadata vector. Expected SDF is dated
#'   at formation; news is dated at realization and retains its initial NA.
#'
#' @details
#' Dates must be calendar month ends, quarter ends when the step is quarterly,
#' or year ends when annual. Consecutive rows must be exactly \code{step} months
#' apart. The panels are validated by their date keys; they are not sorted,
#' intersected, interpolated, restamped or aggregated. Invalid dates, horizons
#' or columns raise \code{hetid_error_bad_argument}; unequal row counts raise
#' \code{hetid_error_dimension_mismatch}. Empty or insufficient samples raise
#' \code{hetid_error_insufficient_data}. Scalar estimator warnings propagate.
#'
#' Expected horizon zero is the exact observed step-bond price, with its existing
#' horizon-zero warning, rather than a fitted positive-horizon expectation.
#' Valid non-step-multiple horizons are retained for news and unpaired expected
#' SDF. No default horizon grid or common finite-row sample is imposed.
#' Term-premium rollover conventions must match the news step; changing the
#' step or aggregating yields does not change those conventions.
#'
#' Quarterly means for descriptive yield series and last observations for SDF
#' inputs are separate choices. Use \code{\link{aggregate_quarterly}} with an
#' explicit method before assembly, rather than inferring a method from the step.
#' The paired expected-SDF variance bound corresponds to \code{paired = TRUE},
#' not to the default unpaired expected panel.
#'
#' @seealso \code{\link{compute_expected_sdf}}, \code{\link{compute_sdf_innovations}}
#' @export
#' @examples
#' acm <- extract_acm_data(maturities = c(1, 2), auto_download = FALSE)
#' compute_sdf_panel(acm[c("date", "y1", "y2")],
#'   acm[c("date", "tp1", "tp2")],
#'   horizons = 1, step = 1
#' )
compute_sdf_panel <- function(yields, term_premia, horizons,
                              step = HETID_CONSTANTS$DEFAULT_STEP,
                              type = c("expected", "news"), paired = FALSE) {
  type <- tryCatch(match.arg(type), error = function(e) {
    stop_bad_argument(conditionMessage(e), "type")
  })
  validate_sdf_panel_inputs(yields, term_premia, horizons, step, type, paired)
  dates <- yields$date
  yields <- yields[setdiff(names(yields), "date")]
  term_premia <- term_premia[setdiff(names(term_premia), "date")]
  panel_data <- data.frame(date = dates)
  prefix <- if (type == "expected") "expected_sdf_m" else "sdf_news_m"
  for (i in horizons) {
    result <- if (type == "expected") {
      compute_expected_sdf(yields, term_premia, i, dates, step, paired)
    } else {
      compute_sdf_innovations(yields, term_premia, i, dates, step)
    }
    panel_data[[paste0(prefix, i)]] <- result[[2L]]
  }
  list(data = panel_data, horizons = unname(horizons), step = step, type = type, paired = paired)
}
