#' Resolve the W2 realization-date index from full-T input dates
#'
#' The W2 residual \eqn{W_{2,t+1}} is a one-step news object realized at
#' \eqn{t+1}, so the caller supplies one period-end date per yield row (length
#' \code{nrow(yields)}, the same convention as \code{compute_w1_residuals}) and
#' this helper shifts to the lead realization dates \code{dates[-1]} =
#' \eqn{d_2, \ldots, d_T} (length \code{nrow(yields) - 1}, the W2 news count).
#' Real dates are mandatory: there is no row-index fallback.
#'
#' @param dates Required full-T \code{Date} vector (one per yield row).
#' @param n_yield_rows Integer, the number of rows in the yields data frame.
#'
#' @return The \eqn{t+1} realization \code{Date} index of the W2 observations,
#'   before any per-maturity complete-cases subsetting.
#'
#' @noRd
resolve_w2_dates <- function(dates, n_yield_rows) {
  validate_dates_vector(dates, n_yield_rows)
  dates[-1L]
}
