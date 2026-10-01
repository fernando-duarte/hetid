#' Compute SDF Innovations Time Series
#'
#' Computes stochastic discount factor (SDF) innovations, the centered second-order
#' approximation to the SDF news:
#' \deqn{e^{\hat{n}(i,t)}\left(\Delta_{t+1}p^{(1)}_{t+i} +
#'   \tfrac{1}{2}\left(\Delta_{t+1}p^{(1)}_{t+i}\right)^{2}\right) - B_i}
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-dates-required
#' @template param-step
#'
#' @return A data frame with \code{nrow(yields)} rows and two columns:
#'   \code{date}, the supplied \code{Date} vector, and \code{sdf_innovations},
#'   the numeric news series. News from row \code{k} to row \code{k + 1}
#'   carries \code{dates[k + 1]}; the first value is \code{NA_real_}.
#'
#' @details
#' In the formula above:
#' - \eqn{\Delta_{t+1}p^{(1)}_{t+i} = \hat{n}(i-step,t+1) - \hat{n}(i,t)}
#' - \eqn{B_i = \tfrac{1}{2}\mathrm{mean}(e^{\hat{n}(i,t)}
#'   (\Delta_{t+1}p^{(1)}_{t+i})^{2})} is the
#'   constant centering term, an exponential-weighted sample mean
#'   subtracted outside the \eqn{e^{\hat{n}}} factor. The mean runs over
#'   non-missing news and exponential weights (T-1 terms with complete data).
#'   If price news has zero conditional mean, the population analogue has
#'   zero unconditional mean. The full series need not have zero sample mean.
#'
#' Supply yields and term premia in annualized percentage points, with rows
#' aligned to the same dates and ordered chronologically. Adjacent rows must
#' be one news period apart, matching \code{step} months; the default step
#' therefore uses annual observations. The function does not reorder rows,
#' aggregate data, or normalize dates. See \code{\link{compute_n_hat}} for
#' the step-period term-premium convention required by the construction.
#'
#' The maturity must satisfy \code{i == step} or
#' \code{i - step >= HETID_CONSTANTS$MIN_MATURITY}, as well as the upper
#' bound below; it need not be a multiple of \code{step}. Supply columns at
#' \code{i - step}, \code{i}, and \code{i + step}. At \code{i == step},
#' only \code{i} and \code{i + step} are needed: the previous leg is the
#' realized log price of the step-maturity bond, and its term premium is
#' set to zero. The step must be a positive integer no greater than
#' \code{HETID_CONSTANTS$MAX_MATURITY %/% 2}.
#'
#' Missing inputs propagate to the affected news values and are excluded
#' from the centering mean. At least two input rows and one non-missing
#' news-weight pair are needed. A single row or all-missing news raises
#' \code{hetid_error_insufficient_data}. Invalid maturities, steps, missing
#' columns, or invalid dates
#' raise \code{hetid_error_bad_argument}; row or date-length mismatches raise
#' \code{hetid_error_dimension_mismatch}. Dates cannot be missing or
#' \code{NULL}, despite the default in the signature. Non-missing yield
#' magnitudes below one throughout trigger a \code{hetid_warning_unit_scale} warning
#' about possible decimal units; no automatic unit conversion is performed.
#'
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step},
#'   because this function requires data at maturity \code{i + step}.
#'
#' @note The returned series is a news series, so its first row value is
#'   always \code{NA_real_}: there are T rows but only T-1 news observations.
#'
#' @export
#'
#' @examples
#' # Monthly data and step match the ACM rollover convention
#' mats <- c(59, 60, 61)
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"), maturities = mats
#' )
#' sdf_innovations_60 <- compute_sdf_innovations(
#'   data[, paste0("y", mats)], data[, paste0("tp", mats)],
#'   i = 60, step = 1, dates = data$date
#' )
#' head(sdf_innovations_60)
compute_sdf_innovations <- function(yields, term_premia, i, dates = NULL,
                                    step = HETID_CONSTANTS$DEFAULT_STEP) {
  prepare_return_data(
    sdf_innovations_series(yields, term_premia, i, step = step),
    dates, yields, "sdf_innovations",
    is_news = TRUE
  )
}

#' Bare SDF-Innovation News Series (internal numeric kernel)
#'
#' The undated numeric core of \code{\link{compute_sdf_innovations}}, returning
#' the T-1 centered news vector consumed by \code{process_w2_maturity}. Holds the
#' input validation so the bare and dated paths are checked identically.
#'
#' @inheritParams compute_sdf_innovations
#' @return Numeric vector of T-1 centered SDF innovations.
#' @noRd
sdf_innovations_series <- function(yields, term_premia, i,
                                   step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_row_alignment(yields, term_premia)

  components <- compute_news_components(yields, term_premia, i, step = step)
  n_hat_i <- components[["n_hat_i"]]
  delta_p <- components$delta_p

  exp_mu <- exp(n_hat_i[seq_along(delta_p)])
  valid <- !is.na(exp_mu) & !is.na(delta_p)
  assert_insufficient_data_ok(
    any(valid),
    "No valid SDF news values to compute expectation"
  )
  b_hat <- 0.5 * mean(exp_mu[valid] * delta_p[valid]^2)

  exp_mu * (delta_p + 0.5 * delta_p^2) - b_hat
}
