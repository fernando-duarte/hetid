#' Compute SDF Innovations Time Series
#'
#' Computes the time series of SDF innovations, the centered second-order
#' approximation to the SDF news:
#' exp(n_hat(i,t)) * (Delta_(t+1)p_(t+i)^(1) + 0.5*(Delta_(t+1)p_(t+i)^(1))^2)
#' - B_i
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-dates-required
#' @template param-step
#'
#' @template return-dated-dataframe
#'
#' @details
#' The SDF innovation is the centered second-order approximation
#' exp(n_hat(i,t)) * (Delta_(t+1)p_(t+i)^(1) + 0.5*(Delta_(t+1)p_(t+i)^(1))^2)
#' - B_i
#'
#' Where:
#' - Delta_(t+1)p_(t+i)^(1) = n_hat(i-step,t+1) - n_hat(i,t)
#' - B_i = 0.5 * mean(exp(n_hat(i,t)) * (Delta_(t+1)p_(t+i)^(1))^2) is the
#'   constant centering term, an exponential-weighted sample mean
#'   subtracted outside the exp(n_hat) factor so the population analogue
#'   has exactly zero unconditional mean; the mean runs over the valid
#'   (non-missing) news dates (T-1 terms with complete data)
#'
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step}
#'   (108 for standard ACM data with the default annual step), because this
#'   function requires data at maturity \code{i + step}.
#'
#' @export
#'
#' @examples
#' # Extract ACM data - need maturities i-step, i, i+step (months)
#' # For i = 60 with the default annual step: 48, 60, 72
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(48, 60, 72)
#' )
#' yields <- data[, paste0("y", c(48, 60, 72))]
#' term_premia <- data[, paste0("tp", c(48, 60, 72))]
#'
#' # Compute SDF innovations for i = 60 (dated data frame)
#' sdf_innovations_60 <- compute_sdf_innovations(
#'   yields, term_premia,
#'   i = 60,
#'   dates = data$date
#' )
#'
compute_sdf_innovations <- function(yields, term_premia, i, dates = NULL,
                                    step = HETID_CONSTANTS$DEFAULT_STEP) {
  prepare_return_data(
    sdf_innovations_series(yields, term_premia, i, step = step),
    dates, yields, "sdf_innovations",
    is_news = TRUE
  )
}

#' Bare SDF-innovation news series (internal numeric kernel)
#'
#' The undated numeric core of \code{\link{compute_sdf_innovations}}, returning
#' the T-1 centered news vector consumed by \code{process_w2_maturity}. Holds the
#' input validation so the bare and dated paths are checked identically.
#'
#' @inheritParams compute_sdf_innovations
#' @return Numeric vector of T-1 centered SDF innovations.
#' @keywords internal
#' @noRd
sdf_innovations_series <- function(yields, term_premia, i,
                                   step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_row_alignment(yields, term_premia)

  # Shared maturity validation, n_hat(i,t) level, and price-news
  # difference. The t-alignment between exp(n_hat_i[t]) and delta_p[t]
  # holds: delta_p[t] = n_hat(i-step,t+1) - n_hat(i,t) pairs with n_hat_i[t].
  components <- compute_news_components(yields, term_premia, i, step = step)
  n_hat_i <- components$n_hat_i
  delta_p <- components$delta_p

  # Constant centering B_i = 0.5 * mean(exp(n_hat) * delta_p^2) over the
  # observed news dates, subtracted OUTSIDE the exp(n_hat) factor (spec
  # centering): the population analogue then has exactly zero mean.
  exp_mu <- exp(n_hat_i[seq_along(delta_p)])
  valid <- !is.na(exp_mu) & !is.na(delta_p)
  assert_insufficient_data_ok(
    any(valid),
    "No valid SDF news values to compute expectation"
  )
  b_hat <- 0.5 * mean(exp_mu[valid] * delta_p[valid]^2)

  # NA in either factor propagates to NA in the centered innovation
  exp_mu * (delta_p + 0.5 * delta_p^2) - b_hat
}
