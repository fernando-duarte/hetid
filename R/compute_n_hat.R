#' Compute Expected Log Bond Price Estimator (n_hat)
#'
#' Computes the time series n_hat(i,t) which is an estimator for
#' E_t\[p_(t+i)^(1)\] = -E_t\[y_(t+i)^(1)\]
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-return-df-dates
#' @template param-step
#'
#' @template return-numeric-or-dataframe
#'
#' @details
#' With maturity weights in years, m(i) = i / MATURITY_UNITS_PER_YEAR,
#' the formula is:
#' n_hat(i,t) = m(i)*y_t^(i) - m(i+step)*y_t^(i+step) +
#' m(i+step)*TP_t^(i+step) - m(i)*TP_t^(i)
#'
#' Under the default step and unit (both 1) this reduces to the classic
#' i*y_t^(i) - (i+1)*y_t^(i+1) + (i+1)*TP_t^(i+1) - i*TP_t^(i).
#'
#' @note The effective maximum for \code{i} is
#'   \code{MAX_MATURITY - step} (9 for standard ACM data with the
#'   default step), because this function requires data at maturity
#'   \code{i + step}.
#'
#' @export
#'
#' @examples
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#'
#' # Compute n_hat for i=5
#' n_hat_5 <- compute_n_hat(
#'   yields = data[, paste0("y", 1:10)],
#'   term_premia = data[, paste0("tp", 1:10)],
#'   i = 5
#' )
#'
#' # Compute n_hat with dates
#' n_hat_5_df <- compute_n_hat(
#'   yields = data[, paste0("y", 1:10)],
#'   term_premia = data[, paste0("tp", 1:10)],
#'   i = 5,
#'   return_df = TRUE,
#'   dates = data$date
#' )
#'
compute_n_hat <- function(yields, term_premia, i, return_df = FALSE, dates = NULL,
                          step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_step(step)
  validate_maturity_index(i, max_maturity = effective_max_maturity(step))
  validate_row_alignment(yields, term_premia)

  # Extract relevant columns
  y_i <- require_column(yields, acm_column_name("yields", i), "yields")
  y_next <- require_column(
    yields, acm_column_name("yields", i + step), "yields"
  )
  tp_i <- require_column(
    term_premia, acm_column_name("term_premia", i), "term_premia"
  )
  tp_next <- require_column(
    term_premia, acm_column_name("term_premia", i + step), "term_premia"
  )

  # Maturity weights in years: annualized yields scale by maturity in
  # years to form log prices
  m_i <- i / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  m_next <- (i + step) / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR

  # Compute n_hat
  n_hat <- m_i * y_i - m_next * y_next + m_next * tp_next - m_i * tp_i

  # Convert percentages to decimals (ACM data is in percentage points)
  n_hat <- n_hat / HETID_CONSTANTS$PERCENT_TO_DECIMAL

  prepare_return_data(
    n_hat, return_df, dates, yields, "n_hat"
  )
}
