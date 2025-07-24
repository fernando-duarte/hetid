#' Compute Expected Log Bond Price Estimator (n_hat)
#'
#' Computes the time series n_hat(i,t) which is an estimator for
#' E_t\[p_(t+i)^(1)\] = -E_t\[y_(t+i)^(1)\]
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-return-df-dates
#'
#' @template return-numeric-or-dataframe
#'
#' @details
#' The formula is:
#' n_hat(i,t) = i*y_t^(i) - (i+1)*y_t^(i+1) + (i+1)*TP_t^(i+1) - i*TP_t^(i)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#'
#' # Compute n_hat for i=5
#' n_hat_5 <- compute_n_hat(
#'   yields = data[, grep("^y", names(data))],
#'   term_premia = data[, grep("^tp", names(data))],
#'   i = 5
#' )
#'
#' # Compute n_hat with dates
#' n_hat_5_df <- compute_n_hat(
#'   yields = data[, grep("^y", names(data))],
#'   term_premia = data[, grep("^tp", names(data))],
#'   i = 5,
#'   return_df = TRUE,
#'   dates = data$date
#' )
#' }
#'
compute_n_hat <- function(yields, term_premia, i, return_df = FALSE, dates = NULL) {
  if (i < HETID_CONSTANTS$MIN_MATURITY) {
    stop("i must be >= ", HETID_CONSTANTS$MIN_MATURITY)
  }

  # Extract relevant columns
  y_i <- yields[[paste0("y", i)]]
  y_i_plus_1 <- yields[[paste0("y", i + 1)]]
  tp_i <- term_premia[[paste0("tp", i)]]
  tp_i_plus_1 <- term_premia[[paste0("tp", i + 1)]]

  # Check for missing columns
  if (is.null(y_i) || is.null(y_i_plus_1)) {
    stop("Required yield columns not found for i = ", i)
  }
  if (is.null(tp_i) || is.null(tp_i_plus_1)) {
    stop("Required term premia columns not found for i = ", i)
  }

  # Compute n_hat
  n_hat <- i * y_i - (i + 1) * y_i_plus_1 + (i + 1) * tp_i_plus_1 - i * tp_i

  # Convert percentages to decimals (ACM data is in percentage points)
  n_hat <- n_hat / HETID_CONSTANTS$PERCENT_TO_DECIMAL

  # Return data frame with dates if requested
  if (return_df) {
    # Use provided dates, or create generic time index
    if (is.null(dates)) {
      dates <- seq_len(nrow(yields))
    }

    # Ensure dates is the same length as the data
    if (length(dates) != length(n_hat)) {
      stop("Length of dates must match number of rows in yields")
    }

    return(data.frame(
      date = dates,
      n_hat = n_hat,
      stringsAsFactors = FALSE
    ))
  }

  n_hat
}
