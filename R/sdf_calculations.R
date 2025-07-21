#' Compute Price News
#'
#' Computes the time series of price news Delta_(t+1)p_(t+i)^(1) or Delta_(t+1)y_(t+i)^(1)
#'
#' @param yields Data frame with columns y1, y2, ..., containing yields
#' @param term_premia Data frame with columns tp1, tp2, ..., containing term premia
#' @param i Integer, the horizon (must be >= 1)
#' @param return_yield_news Logical, if TRUE returns yield news instead of log price news
#' @param return_df Logical, if TRUE returns a data frame with dates (default FALSE)
#' @param dates Optional vector of dates corresponding to the rows in yields/term_premia.
#'   If not provided and return_df = TRUE, will use row indices.
#'
#' @return Numeric vector of price news, or data frame with dates if return_df = TRUE
#'
#' @details
#' The price news for log prices is:
#' Delta_(t+1)p_(t+i)^(1) = n_hat(i-1,t+1) - n_hat(i,t)
#'
#' The price news for yields is:
#' Delta_(t+1)y_(t+i)^(1) = -Delta_(t+1)p_(t+i)^(1)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#'
#' # Compute price news for i=5 (log price news)
#' price_news_5 <- compute_price_news(
#'   yields = data[, grep("^y", names(data))],
#'   term_premia = data[, grep("^tp", names(data))],
#'   i = 5
#' )
#'
#' # Compute price news for yields with dates
#' yield_news_5_df <- compute_price_news(
#'   yields = data[, grep("^y", names(data))],
#'   term_premia = data[, grep("^tp", names(data))],
#'   i = 5,
#'   return_yield_news = TRUE,
#'   return_df = TRUE,
#'   dates = data$date
#' )
#' }
#'
compute_price_news <- function(yields, term_premia, i, return_yield_news = FALSE, return_df = FALSE, dates = NULL) {
  if (i < 1) {
    stop("i must be >= 1")
  }

  # Compute n_hat series
  n_hat_i <- compute_n_hat(yields, term_premia, i, return_df = FALSE, dates = dates) # nolint

  # Special case for i=1: n_hat_0 = -y1
  if (i == 1) {
    # For i=1, n_hat(0,t) = E_t\[p_(t+0)^(1)\] = p_t^(1) = -y_t^(1)
    y1 <- yields[["y1"]]
    if (is.null(y1)) {
      stop("y1 column not found in yields")
    }
    n_hat_i_minus_1 <- -y1 / 100 # Convert to decimal
  } else {
    n_hat_i_minus_1 <- compute_n_hat(yields, term_premia, i - 1, return_df = FALSE, dates = dates) # nolint
  }

  # Number of observations
  n_obs <- length(n_hat_i)

  # Initialize result
  price_news <- rep(NA, n_obs - 1)

  # Compute news: n_hat(i-1,t+1) - n_hat(i,t)
  for (t in 1:(n_obs - 1)) {
    if (!is.na(n_hat_i_minus_1[t + 1]) && !is.na(n_hat_i[t])) {
      price_news[t] <- n_hat_i_minus_1[t + 1] - n_hat_i[t]
    }
  }

  # Return yield news if requested
  if (return_yield_news) {
    price_news <- -price_news
  }

  # Return data frame with dates if requested
  if (return_df) {
    # Use provided dates, or create generic time index
    if (is.null(dates)) {
      dates <- 1:nrow(yields)
    }

    # Ensure dates is the same length as the data
    if (length(dates) != nrow(yields)) {
      stop("Length of dates must match number of rows in yields")
    }

    # Create output vector with proper alignment
    # price_news[t] represents news from t to t+1, so it aligns with date t+1
    # First observation is NA because we can't compute news for t=0 to t=1
    price_news_aligned <- c(NA, price_news)

    return(data.frame(
      date = dates,
      price_news = price_news_aligned,
      stringsAsFactors = FALSE
    ))
  }

  price_news
}


#' Compute SDF Innovations Time Series
#'
#' Computes the time series of SDF innovations given by:
#' exp(n_hat(i,t)) * (Delta_(t+1)p_(t+i)^(1) + 0.5*(Delta_(t+1)p_(t+i)^(1))^2 -
#' 0.5*E\\[(Delta_(t+1)p_(t+i)^(1))^2\\])
#'
#' @param yields Data frame with columns y1, y2, ..., containing yields
#' @param term_premia Data frame with columns tp1, tp2, ..., containing term premia
#' @param i Integer, the horizon (must be >= 1)
#' @param return_df Logical, if TRUE returns a data frame with dates (default FALSE)
#' @param dates Optional vector of dates corresponding to the rows in yields/term_premia.
#'   If not provided and return_df = TRUE, will use row indices.
#'
#' @return Numeric vector of SDF innovations, or data frame with dates if return_df = TRUE
#'
#' @details
#' The SDF innovation is computed as:
#' exp(n_hat(i,t)) * (Delta_(t+1)p_(t+i)^(1) + 0.5*(Delta_(t+1)p_(t+i)^(1))^2 -
#' 0.5*E\\[(Delta_(t+1)p_(t+i)^(1))^2\\])
#'
#' Where:
#' - Delta_(t+1)p_(t+i)^(1) = n_hat(i-1,t+1) - n_hat(i,t)
#' - E\\[(Delta_(t+1)p_(t+i)^(1))^2\\] = (1/(T-1)) * sum(n_hat(i-1,t+1) - n_hat(i,t))^2
#'   for t=1 to T-1
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#'
#' # Compute SDF innovations for i=5
#' sdf_innovations_5 <- compute_sdf_innovations(
#'   yields = data[, grep("^y", names(data))],
#'   term_premia = data[, grep("^tp", names(data))],
#'   i = 5
#' )
#'
#' # Compute SDF innovations with dates
#' sdf_innovations_5_df <- compute_sdf_innovations(
#'   yields = data[, grep("^y", names(data))],
#'   term_premia = data[, grep("^tp", names(data))],
#'   i = 5,
#'   return_df = TRUE,
#'   dates = data$date
#' )
#' }
#'
compute_sdf_innovations <- function(yields, term_premia, i, return_df = FALSE, dates = NULL) {
  if (i < 1) {
    stop("i must be >= 1")
  }

  # Compute n_hat(i,t) series
  n_hat_i <- compute_n_hat(yields, term_premia, i, return_df = FALSE, dates = dates) # nolint

  # Compute price news (Delta_(t+1)p_(t+i)^(1))
  delta_p <- compute_price_news(yields, term_premia, i, return_yield_news = FALSE, return_df = FALSE, dates = dates)

  # Compute E[(Delta_(t+1)p_(t+i)^(1))^2]
  # Remove NA values for the expectation calculation
  delta_p_clean <- delta_p[!is.na(delta_p)]
  if (length(delta_p_clean) == 0) {
    stop("No valid SDF news values to compute expectation")
  }

  expected_delta_p_squared <- mean(delta_p_clean^2)

  # Number of observations (T-1 since delta_p has T-1 elements)
  t_minus_1 <- length(delta_p)

  # Initialize result
  sdf_innovations <- rep(NA, t_minus_1)

  # Compute SDF innovations
  for (t in 1:t_minus_1) {
    if (!is.na(n_hat_i[t]) && !is.na(delta_p[t])) {
      sdf_innovations[t] <- exp(n_hat_i[t]) *
        (delta_p[t] + 0.5 * delta_p[t]^2 - 0.5 * expected_delta_p_squared)
    }
  }

  # Return data frame with dates if requested
  if (return_df) {
    # Use provided dates, or create generic time index
    if (is.null(dates)) {
      dates <- 1:nrow(yields)
    }

    # Ensure dates is the same length as the data
    if (length(dates) != nrow(yields)) {
      stop("Length of dates must match number of rows in yields")
    }

    # SDF innovations have same alignment as price news: first observation is NA
    sdf_innovations_aligned <- c(NA, sdf_innovations)

    return(data.frame(
      date = dates,
      sdf_innovations = sdf_innovations_aligned,
      stringsAsFactors = FALSE
    ))
  }

  sdf_innovations
}
