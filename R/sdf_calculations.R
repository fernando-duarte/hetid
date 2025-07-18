#' Compute SDF News (Innovations)
#'
#' Computes the time series of SDF news Delta_(t+1)p_(t+i)^(1) or Delta_(t+1)y_(t+i)^(1)
#'
#' @param yields Data frame with columns y1, y2, ..., containing yields
#' @param term_premia Data frame with columns tp1, tp2, ..., containing term premia
#' @param i Integer, the horizon (must be >= 1)
#' @param return_yield_news Logical, if TRUE returns yield news instead of log price news
#'
#' @return Numeric vector of SDF news
#'
#' @details
#' The SDF news for log prices is:
#' Delta_(t+1)p_(t+i)^(1) = n_hat(i-1,t+1) - n_hat(i,t)
#'
#' The SDF news for yields is:
#' Delta_(t+1)y_(t+i)^(1) = -Delta_(t+1)p_(t+i)^(1)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#'
#' # Compute SDF news for i=5 (log price news)
#' sdf_news_5 <- compute_sdf_news(
#'   yields = data[, grep("^y", names(data))],
#'   term_premia = data[, grep("^tp", names(data))],
#'   i = 5
#' )
#'
#' # Compute SDF news for yields
#' sdf_yield_news_5 <- compute_sdf_news(
#'   yields = data[, grep("^y", names(data))],
#'   term_premia = data[, grep("^tp", names(data))],
#'   i = 5,
#'   return_yield_news = TRUE
#' )
#' }
#'
compute_sdf_news <- function(yields, term_premia, i, return_yield_news = FALSE) {
  if (i < 1) {
    stop("i must be >= 1")
  }

  # Compute n_hat series
  n_hat_i <- compute_n_hat(yields, term_premia, i)

  # Special case for i=1: n_hat_0 = -y1
  if (i == 1) {
    # For i=1, n_hat(0,t) = E_t\[p_(t+0)^(1)\] = p_t^(1) = -y_t^(1)
    y1 <- yields[["y1"]]
    if (is.null(y1)) {
      stop("y1 column not found in yields")
    }
    n_hat_i_minus_1 <- -y1 / 100 # Convert to decimal
  } else {
    n_hat_i_minus_1 <- compute_n_hat(yields, term_premia, i - 1)
  }

  # Number of observations
  n_obs <- length(n_hat_i)

  # Initialize result
  sdf_news <- rep(NA, n_obs - 1)

  # Compute news: n_hat(i-1,t+1) - n_hat(i,t)
  for (t in 1:(n_obs - 1)) {
    if (!is.na(n_hat_i_minus_1[t + 1]) && !is.na(n_hat_i[t])) {
      sdf_news[t] <- n_hat_i_minus_1[t + 1] - n_hat_i[t]
    }
  }

  # Return yield news if requested
  if (return_yield_news) {
    sdf_news <- -sdf_news
  }

  sdf_news
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
#'
#' @return Numeric vector of SDF innovations
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
#' }
#'
compute_sdf_innovations <- function(yields, term_premia, i) {
  if (i < 1) {
    stop("i must be >= 1")
  }

  # Compute n_hat(i,t) series
  n_hat_i <- compute_n_hat(yields, term_premia, i)

  # Compute SDF news (Delta_(t+1)p_(t+i)^(1))
  delta_p <- compute_sdf_news(yields, term_premia, i, return_yield_news = FALSE)

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

  sdf_innovations
}
