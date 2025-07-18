#' Compute Expected Log Bond Price Estimator (n_hat)
#'
#' Computes the time series n_hat_{i,t} which is an estimator for E_t[p_{t+i}^{(1)}] = -E_t[y_{t+i}^{(1)}]
#'
#' @param yields Data frame with columns y1, y2, ..., containing yields
#' @param term_premia Data frame with columns tp1, tp2, ..., containing term premia
#' @param i Integer, the horizon (must be >= 1)
#'
#' @return Numeric vector of n_hat_{i,t} values
#'
#' @details
#' The formula is:
#' n_hat_{i,t} = i*y_t^{(i)} - (i+1)*y_t^{(i+1)} + (i+1)*TP_t^{(i+1)} - i*TP_t^{(i)}
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
#' }
#'
compute_n_hat <- function(yields, term_premia, i) {
  if (i < 1) {
    stop("i must be >= 1")
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
  n_hat / 100
}


#' Compute Fourth Moment Estimator (k_hat)
#'
#' Computes k_hat_i which estimates E[(p_{t+i}^{(1)} - E_{t+1}[p_{t+i}^{(1)}])^4]
#'
#' @param yields Data frame with columns y1, y2, ..., containing yields
#' @param term_premia Data frame with columns tp1, tp2, ..., containing term premia
#' @param i Integer, the horizon (must be >= 1)
#'
#' @return Numeric value of k_hat_i
#'
#' @details
#' The formula is:
#' k_hat_i = (1/(T-i)) * sum_{t=1}^{T-i} (-y_{t+i}^{(1)} - n_hat_{i-1,t+1})^4
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#'
#' # Compute k_hat for i=5
#' k_hat_5 <- compute_k_hat(
#'   yields = data[, grep("^y", names(data))],
#'   term_premia = data[, grep("^tp", names(data))],
#'   i = 5
#' )
#' }
#'
compute_k_hat <- function(yields, term_premia, i) {
  if (i < 1) {
    stop("i must be >= 1")
  }

  # Get y1 series
  y1 <- yields[["y1"]]
  if (is.null(y1)) {
    stop("y1 column not found in yields")
  }

  # Special case for i=1: n_hat_0 = -y1
  if (i == 1) {
    # For i=1, n_hat_{0,t} = E_t[p_{t+0}^{(1)}] = p_t^{(1)} = -y_t^{(1)}
    n_hat_i_minus_1 <- -y1 / 100 # Convert to decimal
  } else {
    # Compute n_hat_{i-1} series
    n_hat_i_minus_1 <- compute_n_hat(yields, term_premia, i - 1)
  }

  # Number of observations
  T <- length(y1)

  if (T <= i) {
    stop("Not enough observations. Need T > i")
  }

  # Compute the fourth moment
  # Need to align time indices properly
  sum_val <- 0
  count <- 0

  for (t in 1:(T - i)) {
    if (!is.na(y1[t + i]) && !is.na(n_hat_i_minus_1[t + 1])) {
      # Convert y1 from percentage to decimal
      term <- (-y1[t + i] / 100 - n_hat_i_minus_1[t + 1])^4
      sum_val <- sum_val + term
      count <- count + 1
    }
  }

  if (count == 0) {
    return(NA)
  }

  sum_val / count
}


#' Compute Supremum Estimator (c_hat)
#'
#' Computes c_hat_i which estimates sup_t exp(2*E_t[p_{t+i}^{(1)}])
#'
#' @param yields Data frame with columns y1, y2, ..., containing yields
#' @param term_premia Data frame with columns tp1, tp2, ..., containing term premia
#' @param i Integer, the horizon (must be >= 1)
#'
#' @return Numeric value of c_hat_i
#'
#' @details
#' The formula is:
#' c_hat_i = max_{1 <= t <= T} exp(2*n_hat_{i,t})
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#'
#' # Compute c_hat for i=5
#' c_hat_5 <- compute_c_hat(
#'   yields = data[, grep("^y", names(data))],
#'   term_premia = data[, grep("^tp", names(data))],
#'   i = 5
#' )
#' }
#'
compute_c_hat <- function(yields, term_premia, i) {
  if (i < 1) {
    stop("i must be >= 1")
  }

  # Compute n_hat series
  n_hat <- compute_n_hat(yields, term_premia, i)

  # Remove NA values
  n_hat_clean <- n_hat[!is.na(n_hat)]

  if (length(n_hat_clean) == 0) {
    return(NA)
  }

  # Compute maximum of exp(2*n_hat)
  max(exp(2 * n_hat_clean))
}


#' Compute Variance Bound
#'
#' Computes the empirical upper bound for Var(error_{i,t+1})
#'
#' @param yields Data frame with columns y1, y2, ..., containing yields
#' @param term_premia Data frame with columns tp1, tp2, ..., containing term premia
#' @param i Integer, the horizon (must be >= 1)
#'
#' @return Numeric value of the variance bound (1/4)*c_hat_i*k_hat_i
#'
#' @details
#' The variance bound is:
#' Var(error_{i,t+1}) <= (1/4)*c_hat_i*k_hat_i
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#'
#' # Compute variance bound for i=5
#' var_bound_5 <- compute_variance_bound(
#'   yields = data[, grep("^y", names(data))],
#'   term_premia = data[, grep("^tp", names(data))],
#'   i = 5
#' )
#' }
#'
compute_variance_bound <- function(yields, term_premia, i) {
  if (i < 1) {
    stop("i must be >= 1")
  }

  # Compute components
  c_hat <- compute_c_hat(yields, term_premia, i)
  k_hat <- compute_k_hat(yields, term_premia, i)

  # Return variance bound
  0.25 * c_hat * k_hat
}


#' Compute SDF News (Innovations)
#'
#' Computes the time series of SDF news Delta_{t+1}p_{t+i}^{(1)} or Delta_{t+1}y_{t+i}^{(1)}
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
#' Delta_{t+1}p_{t+i}^{(1)} = n_hat_{i-1,t+1} - n_hat_{i,t}
#'
#' The SDF news for yields is:
#' Delta_{t+1}y_{t+i}^{(1)} = -Delta_{t+1}p_{t+i}^{(1)}
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
    # For i=1, n_hat_{0,t} = E_t[p_{t+0}^{(1)}] = p_t^{(1)} = -y_t^{(1)}
    y1 <- yields[["y1"]]
    if (is.null(y1)) {
      stop("y1 column not found in yields")
    }
    n_hat_i_minus_1 <- -y1 / 100 # Convert to decimal
  } else {
    n_hat_i_minus_1 <- compute_n_hat(yields, term_premia, i - 1)
  }

  # Number of observations
  T <- length(n_hat_i)

  # Initialize result
  sdf_news <- rep(NA, T - 1)

  # Compute news: n_hat_{i-1,t+1} - n_hat_{i,t}
  for (t in 1:(T - 1)) {
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
