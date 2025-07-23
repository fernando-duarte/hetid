#' Validate Inputs for Theta Quadratic Solver
#'
#' Internal function to validate inputs for solve_theta_quadratic
#'
#' @param pc_j Principal component vector
#' @param w1 Reduced form residual for Y1
#' @param w2 Reduced form residual for Y2
#' @param tau Quantile parameter
#' @param dates Optional vector of dates corresponding to the time series
#'
#' @return List with cleaned data and validation results
#' @keywords internal
validate_theta_inputs <- function(pc_j, w1, w2, tau, dates = NULL) {
  # Validate numeric types for all inputs at once
  inputs_numeric <- all(sapply(list(pc_j, w1, w2), is.numeric))
  if (!inputs_numeric) {
    stop("pc_j, w1, and w2 must be numeric vectors")
  }

  # Check equal lengths
  lengths <- c(length(pc_j), length(w1), length(w2))
  if (length(unique(lengths)) > 1) {
    stop("All input time series must have the same length")
  }

  # Validate dates if provided
  if (!is.null(dates)) {
    if (length(dates) != lengths[1]) {
      stop("Length of dates must match length of input time series")
    }
  }

  # Validate tau parameter
  tau_valid <- is.numeric(tau) && length(tau) == 1 && tau >= 0 && tau <= 1
  if (!tau_valid) {
    stop("tau must be a single numeric value between 0 and 1")
  }

  # Remove any rows with NA values
  complete_idx <- complete.cases(pc_j, w1, w2)
  n_complete <- sum(complete_idx)

  if (n_complete < 3) {
    stop("Not enough complete observations (need at least 3)")
  }

  # Prepare output
  result <- list(
    pc_j = pc_j[complete_idx],
    w1 = w1[complete_idx],
    w2 = w2[complete_idx],
    n = n_complete
  )

  # Add dates if provided
  if (!is.null(dates)) {
    result$dates <- dates[complete_idx]
  }

  result
}
