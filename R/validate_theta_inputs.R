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
  # Use standardized validation utilities
  validate_numeric_inputs(pc_j = pc_j, w1 = w1, w2 = w2)
  validate_time_series_lengths(pc_j, w1, w2)
  validate_tau_parameter(tau)

  # Validate dates if provided
  if (!is.null(dates)) {
    if (length(dates) != length(pc_j)) {
      stop("Length of dates must match length of input time series")
    }
  }

  # Remove any rows with NA values
  complete_idx <- complete.cases(pc_j, w1, w2)
  n_complete <- sum(complete_idx)

  # Use standardized minimum observations validation
  validate_min_observations(n_complete)

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
