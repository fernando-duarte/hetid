#' Validate Inputs for Gamma Quadratic Solver
#'
#' Internal function to validate inputs for solve_gamma_quadratic
#'
#' @param pc_j Principal component vector
#' @param w1 Reduced form residual for Y1
#' @param w2 Reduced form residual for Y2
#' @param tau Quantile parameter
#'
#' @return List with cleaned data and validation results
#' @keywords internal
validate_gamma_inputs <- function(pc_j, w1, w2, tau) {
  # Input validation
  if (!is.numeric(pc_j) || !is.numeric(w1) || !is.numeric(w2)) {
    stop("pc_j, w1, and w2 must be numeric vectors")
  }

  # Check equal lengths
  n <- length(pc_j)
  if (length(w1) != n || length(w2) != n) {
    stop("All input time series must have the same length")
  }

  # Check tau
  if (!is.numeric(tau) || length(tau) != 1 || tau < 0 || tau > 1) {
    stop("tau must be a single numeric value between 0 and 1")
  }

  # Remove any rows with NA values
  complete_idx <- complete.cases(pc_j, w1, w2)
  if (sum(complete_idx) < 3) {
    stop("Not enough complete observations (need at least 3)")
  }

  list(
    pc_j = pc_j[complete_idx],
    w1 = w1[complete_idx],
    w2 = w2[complete_idx],
    n = sum(complete_idx)
  )
}
