#' Compute Moments for Gamma Quadratic Solver
#'
#' Internal function to compute covariances and variances needed for solve_gamma_quadratic
#'
#' @param pc_j Principal component vector
#' @param w1 Reduced form residual for Y1
#' @param w2 Reduced form residual for Y2
#' @param use_t_minus_1 Whether to use n-1 denominator
#'
#' @return List with computed moments
#' @keywords internal
compute_gamma_moments <- function(pc_j, w1, w2, use_t_minus_1 = TRUE) {
  n <- length(pc_j)
  denom <- ifelse(use_t_minus_1, n - 1, n)

  # Compute derived series
  w1_w2 <- w1 * w2
  w2_sq <- w2^2

  # Compute means
  mean_pc_j <- mean(pc_j)
  mean_w1_w2 <- mean(w1_w2)
  mean_w2_sq <- mean(w2_sq)

  # Compute covariances and variances
  cov_w1w2_pcj <- sum((w1_w2 - mean_w1_w2) * (pc_j - mean_pc_j)) / denom
  cov_w2sq_pcj <- sum((w2_sq - mean_w2_sq) * (pc_j - mean_pc_j)) / denom
  var_w1w2 <- sum((w1_w2 - mean_w1_w2)^2) / denom
  var_w2sq <- sum((w2_sq - mean_w2_sq)^2) / denom
  cov_w1w2_w2sq <- sum((w1_w2 - mean_w1_w2) * (w2_sq - mean_w2_sq)) / denom

  # Check for zero denominators
  if (abs(cov_w2sq_pcj) < .Machine$double.eps) {
    stop("Cov(W2^2, PC_j) is effectively zero, cannot compute quadratic coefficients")
  }
  if (abs(var_w2sq) < .Machine$double.eps) {
    stop("Var(W2^2) is effectively zero, cannot compute quadratic coefficients")
  }

  list(
    cov_w1w2_pcj = cov_w1w2_pcj,
    cov_w2sq_pcj = cov_w2sq_pcj,
    var_w1w2 = var_w1w2,
    var_w2sq = var_w2sq,
    cov_w1w2_w2sq = cov_w1w2_w2sq
  )
}
