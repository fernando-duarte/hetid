#' Compute Scalar Statistics for Heteroskedasticity Identification
#'
#' Computes scalar statistics S_i^(0) and sigma_i^2 for each maturity i.
#'
#' @param w1 Numeric vector of W1 residuals from compute_w1_residuals()
#' @param w2 Matrix of W2 residuals (T x I) from compute_w2_residuals()
#' @param maturities Vector of maturity indices to compute statistics for.
#'   Default is all columns of w2.
#'
#' @return A list containing:
#' \describe{
#'   \item{s_i_0}{Named vector of S_i^(0) values for each maturity}
#'   \item{sigma_i_sq}{Named vector of sigma_i^2 values for each maturity}
#' }
#'
#' @details
#' For each maturity i, computes:
#' \deqn{\hat{S}_i^{(0)} = \frac{1}{T} \| W_1 \odot W_2^{(i)} \|_2^2}
#' \deqn{\hat{\sigma}_i^2 = \frac{1}{T} \| (W_2^{(i)})^{\odot 2} \|_2^2 -
#'   \left(\frac{1}{T} \| W_2^{(i)} \|_2^2 \right)^2}
#'
#' where \eqn{\odot} denotes the Hadamard (elementwise) product and
#' \eqn{W_2^{(i)}} is the i-th column of W2.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # First compute residuals
#' w1_res <- compute_w1_residuals()
#' w2_res <- compute_w2_residuals(yields, term_premia, maturities = 2:5)
#'
#' # Extract residual vectors/matrix
#' w1 <- w1_res$residuals
#' w2 <- do.call(cbind, w2_res$residuals)
#'
#' # Compute scalar statistics
#' scalar_stats <- compute_scalar_statistics(w1, w2)
#' print(scalar_stats$s_i_0)
#' print(scalar_stats$sigma_i_sq)
#' }
compute_scalar_statistics <- function(w1, w2, maturities = NULL) {
  # Validate inputs
  if (!is.numeric(w1) || !is.vector(w1)) {
    stop("w1 must be a numeric vector")
  }

  if (!is.matrix(w2) && !is.data.frame(w2)) {
    stop("w2 must be a matrix or data frame")
  }
  w2 <- as.matrix(w2)

  # Check dimensions
  T_obs <- length(w1)
  if (nrow(w2) != T_obs) {
    stop("w1 and w2 must have the same number of observations")
  }

  # Set maturities if not provided
  if (is.null(maturities)) {
    maturities <- seq_len(ncol(w2))
  }

  # Validate maturities
  if (any(maturities < 1) || any(maturities > ncol(w2))) {
    stop("maturities must be between 1 and ncol(w2)")
  }

  # Initialize storage
  s_i_0 <- numeric(length(maturities))
  sigma_i_sq <- numeric(length(maturities))
  names(s_i_0) <- paste0("maturity_", maturities)
  names(sigma_i_sq) <- paste0("maturity_", maturities)

  # Compute statistics for each maturity
  for (idx in seq_along(maturities)) {
    i <- maturities[idx]
    w2_i <- w2[, i]

    # S_i^(0) = (1/T) * ||w1 ⊙ w2_i||_2^2
    hadamard_prod <- w1 * w2_i
    s_i_0[idx] <- sum(hadamard_prod^2) / T_obs

    # sigma_i^2 = (1/T) * ||w2_i^⊙2||_2^2 - ((1/T) * ||w2_i||_2^2)^2
    w2_i_sq <- w2_i^2
    term1 <- sum(w2_i_sq^2) / T_obs
    term2 <- (sum(w2_i_sq) / T_obs)^2
    sigma_i_sq[idx] <- term1 - term2
  }

  list(
    s_i_0 = s_i_0,
    sigma_i_sq = sigma_i_sq
  )
}
