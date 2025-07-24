#' Compute Matrix Statistics for Heteroskedasticity Identification
#'
#' Computes matrix statistics S_i^(1) and S_i^(2) for each maturity i.
#'
#' @param w1 Numeric vector of W1 residuals from compute_w1_residuals()
#' @param w2 Matrix of W2 residuals (T x I) from compute_w2_residuals()
#' @param maturities Vector of maturity indices to compute statistics for.
#'   Default is all columns of w2.
#'
#' @return A list containing:
#' \describe{
#'   \item{s_i_1}{List of vectors, each element i is S_i^(1) (length I)}
#'   \item{s_i_2}{List of matrices, each element i is S_i^(2) (I x I)}
#' }
#'
#' @details
#' First computes the matrix:
#' \deqn{W_2^{\circ i} = \text{diag}(W_2^{(i)}) W_2}
#'
#' Then for each maturity i:
#' \deqn{\hat{S}_i^{(1)} = \frac{1}{T} (W_1 \odot W_2^{(i)})^T W_2^{\circ i}}
#' \deqn{\hat{S}_i^{(2)} = \frac{1}{T} (W_2^{\circ i})^T W_2^{\circ i}}
#'
#' where \eqn{\odot} denotes the Hadamard (elementwise) product.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Compute residuals
#' w1_res <- compute_w1_residuals()
#' w2_res <- compute_w2_residuals(yields, term_premia, maturities = 2:5)
#'
#' # Extract residual vectors/matrix
#' w1 <- w1_res$residuals
#' w2 <- do.call(cbind, w2_res$residuals)
#'
#' # Compute matrix statistics
#' mat_stats <- compute_matrix_statistics(w1, w2)
#'
#' # Access results for maturity 2
#' s_1_mat2 <- mat_stats$s_i_1[[1]] # Vector of length I
#' s_2_mat2 <- mat_stats$s_i_2[[1]] # Matrix I x I
#' }
compute_matrix_statistics <- function(w1, w2, maturities = NULL) {
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

  # Get dimensions
  I <- ncol(w2)
  n_maturities <- length(maturities)

  # Initialize storage
  s_i_1 <- vector("list", n_maturities)
  names(s_i_1) <- paste0("maturity_", maturities)

  s_i_2 <- vector("list", n_maturities)
  names(s_i_2) <- paste0("maturity_", maturities)

  # Compute statistics for each maturity
  for (idx in seq_along(maturities)) {
    i <- maturities[idx]
    w2_i <- w2[, i]

    # Compute W_2^{circ i} = diag(W_2^{(i)}) * W_2
    # This is element-wise multiplication of each column of W2 by w2_i
    w2_circ_i <- w2_i * w2 # Broadcasting: each column of w2 is multiplied by w2_i

    # S_i^(1) = (1/T) * (w1 ⊙ w2_i)^T * W_2^{circ i}
    hadamard_w1_w2i <- w1 * w2_i
    s_i_1_vec <- t(hadamard_w1_w2i) %*% w2_circ_i / T_obs
    s_i_1_vec <- as.vector(s_i_1_vec) # Ensure it's a vector
    names(s_i_1_vec) <- paste0("maturity_", seq_len(I))
    s_i_1[[idx]] <- s_i_1_vec

    # S_i^(2) = (1/T) * (W_2^{circ i})^T * W_2^{circ i}
    s_i_2_mat <- t(w2_circ_i) %*% w2_circ_i / T_obs
    rownames(s_i_2_mat) <- paste0("maturity_", seq_len(I))
    colnames(s_i_2_mat) <- paste0("maturity_", seq_len(I))
    s_i_2[[idx]] <- s_i_2_mat
  }

  list(
    s_i_1 = s_i_1,
    s_i_2 = s_i_2
  )
}
