#' Compute Vector Statistics for Heteroskedasticity Identification
#'
#' Computes vector statistics R_i^(0), R_i^(1), and P_i^(0) for each maturity i.
#'
#' @param w1 Numeric vector of W1 residuals from compute_w1_residuals()
#' @param w2 Matrix of W2 residuals (T x I) from compute_w2_residuals()
#' @param pcs Matrix of principal components (T x J)
#' @param maturities Vector of maturity indices to compute statistics for.
#'   Default is all columns of w2.
#'
#' @return A list containing:
#' \describe{
#'   \item{r_i_0}{Matrix (J x I) where column i contains R_i^(0)}
#'   \item{r_i_1}{List of matrices, each element i is R_i^(1) (J x I)}
#'   \item{p_i_0}{Matrix (J x I) where column i contains P_i^(0)}
#' }
#'
#' @details
#' For each maturity i, computes:
#' \deqn{\hat{R}_i^{(0)} = \frac{1}{T} PC^T (W_1 \odot W_2^{(i)})}
#' \deqn{\hat{R}_i^{(1)} = \frac{1}{T} PC^T (W_2 \odot W_2^{(i)})}
#' \deqn{\hat{P}_i^{(0)} = \frac{1}{T} PC^T (W_2^{(i)})^{\odot 2}}
#'
#' where \eqn{\odot} denotes the Hadamard (elementwise) product.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load principal components
#' data(variables)
#' pcs <- as.matrix(variables[, paste0("pc", 1:4)])
#'
#' # Compute residuals
#' w1_res <- compute_w1_residuals(n_pcs = 4)
#' w2_res <- compute_w2_residuals(yields, term_premia, maturities = 2:5)
#'
#' # Extract residual vectors/matrix
#' w1 <- w1_res$residuals
#' w2 <- do.call(cbind, w2_res$residuals)
#'
#' # Align dimensions (PCs are T, residuals are T-1)
#' pcs_aligned <- pcs[2:nrow(pcs), ]
#'
#' # Compute vector statistics
#' vec_stats <- compute_vector_statistics(w1, w2, pcs_aligned)
#' }
compute_vector_statistics <- function(w1, w2, pcs, maturities = NULL) {
  # Validate inputs
  if (!is.numeric(w1) || !is.vector(w1)) {
    stop("w1 must be a numeric vector")
  }

  if (!is.matrix(w2) && !is.data.frame(w2)) {
    stop("w2 must be a matrix or data frame")
  }
  w2 <- as.matrix(w2)

  if (!is.matrix(pcs) && !is.data.frame(pcs)) {
    stop("pcs must be a matrix or data frame")
  }
  pcs <- as.matrix(pcs)

  # Check dimensions
  T_obs <- length(w1)
  if (nrow(w2) != T_obs) {
    stop("w1 and w2 must have the same number of observations")
  }
  if (nrow(pcs) != T_obs) {
    stop("pcs must have the same number of observations as w1 and w2")
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
  J <- ncol(pcs)
  I <- ncol(w2)
  n_maturities <- length(maturities)

  # Initialize storage
  r_i_0 <- matrix(NA, nrow = J, ncol = n_maturities)
  colnames(r_i_0) <- paste0("maturity_", maturities)
  rownames(r_i_0) <- paste0("pc", seq_len(J))

  r_i_1 <- vector("list", n_maturities)
  names(r_i_1) <- paste0("maturity_", maturities)

  p_i_0 <- matrix(NA, nrow = J, ncol = n_maturities)
  colnames(p_i_0) <- paste0("maturity_", maturities)
  rownames(p_i_0) <- paste0("pc", seq_len(J))

  # Compute statistics for each maturity
  for (idx in seq_along(maturities)) {
    i <- maturities[idx]
    w2_i <- w2[, i]

    # R_i^(0) = (1/T) * PC^T * (w1 ⊙ w2_i)
    hadamard_w1_w2i <- w1 * w2_i
    r_i_0[, idx] <- t(pcs) %*% hadamard_w1_w2i / T_obs

    # R_i^(1) = (1/T) * PC^T * (w2 ⊙ w2_i)
    # This is a J x I matrix
    r_i_1_mat <- matrix(NA, nrow = J, ncol = I)
    for (j in seq_len(I)) {
      hadamard_w2j_w2i <- w2[, j] * w2_i
      r_i_1_mat[, j] <- t(pcs) %*% hadamard_w2j_w2i / T_obs
    }
    colnames(r_i_1_mat) <- paste0("maturity_", seq_len(I))
    rownames(r_i_1_mat) <- paste0("pc", seq_len(J))
    r_i_1[[idx]] <- r_i_1_mat

    # P_i^(0) = (1/T) * PC^T * (w2_i)^⊙2
    w2_i_sq <- w2_i^2
    p_i_0[, idx] <- t(pcs) %*% w2_i_sq / T_obs
  }

  list(
    r_i_0 = r_i_0,
    r_i_1 = r_i_1,
    p_i_0 = p_i_0
  )
}
