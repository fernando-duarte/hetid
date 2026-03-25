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
#' set.seed(42)
#' T_obs <- 100
#' I <- 4
#' J <- 3
#' w1 <- rnorm(T_obs)
#' w2 <- matrix(rnorm(T_obs * I), nrow = T_obs, ncol = I)
#' pcs <- matrix(rnorm(T_obs * J), nrow = T_obs, ncol = J)
#'
#' vec_stats <- compute_vector_statistics(w1, w2, pcs)
#' vec_stats$r_i_0
#' vec_stats$r_i_1[[1]]
compute_vector_statistics <- function(w1, w2, pcs,
                                      maturities = NULL) {
  validated <- validate_statistics_inputs(
    w1, w2, maturities
  )
  w2 <- validated$w2
  t_obs <- validated$t_obs
  maturities <- validated$maturities

  # Additional pcs validation (unique to this function)
  if (!is.matrix(pcs) && !is.data.frame(pcs)) {
    stop("pcs must be a matrix or data frame")
  }
  pcs <- as.matrix(pcs)
  if (nrow(pcs) != t_obs) {
    stop(
      "pcs must have the same number of ",
      "observations as w1 and w2"
    )
  }

  # Get dimensions
  J <- ncol(pcs)
  n_maturities_idx <- ncol(w2)
  n_maturities <- length(maturities)

  # Initialize storage
  r_i_0 <- matrix(0, nrow = J, ncol = n_maturities)
  rownames(r_i_0) <- get_pc_column_names(J)
  colnames(r_i_0) <- maturity_names(maturities)

  r_i_1 <- vector("list", n_maturities)
  names(r_i_1) <- maturity_names(maturities)

  p_i_0 <- matrix(0, nrow = J, ncol = n_maturities)
  rownames(p_i_0) <- get_pc_column_names(J)
  colnames(p_i_0) <- maturity_names(maturities)

  # Compute statistics for each maturity
  for (idx in seq_along(maturities)) {
    i <- maturities[idx]
    w2_i <- w2[, i]

    # R_i^(0) = (1/T) * PC^T * (w1 ⊙ w2_i)
    hadamard_w1_w2i <- w1 * w2_i
    r_i_0[, idx] <- t(pcs) %*% hadamard_w1_w2i / t_obs

    # R_i^(1) = (1/T) * PC^T * (w2 ⊙ w2_i)
    # This is a J x I matrix; w2 * w2_i broadcasts the column
    r_i_1_mat <- t(pcs) %*% (w2 * w2_i) / t_obs
    colnames(r_i_1_mat) <- paste0(
      "maturity_", seq_len(n_maturities_idx)
    )
    rownames(r_i_1_mat) <- get_pc_column_names(J)
    r_i_1[[idx]] <- r_i_1_mat

    # P_i^(0) = (1/T) * PC^T * (w2_i)^⊙2
    w2_i_sq <- w2_i^2
    p_i_0[, idx] <- t(pcs) %*% w2_i_sq / t_obs
  }

  list(
    r_i_0 = r_i_0,
    r_i_1 = r_i_1,
    p_i_0 = p_i_0
  )
}
