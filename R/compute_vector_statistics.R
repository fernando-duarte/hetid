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
#' For each maturity i, computes the centered sample covariances of the
#' principal components with the residual products (1/T normalization; see
#' [centered_cov()] and the spec sections on moment notation and centering):
#' \deqn{\hat{R}_i^{(0)} = \widehat{\mathrm{Cov}}(PC, W_1 \odot W_2^{(i)})}
#' \deqn{\hat{R}_i^{(1)} = \widehat{\mathrm{Cov}}(PC, W_2 \odot W_2^{(i)})}
#' \deqn{\hat{P}_i^{(0)} = \widehat{\mathrm{Cov}}(PC, (W_2^{(i)})^{\odot 2})}
#'
#' where \eqn{\odot} denotes the Hadamard (elementwise) product.
#'
#' Row labels of \code{r_i_0}, \code{r_i_1}, and \code{p_i_0} use
#' \code{colnames(pcs)} when present, falling back to the standard
#' \code{pc1..pcJ} names.
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' n_obs <- 100
#' I <- 4
#' J <- 3
#' w1 <- rnorm(n_obs)
#' w2 <- matrix(rnorm(n_obs * I), nrow = n_obs, ncol = I)
#' pcs <- matrix(rnorm(n_obs * J), nrow = n_obs, ncol = J)
#'
#' vec_stats <- compute_vector_statistics(w1, w2, pcs)
#' vec_stats$r_i_0
#' vec_stats$r_i_1[[1]]
compute_vector_statistics <- function(w1, w2, pcs,
                                      maturities = NULL) {
  validated <- validate_statistics_inputs(w1, w2, maturities)
  pcs <- validate_pcs_input(pcs, validated$t_obs)
  compute_vector_statistics_impl(
    w1, validated$w2, pcs, validated$maturities
  )
}

#' Vector Statistics Worker on Validated Inputs
#'
#' Trusts inputs already validated by
#' \code{validate_statistics_inputs()} and \code{validate_pcs_input()};
#' the exported wrapper and \code{compute_identification_moments()}
#' validate once and delegate here. Instrument-axis labels come from
#' \code{colnames(pcs)} when present, with the standard pc names as
#' fallback.
#'
#' @param w1 Numeric vector of W1 residuals
#' @param w2 Numeric matrix of W2 residuals (T x I)
#' @param pcs Numeric matrix of principal components (T x J)
#' @param maturities Vector of validated maturity indices
#' @return List with r_i_0, r_i_1, and p_i_0
#' @noRd
compute_vector_statistics_impl <- function(w1, w2, pcs, maturities) {
  pc_names <- colnames(pcs)
  if (is.null(pc_names)) {
    pc_names <- get_pc_column_names(ncol(pcs))
  }
  theta_names <- maturity_names(seq_len(ncol(w2)))

  results <- compute_per_maturity(
    w1, w2, maturities,
    function(w1, w2, w2_i, ...) {
      hadamard_w1_w2i <- w1 * w2_i
      r_i_0_vec <- as.vector(
        centered_cov(pcs, hadamard_w1_w2i)
      )
      r_i_1_mat <- centered_cov(pcs, w2 * w2_i)
      colnames(r_i_1_mat) <- theta_names
      rownames(r_i_1_mat) <- pc_names
      w2_i_sq <- w2_i^2
      p_i_0_vec <- as.vector(
        centered_cov(pcs, w2_i_sq)
      )
      list(
        r_i_0 = r_i_0_vec,
        r_i_1 = r_i_1_mat,
        p_i_0 = p_i_0_vec
      )
    }
  )

  mat_names <- names(results)

  r_i_0 <- do.call(
    cbind, lapply(results, `[[`, "r_i_0")
  )
  rownames(r_i_0) <- pc_names
  colnames(r_i_0) <- mat_names

  p_i_0 <- do.call(
    cbind, lapply(results, `[[`, "p_i_0")
  )
  rownames(p_i_0) <- pc_names
  colnames(p_i_0) <- mat_names

  list(
    r_i_0 = r_i_0,
    r_i_1 = lapply(results, `[[`, "r_i_1"),
    p_i_0 = p_i_0
  )
}
