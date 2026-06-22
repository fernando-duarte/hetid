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
#'   \item{s_i_1}{List of S_i^(1) vectors (each length I, the theta axis),
#'     keyed \code{maturity_N} (N = the w2 column index, not necessarily a
#'     bond maturity) with one entry per element of \code{maturities}.}
#'   \item{s_i_2}{List of S_i^(2) matrices (each I x I, the theta axis),
#'     keyed \code{maturity_N} with one entry per element of
#'     \code{maturities}.}
#' }
#'
#' @details
#' First computes the matrix:
#' \deqn{W_2^{\circ i} = \text{diag}(W_2^{(i)}) W_2}
#'
#' Then for each maturity i computes the centered sample (co)variances (1/T
#' normalization; see [centered_cov()] and the spec sections on moment
#' notation and centering):
#' \deqn{\hat{S}_i^{(1)} = \widehat{\mathrm{Cov}}(W_2^{\circ i},
#'   W_1 \odot W_2^{(i)})}
#' \deqn{\hat{S}_i^{(2)} = \widehat{\mathrm{Var}}(W_2^{\circ i})}
#'
#' where \eqn{\odot} denotes the Hadamard (elementwise) product.
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' n_obs <- 100
#' I <- 4
#' w1 <- rnorm(n_obs)
#' w2 <- matrix(rnorm(n_obs * I), nrow = n_obs, ncol = I)
#'
#' mat_stats <- compute_matrix_statistics(w1, w2)
#' mat_stats$s_i_1[[1]]
#' mat_stats$s_i_2[[1]]
compute_matrix_statistics <- function(w1, w2,
                                      maturities = NULL) {
  validated <- validate_statistics_inputs(w1, w2, maturities)
  compute_matrix_statistics_impl(
    w1, validated$w2, validated$maturities
  )
}

#' Matrix Statistics Worker on Validated Inputs
#'
#' Trusts inputs already validated by
#' \code{validate_statistics_inputs()}; the exported wrapper and
#' \code{compute_identification_moments()} validate once and delegate
#' here.
#'
#' @param w1 Numeric vector of W1 residuals
#' @param w2 Numeric matrix of W2 residuals (T x I)
#' @param maturities Vector of validated maturity indices
#' @return List with s_i_1 and s_i_2
#' @noRd
compute_matrix_statistics_impl <- function(w1, w2, maturities) {
  theta_names <- maturity_names(seq_len(ncol(w2)))
  results <- compute_per_maturity(
    w1, w2, maturities,
    function(w1, w2, w2_i, ...) {
      w2_circ_i <- w2_i * w2
      hadamard_w1_w2i <- w1 * w2_i
      s_i_1_vec <- as.vector(
        centered_cov(hadamard_w1_w2i, w2_circ_i)
      )
      names(s_i_1_vec) <- theta_names
      s_i_2_mat <- centered_cov(w2_circ_i, w2_circ_i)
      rownames(s_i_2_mat) <- theta_names
      colnames(s_i_2_mat) <- theta_names
      list(s_i_1 = s_i_1_vec, s_i_2 = s_i_2_mat)
    }
  )
  list(
    s_i_1 = lapply(results, `[[`, "s_i_1"),
    s_i_2 = lapply(results, `[[`, "s_i_2")
  )
}
