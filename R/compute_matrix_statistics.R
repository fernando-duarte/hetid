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
#' set.seed(42)
#' T_obs <- 100
#' I <- 4
#' w1 <- rnorm(T_obs)
#' w2 <- matrix(rnorm(T_obs * I), nrow = T_obs, ncol = I)
#'
#' mat_stats <- compute_matrix_statistics(w1, w2)
#' mat_stats$s_i_1[[1]]
#' mat_stats$s_i_2[[1]]
compute_matrix_statistics <- function(w1, w2,
                                      maturities = NULL) {
  results <- compute_per_maturity(
    w1, w2, maturities,
    function(w1, w2, w2_i, t_obs, ...) {
      n_mat <- ncol(w2)
      w2_circ_i <- w2_i * w2
      hadamard_w1_w2i <- w1 * w2_i
      s_i_1_vec <- as.vector(
        crossprod(hadamard_w1_w2i, w2_circ_i) / t_obs
      )
      names(s_i_1_vec) <- paste0(
        "maturity_", seq_len(n_mat)
      )
      s_i_2_mat <- crossprod(w2_circ_i) / t_obs
      mat_names <- paste0(
        "maturity_", seq_len(n_mat)
      )
      rownames(s_i_2_mat) <- mat_names
      colnames(s_i_2_mat) <- mat_names
      list(s_i_1 = s_i_1_vec, s_i_2 = s_i_2_mat)
    }
  )
  list(
    s_i_1 = lapply(results, `[[`, "s_i_1"),
    s_i_2 = lapply(results, `[[`, "s_i_2")
  )
}
