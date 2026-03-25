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
#' set.seed(42)
#' T_obs <- 100
#' I <- 4
#' w1 <- rnorm(T_obs)
#' w2 <- matrix(rnorm(T_obs * I), nrow = T_obs, ncol = I)
#'
#' scalar_stats <- compute_scalar_statistics(w1, w2)
#' scalar_stats$s_i_0
#' scalar_stats$sigma_i_sq
compute_scalar_statistics <- function(w1, w2,
                                      maturities = NULL) {
  results <- compute_per_maturity(
    w1, w2, maturities,
    function(w1, w2, w2_i, t_obs, ...) {
      hadamard_prod <- w1 * w2_i
      s_i_0_val <- sum(hadamard_prod^2) / t_obs
      w2_i_sq <- w2_i^2
      term1 <- sum(w2_i_sq^2) / t_obs
      term2 <- (sum(w2_i_sq) / t_obs)^2
      sigma_i_sq_val <- term1 - term2
      list(
        s_i_0 = s_i_0_val,
        sigma_i_sq = sigma_i_sq_val
      )
    }
  )
  list(
    s_i_0 = vapply(
      results, `[[`, numeric(1), "s_i_0"
    ),
    sigma_i_sq = vapply(
      results, `[[`, numeric(1), "sigma_i_sq"
    )
  )
}
