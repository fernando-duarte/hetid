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
#' For each maturity i, computes the centered sample variances (1/T
#' normalization; see [centered_cov()] and the spec sections on moment
#' notation and centering):
#' \deqn{\hat{S}_i^{(0)} = \widehat{\mathrm{Var}}(W_1 \odot W_2^{(i)})}
#' \deqn{\hat{\sigma}_i^2 = \widehat{\mathrm{Var}}\big((W_2^{(i)})^{\odot 2}\big)}
#'
#' where \eqn{\odot} denotes the Hadamard (elementwise) product and
#' \eqn{W_2^{(i)}} is the i-th column of W2.
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
#' scalar_stats <- compute_scalar_statistics(w1, w2)
#' scalar_stats$s_i_0
#' scalar_stats$sigma_i_sq
compute_scalar_statistics <- function(w1, w2,
                                      maturities = NULL) {
  validated <- validate_statistics_inputs(w1, w2, maturities)
  compute_scalar_statistics_impl(
    w1, validated$w2, validated$maturities
  )
}

#' Scalar Statistics Worker on Validated Inputs
#'
#' Trusts inputs already validated by
#' \code{validate_statistics_inputs()}; the exported wrapper and
#' \code{compute_identification_moments()} validate once and delegate
#' here.
#'
#' @param w1 Numeric vector of W1 residuals
#' @param w2 Numeric matrix of W2 residuals (T x I)
#' @param maturities Vector of validated maturity indices
#' @return List with named vectors s_i_0 and sigma_i_sq
#' @noRd
compute_scalar_statistics_impl <- function(w1, w2, maturities) {
  results <- compute_per_maturity(
    w1, w2, maturities,
    function(w1, w2, w2_i, ...) {
      hadamard_prod <- w1 * w2_i
      s_i_0_val <- as.numeric(
        centered_cov(hadamard_prod, hadamard_prod)
      )
      w2_i_sq <- w2_i^2
      sigma_i_sq_val <- as.numeric(
        centered_cov(w2_i_sq, w2_i_sq)
      )
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
