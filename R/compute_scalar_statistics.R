#' Compute Scalar Statistics for Heteroskedasticity Identification
#'
#' Computes the centered sample variances \eqn{S_i^{(0)}} and
#' \eqn{\sigma_i^2} for each selected \code{w2} column.
#'
#' @param w1 Finite numeric vector of \eqn{\omega_1} residuals, with at least
#'   two observations, such as the \code{residuals} component returned by
#'   \code{\link{compute_w1_residuals}}.
#' @param w2 Numeric matrix or data frame of \eqn{\omega_2} residuals with
#'   \code{length(w1)} rows and at least one column, assembled from the
#'   residual vectors returned by \code{\link{compute_w2_residuals}}.
#'   All entries must be finite, and rows must correspond to the same
#'   observations as \code{w1} in the same order.
#'   Align dated series by realization date before extracting numeric inputs.
#' @param maturities Nonempty numeric vector of distinct integer-valued
#'   \code{w2} column indices between 1 and \code{ncol(w2)}. These indices
#'   need not denote bond maturities. The default, \code{NULL}, selects all
#'   columns in order.
#'
#' @return A list containing:
#' \describe{
#'   \item{s_i_0}{Named numeric vector of \eqn{S_i^{(0)}} values, keyed \code{maturity_N}
#'     (N = the w2 column index, not necessarily a bond maturity) with
#'     one entry per element of \code{maturities}.}
#'   \item{sigma_i_sq}{Named numeric vector of \eqn{\sigma_i^2} values, keyed
#'     \code{maturity_N} with one entry per element of \code{maturities}.}
#' }
#' Both vectors follow the order of \code{maturities}; element \code{k}
#' corresponds to \code{maturities[k]}. A statistic is zero when the
#' corresponding finite product or squared-residual series is constant.
#' Intermediate arithmetic can overflow for very large finite residuals,
#' yielding non-finite statistics.
#'
#' @details
#' For each selected column i, computes the centered sample variances using
#' \eqn{1/T} normalization, where \eqn{T = \operatorname{length}(w1)}
#' (see \code{\link{centered_cov}}):
#' \deqn{\hat{S}_i^{(0)} = \widehat{\mathrm{Var}}(\omega_1 \odot \omega_2^{(i)})}
#' \deqn{\hat{\sigma}_i^2 = \widehat{\mathrm{Var}}\big((\omega_2^{(i)})^{\odot 2}\big)}
#'
#' where \eqn{\odot} denotes the Hadamard (elementwise) product and
#' \eqn{\omega_2^{(i)}} is the i-th column of \eqn{\omega_2}.
#' \code{NA}, \code{NaN}, or infinite input values are rejected, including in
#' unselected \code{w2} columns; observations are not dropped. Invalid types
#' or maturity indices signal a \code{hetid_error_bad_argument}; mismatched
#' observation counts signal a \code{hetid_error_dimension_mismatch}; fewer
#' than two observations signal a \code{hetid_error_insufficient_data}.
#'
#' @export
#'
#' @examples
#' w1 <- c(-2, -1, 0, 1, 2)
#' w2 <- cbind(c(2, -1, -2, -1, 2), c(-1, 2, 0, -2, 1))
#'
#' scalar_stats <- compute_scalar_statistics(w1, w2)
#' scalar_stats$s_i_0
#' scalar_stats$sigma_i_sq
#'
#' compute_scalar_statistics(w1, w2, maturities = c(2, 1))
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
#' @param w1 Numeric vector of \eqn{\omega_1} residuals.
#' @param w2 Numeric matrix of \eqn{\omega_2} residuals (T x I).
#' @param maturities Vector of validated maturity indices.
#' @return A list with named numeric vectors \code{s_i_0} and \code{sigma_i_sq}.
#' @noRd
compute_scalar_statistics_impl <- function(w1, w2, maturities) {
  results <- compute_per_maturity(
    w1, w2, maturities,
    function(w1, w2_i, ...) {
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
