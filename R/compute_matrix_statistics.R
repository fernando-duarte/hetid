#' Compute Matrix Statistics for Heteroskedasticity Identification
#'
#' Computes matrix statistics S_i^(1) and S_i^(2) for each maturity i.
#'
#' @param w1 Numeric vector of \eqn{\omega_1} residuals from
#'   \code{\link{compute_w1_residuals}}, with at least two finite observations.
#' @param w2 Numeric matrix or data frame of \eqn{\omega_2} residuals from
#'   \code{\link{compute_w2_residuals}}, with \code{T = length(w1)} rows and
#'   \code{I >= 1} columns. All entries must be finite; rows must represent the
#'   same observations in the same order as \code{w1}.
#' @param maturities Nonempty numeric vector of distinct integer-valued
#'   \code{w2} column indices between 1 and \code{ncol(w2)} (the constraint
#'   axis). \code{NULL}, the default, selects all columns in order. These
#'   indices are positions, not bond maturities in months or years.
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
#' Both lists follow the order of \code{maturities}. Vector names and matrix
#' row and column names are \code{maturity_1}, ..., \code{maturity_I},
#' corresponding to all \code{w2} columns, even when only a subset of
#' constraints is selected. Zero or singular statistics are returned as
#' computed, without a degeneracy warning.
#'
#' @details
#' First computes the matrix:
#' \deqn{\omega_2^{\circ i} = \text{diag}(\omega_2^{(i)}) \omega_2}
#'
#' Then for each maturity i computes the centered sample (co)variances (1/T
#' normalization; see \code{\link{centered_cov}}):
#' \deqn{\hat{S}_i^{(1)} = \widehat{\mathrm{Cov}}(\omega_2^{\circ i},
#'   \omega_1 \odot \omega_2^{(i)})}
#' \deqn{\hat{S}_i^{(2)} = \widehat{\mathrm{Var}}(\omega_2^{\circ i})}
#'
#' where \eqn{\odot} denotes the Hadamard (elementwise) product.
#'
#' Missing, \code{NaN}, and infinite values are rejected; no observations
#' are dropped. Invalid types or maturity indices signal a
#' \code{hetid_error_bad_argument}; unequal observation counts signal a
#' \code{hetid_error_dimension_mismatch}; fewer than two observations signal
#' a \code{hetid_error_insufficient_data}.
#'
#' @seealso \code{\link{compute_identification_moments}} for all seven
#'   moments in a validated container.
#'
#' @export
#'
#' @examples
#' w1 <- c(-0.2, 0.1, -0.1, 0.3, -0.3, 0.2)
#' w2 <- cbind(
#'   c(0.1, -0.2, 0.3, -0.1, 0.2, -0.3),
#'   c(-0.1, 0.3, -0.2, 0.2, -0.3, 0.1)
#' )
#'
#' mat_stats <- compute_matrix_statistics(w1, w2)
#' mat_stats$s_i_1[[1]]
#' mat_stats$s_i_2[[1]]
#'
#' subset_stats <- compute_matrix_statistics(w1, w2, maturities = 2)
#' names(subset_stats$s_i_1)
#' names(subset_stats$s_i_1[[1]])
#' dim(subset_stats$s_i_2[[1]])
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
#' @param w1 Numeric vector of \eqn{\omega_1} residuals.
#' @param w2 Numeric matrix of \eqn{\omega_2} residuals (T x I).
#' @param maturities Vector of validated maturity indices.
#' @return List with \code{s_i_1} and \code{s_i_2}.
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
