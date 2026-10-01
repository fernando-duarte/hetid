#' Compute Vector Statistics for Heteroskedasticity Identification
#'
#' Computes vector statistics R_i^(0), R_i^(1), and P_i^(0) for each maturity i.
#'
#' @param w1 Finite numeric vector of \eqn{\omega_1} residuals of length T,
#'   with at least two observations, from \code{\link{compute_w1_residuals}}.
#' @param w2 Numeric matrix or data frame of \eqn{\omega_2} residuals (T x I)
#'   from \code{\link{compute_w2_residuals}}, with at least one column.
#' @param pcs Numeric matrix or data frame of exogenous time-series instruments
#'   (T x J), with at least one column. In the VFCI application these
#'   are principal components of nominal financial asset returns. Column names label
#'   the instrument axis of the moments (falling back to pc1..pcJ).
#' @param maturities Nonempty numeric vector of distinct integer-valued
#'   \code{w2} column indices between 1 and \code{ncol(w2)}, in the desired
#'   output order. The default \code{NULL} selects all columns of \code{w2}.
#'
#' @return A list with the following components. Here J is the instrument
#' count, n_components = \code{ncol(w2)} is the theta axis, and M is the
#' number of selected maturities (the constraint axis).
#' \describe{
#'   \item{r_i_0}{Matrix (J x M); column k holds R_i^(0) for maturity
#'     \code{maturities[k]} (columns named \code{maturity_N}).}
#'   \item{r_i_1}{Named list of length M (keys maturity_N); element k is the
#'     J x n_components matrix R_i^(1) for maturity \code{maturities[k]},
#'     with columns named \code{maturity_1}, ..., \code{maturity_I}.}
#'   \item{p_i_0}{Matrix (J x M); column k holds P_i^(0) for maturity
#'     \code{maturities[k]} (columns named \code{maturity_N}).}
#' }
#'
#' @details
#' For each maturity i, computes the centered sample covariances of the
#' instruments with the residual products (1/T normalization; see
#' \code{\link{centered_cov}} and the spec sections on moment notation and centering):
#' \deqn{\hat{R}_i^{(0)} = \widehat{\mathrm{Cov}}(PC, \omega_1 \odot \omega_2^{(i)})}
#' \deqn{\hat{R}_i^{(1)} = \widehat{\mathrm{Cov}}(PC, \omega_2 \odot \omega_2^{(i)})}
#' \deqn{\hat{P}_i^{(0)} = \widehat{\mathrm{Cov}}(PC, (\omega_2^{(i)})^{\odot 2})}
#'
#' where \eqn{\odot} denotes the Hadamard (elementwise) product.
#'
#' Inputs must contain only finite numeric values; missing observations
#' are rejected rather than omitted. Rows must already refer to the same
#' observations in the same order; this function does not align dates.
#' Invalid inputs raise structured \code{hetid_error} conditions.
#' Maturity indices identify \code{w2} columns, not bond maturities in months.
#' Constant instruments or constant finite residual products yield zero
#' covariances; no variance-degeneracy diagnostic is run. Finite inputs can
#' still overflow when forming residual products, yielding non-finite covariances.
#'
#' Row labels of \code{r_i_0}, \code{r_i_1}, and \code{p_i_0} use
#' \code{colnames(pcs)} when present, falling back to the standard
#' \code{pc1..pcJ} names.
#'
#' @export
#'
#' @examples
#' w1 <- c(-2, 1, 0, 2, -1, 0)
#' w2 <- cbind(c(-1, 0, 2, -2, 1, 0), c(0, -2, 1, 0, 2, -1))
#' pcs <- cbind(pc1 = c(-2, -1, 0, 0, 1, 2), pc2 = c(1, -1, 2, -2, 0, 0))
#'
#' vec_stats <- compute_vector_statistics(w1, w2, pcs)
#' vec_stats$r_i_0
#' vec_stats$r_i_1[[1]]
#'
#' selected <- compute_vector_statistics(w1, w2, pcs, maturities = 2)
#' colnames(selected$r_i_0)
#' colnames(selected$r_i_1[[1]])
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
#' @param w1 Numeric vector of \eqn{\omega_1} residuals.
#' @param w2 Numeric matrix of \eqn{\omega_2} residuals (T x I).
#' @param pcs Numeric matrix of instruments (T x J).
#' @param maturities Vector of validated maturity indices.
#' @return List with \code{r_i_0}, \code{r_i_1}, and \code{p_i_0}.
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
