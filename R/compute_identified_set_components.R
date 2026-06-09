#' Compute Basic Components for Identified Set
#'
#' Computes the basic components L_i, V_i, and Q_i for the identified set
#' calculation for each maturity i.
#'
#' @param gamma Matrix (J x I) where each column gamma_i contains the
#'   coefficients for system column i. I must equal the moments'
#'   \code{n_components} attribute and J its instrument count.
#' @param moments A \code{hetid_moments} object from
#'   \code{\link{compute_identification_moments}}
#'
#' @return An object of class \code{hetid_components}: a list containing
#' \describe{
#'   \item{L_i}{Named vector of L_i values for each maturity}
#'   \item{V_i}{Named vector of V_i values for each maturity}
#'   \item{Q_i}{List of vectors, each element is Q_i (length n_components)}
#' }
#' carrying the moments' \code{maturities} and \code{n_components}
#' attributes forward.
#'
#' @details
#' For each maturity i, computes:
#' \deqn{L_i(\boldsymbol{\Gamma}) = \boldsymbol{\gamma}_i^{\top} \hat{\mathbf{R}}_i^{(0)}}
#' \deqn{V_i(\boldsymbol{\Gamma}) = \boldsymbol{\gamma}_i^{\top}
#'   \left(\hat{\mathbf{P}}_i^{(0)} (\hat{\mathbf{P}}_i^{(0)})^{\top}\right)
#'   \boldsymbol{\gamma}_i}
#' \deqn{\mathbf{Q}_i(\boldsymbol{\Gamma}) = \boldsymbol{\gamma}_i^{\top}
#'   \hat{\mathbf{R}}_i^{(1)} \in \mathbb{R}^I}
#'
#' where \eqn{\boldsymbol{\gamma}_i} is the i-th column of the matrix
#' \eqn{\boldsymbol{\Gamma}}, \eqn{\hat{\mathbf{R}}_i^{(0)}} is a vector,
#' \eqn{\hat{\mathbf{R}}_i^{(1)}} is a matrix, and \eqn{\hat{\mathbf{P}}_i^{(0)}}
#' is a vector.
#'
#' @template section-maturity-convention
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' n_obs <- 100
#' J <- 3
#' I <- 4
#' w1 <- rnorm(n_obs)
#' w2 <- matrix(rnorm(n_obs * I), nrow = n_obs, ncol = I)
#' pcs <- matrix(rnorm(n_obs * J), nrow = n_obs, ncol = J)
#' gamma <- matrix(rnorm(J * I), nrow = J, ncol = I)
#'
#' moments <- compute_identification_moments(w1, w2, pcs)
#' components <- compute_identified_set_components(gamma, moments)
compute_identified_set_components <- function(gamma, moments) {
  assert_hetid_moments(moments)
  assert_bad_argument_ok(
    is.matrix(gamma),
    "gamma must be a matrix",
    arg = "gamma"
  )

  maturities <- attr(moments, "maturities")
  n_components <- attr(moments, "n_components")
  j_rows <- nrow(moments$r_i_0)

  assert_dimension_ok(
    ncol(gamma) == n_components,
    paste0(
      "gamma must have n_components (", n_components,
      ") columns to match the moments' system"
    )
  )
  assert_dimension_ok(
    nrow(gamma) == j_rows,
    paste0(
      "gamma must have the same number of rows (J = ", j_rows,
      ") as the moments' instruments"
    )
  )

  r_i_0 <- moments$r_i_0
  r_i_1 <- moments$r_i_1
  p_i_0 <- moments$p_i_0
  n_maturities <- length(maturities)

  L_i <- numeric(n_maturities) # nolint: object_name_linter.
  V_i <- numeric(n_maturities) # nolint: object_name_linter.
  Q_i <- vector("list", n_maturities) # nolint: object_name_linter.

  names(L_i) <- maturity_names(maturities) # nolint: object_name_linter.
  names(V_i) <- maturity_names(maturities) # nolint: object_name_linter.
  names(Q_i) <- maturity_names(maturities) # nolint: object_name_linter.

  for (idx in seq_along(maturities)) {
    i <- maturities[idx]
    gamma_i <- gamma[, i, drop = FALSE]

    L_i[idx] <- as.numeric( # nolint: object_name_linter.
      crossprod(gamma_i, r_i_0[, idx])
    )

    p_i_0_vec <- p_i_0[, idx, drop = FALSE]
    V_i[idx] <- as.numeric( # nolint: object_name_linter.
      crossprod(gamma_i, p_i_0_vec)
    )^2

    R_i_1_mat <- r_i_1[[idx]] # nolint: object_name_linter.
    Q_i[[idx]] <- as.numeric( # nolint: object_name_linter.
      crossprod(gamma_i, R_i_1_mat)
    )
    names(Q_i[[idx]]) <- paste0( # nolint: object_name_linter.
      "maturity_", seq_len(ncol(R_i_1_mat))
    )
  }

  new_hetid_components(
    L_i = L_i, V_i = V_i, Q_i = Q_i,
    maturities = maturities, n_components = n_components
  )
}

#' Construct a hetid_components Object
#'
#' Low-level constructor carrying the maturity identity of the moments
#' the components were derived from. Shape validation happens downstream
#' in \code{validate_quadratic_inputs()}.
#'
#' @param L_i Named vector of L_i values
#' @param V_i Named vector of V_i values
#' @param Q_i List of Q_i vectors
#' @param maturities Integer vector of w2 column indices
#' @param n_components Theta-axis dimension
#'
#' @return A classed \code{hetid_components} list
#' @keywords internal
new_hetid_components <- function(L_i, V_i, Q_i, # nolint: object_name_linter.
                                 maturities, n_components) {
  structure(
    list(L_i = L_i, V_i = V_i, Q_i = Q_i),
    maturities = as.integer(maturities),
    n_components = as.integer(n_components),
    class = "hetid_components"
  )
}
