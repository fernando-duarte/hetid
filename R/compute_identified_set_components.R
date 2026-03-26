#' Compute Basic Components for Identified Set
#'
#' Computes the basic components L_i, V_i, and Q_i for the identified set
#' calculation for each maturity i.
#'
#' @param gamma Matrix (J x I) where each column gamma_i contains the
#'   coefficients for maturity i
#' @param r_i_0 Position-indexed matrix (J x n_maturities)
#'   from \code{compute_vector_statistics()}
#' @param r_i_1 Position-indexed list of n_maturities
#'   matrices (each J x I) from
#'   \code{compute_vector_statistics()}
#' @param p_i_0 Position-indexed matrix (J x n_maturities)
#'   from \code{compute_vector_statistics()}
#' @param maturities Integer vector of maturity indices.
#'   If NULL, inferred from input names via
#'   \code{\link{resolve_maturities}}; defaults to all
#'   columns when inputs are unnamed and full-size.
#'
#' @return A list containing:
#' \describe{
#'   \item{L_i}{Named vector of L_i values for each maturity}
#'   \item{V_i}{Named vector of V_i values for each maturity}
#'   \item{Q_i}{List of vectors, each element i is Q_i (length I)}
#' }
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
#' @section Maturity Indexing Convention:
#' All statistical inputs (r_i_0, r_i_1, p_i_0) are
#' \strong{position-indexed}: their outer dimension
#' must equal \code{length(maturities)}, with element
#' k corresponding to \code{maturities[k]}. Each
#' \code{r_i_1[[k]]} remains a full-size J x I matrix.
#'
#' \code{gamma} is \strong{full-size} (J x I), indexed
#' by maturity value. The function uses
#' \code{gamma[, maturities[k]]} internally.
#'
#' When \code{maturities = NULL}, maturity values are
#' inferred from input names via
#' \code{\link{resolve_maturities}}, matching the
#' convention in
#' \code{\link{compute_identified_set_quadratic}}.
#'
#' This matches the output convention of
#' \code{\link{compute_vector_statistics}}, so
#' statistics can be passed directly without manual
#' subsetting.
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' T_obs <- 100
#' J <- 3
#' I <- 4
#' w1 <- rnorm(T_obs)
#' w2 <- matrix(rnorm(T_obs * I), nrow = T_obs, ncol = I)
#' pcs <- matrix(rnorm(T_obs * J), nrow = T_obs, ncol = J)
#' gamma <- matrix(rnorm(J * I), nrow = J, ncol = I)
#'
#' vec_stats <- compute_vector_statistics(w1, w2, pcs)
#' components <- compute_identified_set_components(
#'   gamma = gamma,
#'   r_i_0 = vec_stats$r_i_0,
#'   r_i_1 = vec_stats$r_i_1,
#'   p_i_0 = vec_stats$p_i_0
#' )
compute_identified_set_components <- function(gamma, r_i_0, r_i_1, p_i_0,
                                              maturities = NULL) {
  # Validate inputs
  assert_bad_argument_ok(
    is.matrix(gamma),
    "gamma must be a matrix",
    arg = "gamma"
  )
  assert_bad_argument_ok(
    is.matrix(r_i_0),
    "r_i_0 must be a matrix",
    arg = "r_i_0"
  )
  assert_bad_argument_ok(
    is.list(r_i_1),
    "r_i_1 must be a list of matrices",
    arg = "r_i_1"
  )
  assert_bad_argument_ok(
    is.matrix(p_i_0),
    "p_i_0 must be a matrix",
    arg = "p_i_0"
  )

  J <- nrow(gamma)
  n_components <- ncol(gamma)

  maturities <- resolve_maturities(
    maturities,
    list(r_i_1 = r_i_1),
    n_components
  )

  assert_bad_argument_ok(
    all(maturities >= 1) &&
      all(maturities <= n_components),
    "maturities must be between 1 and I",
    arg = "maturities"
  )
  n_maturities <- length(maturities)

  assert_dimension_ok(
    nrow(r_i_0) == J &&
      ncol(r_i_0) == n_maturities,
    "r_i_0 must be J x length(maturities)"
  )
  assert_dimension_ok(
    length(r_i_1) == n_maturities,
    "r_i_1 must have length(maturities) elements"
  )
  assert_dimension_ok(
    nrow(p_i_0) == J &&
      ncol(p_i_0) == n_maturities,
    "p_i_0 must be J x length(maturities)"
  )

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
    assert_dimension_ok(
      is.matrix(R_i_1_mat) &&
        nrow(R_i_1_mat) == J,
      paste0(
        "r_i_1 for maturity ", i,
        " (position ", idx,
        ") must be a J x I matrix"
      )
    )
    Q_i[[idx]] <- as.numeric( # nolint: object_name_linter.
      crossprod(gamma_i, R_i_1_mat)
    )
    names(Q_i[[idx]]) <- paste0( # nolint: object_name_linter.
      "maturity_", seq_len(ncol(R_i_1_mat))
    )
  }

  list(
    L_i = L_i,
    V_i = V_i,
    Q_i = Q_i
  )
}
