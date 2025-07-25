#' Compute Basic Components for Identified Set
#'
#' Computes the basic components L_i, V_i, and Q_i for the identified set
#' calculation for each maturity i.
#'
#' @param gamma Matrix (J x I) where each column gamma_i contains the
#'   coefficients for maturity i
#' @param r_i_0 Matrix (J x I) where column i contains R_i^(0) from
#'   compute_vector_statistics()
#' @param r_i_1 List of matrices, each element i is R_i^(1) (J x I) from
#'   compute_vector_statistics()
#' @param p_i_0 Matrix (J x I) where column i contains P_i^(0) from
#'   compute_vector_statistics()
#' @param maturities Vector of maturity indices to compute components for.
#'   Default is all columns.
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
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume we have computed vector statistics
#' vec_stats <- compute_vector_statistics(w1, w2, pcs)
#'
#' # Create gamma matrix (J x I)
#' J <- 4 # number of PCs
#' I <- 8 # number of maturities
#' gamma <- matrix(rnorm(J * I), nrow = J, ncol = I)
#'
#' # Compute identified set components
#' components <- compute_identified_set_components(
#'   gamma = gamma,
#'   r_i_0 = vec_stats$r_i_0,
#'   r_i_1 = vec_stats$r_i_1,
#'   p_i_0 = vec_stats$p_i_0
#' )
#' }
compute_identified_set_components <- function(gamma, r_i_0, r_i_1, p_i_0,
                                              maturities = NULL) {
  # Validate inputs
  if (!is.matrix(gamma)) {
    stop("gamma must be a matrix")
  }

  if (!is.matrix(r_i_0)) {
    stop("r_i_0 must be a matrix")
  }

  if (!is.list(r_i_1)) {
    stop("r_i_1 must be a list of matrices")
  }

  if (!is.matrix(p_i_0)) {
    stop("p_i_0 must be a matrix")
  }

  # Check dimensions
  J <- nrow(gamma)
  I <- ncol(gamma)

  if (nrow(r_i_0) != J || ncol(r_i_0) != I) {
    stop("r_i_0 must have dimensions J x I matching gamma")
  }

  if (length(r_i_1) != I) {
    stop("r_i_1 must have I elements")
  }

  if (nrow(p_i_0) != J || ncol(p_i_0) != I) {
    stop("p_i_0 must have dimensions J x I matching gamma")
  }

  # Set maturities if not provided
  if (is.null(maturities)) {
    maturities <- seq_len(I)
  }

  # Validate maturities
  if (any(maturities < 1) || any(maturities > I)) {
    stop("maturities must be between 1 and I")
  }

  # Initialize storage
  n_maturities <- length(maturities)
  L_i <- numeric(n_maturities)
  V_i <- numeric(n_maturities)
  Q_i <- vector("list", n_maturities)

  names(L_i) <- paste0("maturity_", maturities)
  names(V_i) <- paste0("maturity_", maturities)
  names(Q_i) <- paste0("maturity_", maturities)

  # Compute components for each maturity
  for (idx in seq_along(maturities)) {
    i <- maturities[idx]
    gamma_i <- gamma[, i, drop = FALSE] # Keep as column vector

    # L_i = gamma_i^T * R_i^(0)
    L_i[idx] <- as.numeric(t(gamma_i) %*% r_i_0[, i])

    # V_i = gamma_i^T * (P_i^(0) * (P_i^(0))^T) * gamma_i
    p_i_0_vec <- p_i_0[, i, drop = FALSE] # Keep as column vector
    P_i_0_outer <- p_i_0_vec %*% t(p_i_0_vec)
    V_i[idx] <- as.numeric(t(gamma_i) %*% P_i_0_outer %*% gamma_i)

    # Q_i = gamma_i^T * R_i^(1) (this is a vector of length I)
    R_i_1_mat <- r_i_1[[i]]
    if (!is.matrix(R_i_1_mat) || nrow(R_i_1_mat) != J) {
      stop(paste("r_i_1[[", i, "]] must be a J x I matrix"))
    }
    Q_i[[idx]] <- as.numeric(t(gamma_i) %*% R_i_1_mat)
    names(Q_i[[idx]]) <- paste0("maturity_", seq_len(ncol(R_i_1_mat)))
  }

  list(
    L_i = L_i,
    V_i = V_i,
    Q_i = Q_i
  )
}
