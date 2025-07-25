#' Compute Quadratic Form Components for Identified Set
#'
#' Computes the quadratic form components d_i, A_i, b_i, and c_i for the
#' identified set calculation for each maturity i.
#'
#' @param gamma Matrix (J x I) where each column gamma_i contains the
#'   coefficients for maturity i
#' @param tau Vector of positive real numbers (length I) containing tau_i values
#' @param L_i Named vector of L_i values from compute_identified_set_components()
#' @param V_i Named vector of V_i values from compute_identified_set_components()
#' @param Q_i List of vectors Q_i from compute_identified_set_components()
#' @param s_i_0 Named vector of S_i^(0) values from compute_scalar_statistics()
#' @param s_i_1 List of vectors S_i^(1) from compute_matrix_statistics()
#' @param s_i_2 List of matrices S_i^(2) from compute_matrix_statistics()
#' @param sigma_i_sq Named vector of sigma_i^2 values from
#'   compute_scalar_statistics()
#' @param maturities Vector of maturity indices to compute components for.
#'   Default is all available.
#'
#' @return A list containing:
#' \describe{
#'   \item{d_i}{Named vector of d_i values for each maturity}
#'   \item{A_i}{List of symmetric matrices A_i for each maturity}
#'   \item{b_i}{List of vectors b_i for each maturity}
#'   \item{c_i}{Named vector of c_i values for each maturity}
#' }
#'
#' @details
#' For each maturity i, computes:
#' \deqn{d_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) =
#'   \frac{\tau_i^2 V_i(\boldsymbol{\Gamma})}{\hat{\sigma}_i^2}}
#' \deqn{\mathbf{A}_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) =
#'   \mathbf{Q}_i(\boldsymbol{\Gamma}) \mathbf{Q}_i(\boldsymbol{\Gamma})^{\top} -
#'   d_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) \hat{\mathbf{S}}_i^{(2)}}
#' \deqn{\mathbf{b}_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) =
#'   -2 L_i(\boldsymbol{\Gamma}) \mathbf{Q}_i(\boldsymbol{\Gamma}) +
#'   2 d_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) \hat{\mathbf{S}}_i^{(1)}}
#' \deqn{c_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) =
#'   L_i(\boldsymbol{\Gamma})^2 -
#'   d_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) \hat{S}_i^{(0)}}
#'
#' where \eqn{\mathbf{A}_i} is a symmetric matrix by construction,
#' \eqn{\mathbf{b}_i} is a vector, and \eqn{c_i} is a scalar.
#' The identified set for \eqn{\boldsymbol{\theta} \in \mathbb{R}^I} is:
#' \deqn{\Theta(\boldsymbol{\Gamma}, \boldsymbol{\tau}) =
#'   \left\{\boldsymbol{\theta} \in \mathbb{R}^I :
#'   \boldsymbol{\theta}^{\top} \mathbf{A}_i(\boldsymbol{\Gamma}, \boldsymbol{\tau})
#'   \boldsymbol{\theta} +
#'   \mathbf{b}_i(\boldsymbol{\Gamma}, \boldsymbol{\tau})^{\top} \boldsymbol{\theta} +
#'   c_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) \leq 0, \; i = 1, \ldots, I\right\}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume we have all required statistics computed
#' vec_stats <- compute_vector_statistics(w1, w2, pcs)
#' scalar_stats <- compute_scalar_statistics(w1, w2)
#' matrix_stats <- compute_matrix_statistics(w1, w2)
#'
#' # Create gamma matrix and tau vector
#' J <- 4 # number of PCs
#' I <- 8 # number of maturities
#' gamma <- matrix(rnorm(J * I), nrow = J, ncol = I)
#' tau <- rep(1, I) # positive values
#'
#' # Compute basic components
#' components <- compute_identified_set_components(
#'   gamma = gamma,
#'   r_i_0 = vec_stats$r_i_0,
#'   r_i_1 = vec_stats$r_i_1,
#'   p_i_0 = vec_stats$p_i_0
#' )
#'
#' # Compute quadratic form components
#' quad_components <- compute_identified_set_quadratic(
#'   gamma = gamma,
#'   tau = tau,
#'   L_i = components$L_i,
#'   V_i = components$V_i,
#'   Q_i = components$Q_i,
#'   s_i_0 = scalar_stats$s_i_0,
#'   s_i_1 = matrix_stats$s_i_1,
#'   s_i_2 = matrix_stats$s_i_2,
#'   sigma_i_sq = scalar_stats$sigma_i_sq
#' )
#' }
compute_identified_set_quadratic <- function(gamma, tau, L_i, V_i, Q_i,
                                             s_i_0, s_i_1, s_i_2, sigma_i_sq,
                                             maturities = NULL) {
  # Validate inputs
  if (!is.matrix(gamma)) {
    stop("gamma must be a matrix")
  }

  if (!is.numeric(tau) || !is.vector(tau)) {
    stop("tau must be a numeric vector")
  }

  if (any(tau <= 0)) {
    stop("All elements of tau must be positive")
  }

  I <- ncol(gamma)

  if (length(tau) != I) {
    stop("tau must have length I (number of columns in gamma)")
  }

  if (!is.numeric(L_i) || !is.numeric(V_i)) {
    stop("L_i and V_i must be numeric vectors")
  }

  if (!is.list(Q_i)) {
    stop("Q_i must be a list")
  }

  if (!is.numeric(s_i_0) || !is.numeric(sigma_i_sq)) {
    stop("s_i_0 and sigma_i_sq must be numeric vectors")
  }

  if (!is.list(s_i_1) || !is.list(s_i_2)) {
    stop("s_i_1 and s_i_2 must be lists")
  }

  # Set maturities if not provided
  if (is.null(maturities)) {
    maturities <- seq_len(min(length(L_i), length(V_i), length(Q_i)))
  }

  # Initialize storage
  n_maturities <- length(maturities)
  d_i <- numeric(n_maturities)
  A_i <- vector("list", n_maturities)
  b_i <- vector("list", n_maturities)
  c_i <- numeric(n_maturities)

  names(d_i) <- paste0("maturity_", maturities)
  names(A_i) <- paste0("maturity_", maturities)
  names(b_i) <- paste0("maturity_", maturities)
  names(c_i) <- paste0("maturity_", maturities)

  # Compute quadratic form components for each maturity
  for (idx in seq_along(maturities)) {
    i <- maturities[idx]

    # Extract values for maturity i
    L_i_val <- L_i[idx]
    V_i_val <- V_i[idx]
    Q_i_vec <- Q_i[[idx]]
    s_i_0_val <- s_i_0[idx]
    s_i_1_vec <- s_i_1[[i]]
    s_i_2_mat <- s_i_2[[i]]
    sigma_i_sq_val <- sigma_i_sq[idx]
    tau_i <- tau[i]

    # Validate dimensions
    if (!is.numeric(Q_i_vec) || length(Q_i_vec) != I) {
      stop(paste("Q_i[[", idx, "]] must be a numeric vector of length I"))
    }

    if (!is.numeric(s_i_1_vec) || length(s_i_1_vec) != I) {
      stop(paste("s_i_1[[", i, "]] must be a numeric vector of length I"))
    }

    if (!is.matrix(s_i_2_mat) || nrow(s_i_2_mat) != I || ncol(s_i_2_mat) != I) {
      stop(paste("s_i_2[[", i, "]] must be an I x I matrix"))
    }

    # Compute d_i = tau_i^2 * V_i / sigma_i^2
    d_i[idx] <- (tau_i^2 * V_i_val) / sigma_i_sq_val

    # Compute A_i = Q_i * Q_i^T - d_i * S_i^(2)
    Q_i_outer <- outer(Q_i_vec, Q_i_vec)
    A_i[[idx]] <- Q_i_outer - d_i[idx] * s_i_2_mat

    # Verify A_i is symmetric (it should be by construction)
    if (!isSymmetric(A_i[[idx]], tol = 1e-10)) {
      # Force exact symmetry
      A_i[[idx]] <- (A_i[[idx]] + t(A_i[[idx]])) / 2
    }

    # Compute b_i = -2 * L_i * Q_i + 2 * d_i * S_i^(1)
    b_i[[idx]] <- -2 * L_i_val * Q_i_vec + 2 * d_i[idx] * s_i_1_vec
    names(b_i[[idx]]) <- paste0("maturity_", seq_len(I))

    # Compute c_i = L_i^2 - d_i * S_i^(0)
    c_i[idx] <- L_i_val^2 - d_i[idx] * s_i_0_val
  }

  list(
    d_i = d_i,
    A_i = A_i,
    b_i = b_i,
    c_i = c_i
  )
}
