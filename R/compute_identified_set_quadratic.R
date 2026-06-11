#' Compute Quadratic Form Components for Identified Set
#'
#' Computes the quadratic form components d_i, A_i, b_i, and c_i for the
#' identified set calculation for each maturity i.
#'
#' @param tau Vector of real numbers in \code{[0, 1)} (length I, the
#'   moments' \code{n_components}) containing tau_i values, indexed by
#'   system column. Exact zeros are allowed and correspond to the
#'   point-identification benchmark.
#' @param components A \code{hetid_components} object from
#'   \code{\link{compute_identified_set_components}}
#' @param moments The \code{hetid_moments} object the components were
#'   computed from (see
#'   \code{\link{compute_identification_moments}}). Its
#'   \code{sigma_i_sq} values must be finite and strictly positive
#'   (zero indicates no heteroskedasticity to exploit for
#'   identification).
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
#' \deqn{d_i = \tau_i^2 V_i / \hat{\sigma}_i^2}
#' \deqn{A_i = Q_i Q_i^\top - d_i \hat{S}_i^{(2)}}
#' \deqn{b_i = -2 L_i Q_i + 2 d_i \hat{S}_i^{(1)}}
#' \deqn{c_i = L_i^2 - d_i \hat{S}_i^{(0)}}
#'
#' where \eqn{A_i} is symmetric by construction,
#' \eqn{b_i} is a vector, and \eqn{c_i} is a scalar.
#' The identified set is:
#' \deqn{\Theta = \{\theta \in R^I :
#'   \theta^\top A_i \theta + b_i^\top \theta +
#'   c_i \leq 0\}}
#' with one constraint for each \code{i} in the container's
#' \code{maturities} (not necessarily \eqn{1, \ldots, I}; see the
#' maturity convention below).
#'
#' The components and moments must carry identical \code{maturities} and
#' \code{n_components} attributes (they do when the components were
#' computed from these moments); a mismatch raises a structured
#' dimension error. Prefer \code{\link{build_quadratic_system}}, which
#' computes the components internally from \code{(gamma, moments)} and
#' makes a stale pairing impossible.
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
#' tau <- rep(0.2, I)
#'
#' moments <- compute_identification_moments(w1, w2, pcs)
#' components <- compute_identified_set_components(gamma, moments)
#' quad <- compute_identified_set_quadratic(tau, components, moments)
compute_identified_set_quadratic <- function(tau, components, moments) {
  validation <- validate_quadratic_inputs(tau, components, moments)

  quadratic_from_components(
    tau = tau,
    L_i = components$L_i, V_i = components$V_i, Q_i = components$Q_i,
    s_i_0 = moments$s_i_0, s_i_1 = moments$s_i_1, s_i_2 = moments$s_i_2,
    sigma_i_sq = moments$sigma_i_sq,
    maturities = validation$maturities,
    n_components = validation$n_components
  )
}

#' Compute the Quadratic Form from Raw Components
#'
#' Internal workhorse holding the d/A/b/c arithmetic. Inputs are raw
#' position-indexed vectors/lists already validated (or hand-built in
#' tests); \code{maturities} supplies the value mapping for element k.
#'
#' @param tau Vector of length \code{n_components}, indexed by system
#'   column
#' @param L_i,V_i,s_i_0,sigma_i_sq Position-indexed numeric vectors
#' @param Q_i,s_i_1,s_i_2 Position-indexed lists of theta-axis objects
#' @param maturities Integer vector; element k maps to
#'   \code{maturities[k]}
#' @param n_components Theta-axis dimension (I)
#'
#' @return A list with d_i, A_i, b_i, c_i
#' @keywords internal
quadratic_from_components <- function(tau,
                                      L_i, V_i, Q_i, # nolint: object_name_linter.
                                      s_i_0, s_i_1, s_i_2, sigma_i_sq,
                                      maturities, n_components) {
  n_maturities <- length(maturities)

  # Initialize storage
  d_i <- numeric(n_maturities)
  A_i <- vector("list", n_maturities) # nolint: object_name_linter.
  b_i <- vector("list", n_maturities)
  c_i <- numeric(n_maturities)

  names(d_i) <- maturity_names(maturities)
  names(A_i) <- maturity_names(maturities) # nolint: object_name_linter.
  names(b_i) <- maturity_names(maturities)
  names(c_i) <- maturity_names(maturities)

  # Compute quadratic form components for each maturity
  for (idx in seq_along(maturities)) {
    i <- maturities[idx]

    quad <- assemble_constraint_quadratic(
      tau_ik = tau[i],
      l_val = L_i[idx], v_val = V_i[idx], q_vec = Q_i[[idx]],
      s_i_0_val = s_i_0[idx], s_i_1_vec = s_i_1[[idx]],
      s_i_2_mat = s_i_2[[idx]],
      sigma_i_sq_val = sigma_i_sq[idx],
      n_components = n_components,
      label = paste0("maturity ", i)
    )
    d_i[idx] <- quad$d
    A_i[[idx]] <- quad$A # nolint: object_name_linter.
    b_i[[idx]] <- quad$b
    c_i[idx] <- quad$c
  }

  list(
    d_i = d_i,
    A_i = A_i,
    b_i = b_i,
    c_i = c_i
  )
}
