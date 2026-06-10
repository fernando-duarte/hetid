#' Build the Quadratic System for the Identified Set
#'
#' Recommended entry point of the identification chain: computes the
#' components L_i, V_i, Q_i internally from \code{(gamma, moments)} and
#' chains into the quadratic form d_i, A_i, b_i, c_i. Because the
#' components are derived inside the call, a stale components/gamma
#' pairing is impossible on this path.
#'
#' @param gamma Matrix (J x I) where each column gamma_i contains the
#'   coefficients for system column i. I must equal the moments'
#'   \code{n_components} attribute and J its instrument count.
#' @param tau Vector of real numbers in \code{[0, 1)} (length I),
#'   indexed by system column. Exact zeros correspond to the
#'   point-identification benchmark.
#' @param moments A \code{hetid_moments} object from
#'   \code{\link{compute_identification_moments}}
#'
#' @return A list containing:
#' \describe{
#'   \item{components}{The \code{hetid_components} object (L_i, V_i, Q_i)}
#'   \item{quadratic}{The quadratic form list (d_i, A_i, b_i, c_i)}
#' }
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
#' system <- build_quadratic_system(gamma, tau, moments)
#'
#' # Evaluate one constraint: negative values are inside the set
#' check <- make_constraint_checker(
#'   system$quadratic$A_i[[1]],
#'   system$quadratic$b_i[[1]],
#'   system$quadratic$c_i[[1]]
#' )
#' check(rep(0, I))
build_quadratic_system <- function(gamma, tau, moments) {
  components <- compute_identified_set_components(gamma, moments)
  quadratic <- compute_identified_set_quadratic(tau, components, moments)
  list(components = components, quadratic = quadratic)
}
