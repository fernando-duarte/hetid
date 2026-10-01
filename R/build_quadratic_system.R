#' Build the Quadratic System for the Identified Set
#'
#' Recommended entry point of the identification chain: computes the
#' components L_i, V_i, Q_i internally from \code{(gamma, moments)} and
#' chains into the quadratic form d_i, A_i, b_i, c_i. Because the
#' components are derived inside the call, a stale components/gamma
#' pairing is impossible on this path.
#'
#' @param gamma Finite numeric matrix (J x I) where each column gamma_i contains the
#'   coefficients for system column i. I must equal the moments'
#'   \code{n_components} attribute and J its instrument count. Each
#'   constrained column must contain at least one nonzero coefficient.
#' @param tau Finite numeric vector of real numbers in \code{[0, 1)} (length I),
#'   indexed by system column. Exact zeros correspond to the
#'   point-identification benchmark.
#' @param moments A \code{hetid_moments} object from
#'   \code{\link{compute_identification_moments}}. Moment values must be
#'   finite, and \code{sigma_i_sq} must be strictly positive for every
#'   constrained maturity.
#'
#' @return A list containing:
#' \describe{
#'   \item{components}{A \code{hetid_components} object with named numeric
#'     vectors \code{L_i} and \code{V_i} and a named list \code{Q_i} of
#'     length-I numeric vectors. It retains the moments' \code{maturities}
#'     and \code{n_components} attributes.}
#'   \item{quadratic}{A list with named numeric vectors \code{d_i} and
#'     \code{c_i}, a named list \code{A_i} of symmetric I x I matrices,
#'     and a named list \code{b_i} of length-I numeric vectors.}
#' }
#' Each per-maturity vector or list within these objects has length
#' \code{length(attr(moments, "maturities"))},
#' with keys \code{maturity_N} for the constrained w2 column indices.
#'
#' @details
#' NA, NaN, or infinite numeric values, all-zero constrained columns of \code{gamma},
#' non-positive \code{sigma_i_sq}, and incompatible dimensions raise
#' structured \code{hetid_error} conditions. No observations are removed.
#' The moments use centered \eqn{1/T} covariances and variances.
#' A candidate \eqn{\theta} belongs to the identified set when
#' \eqn{\theta^\top A_i \theta + b_i^\top \theta + c_i \leq 0}
#' for every constrained maturity. Building the system does not test
#' whether the identified set is empty.
#'
#' @seealso \code{\link{compute_identified_set_components}} and
#'   \code{\link{compute_identified_set_quadratic}} for the component
#'   definitions, and \code{\link{make_system_checker}} for evaluating
#'   every constraint at a candidate theta.
#'
#' @template section-maturity-convention
#'
#' @export
#'
#' @examples
#' local({
#'   old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
#'   on.exit(if (is.null(old_seed)) {
#'     rm(".Random.seed", envir = .GlobalEnv)
#'   } else {
#'     assign(".Random.seed", old_seed, envir = .GlobalEnv)
#'   })
#'   set.seed(42)
#'   n_obs <- 100
#'   J <- 2
#'   I <- 2
#'   w1 <- rnorm(n_obs)
#'   w2 <- matrix(rnorm(n_obs * I), nrow = n_obs, ncol = I)
#'   pcs <- matrix(rnorm(n_obs * J), nrow = n_obs, ncol = J)
#'   gamma <- matrix(rnorm(J * I), nrow = J, ncol = I)
#'   tau <- rep(0.2, I)
#'
#'   moments <- compute_identification_moments(w1, w2, pcs, maturities = 2)
#'   system <- build_quadratic_system(gamma, tau, moments)
#'   print(names(system$quadratic$c_i))
#'
#'   # Non-positive values satisfy this maturity's constraint
#'   check <- make_constraint_checker(
#'     system$quadratic$A_i[[1]],
#'     system$quadratic$b_i[[1]],
#'     system$quadratic$c_i[[1]]
#'   )
#'   print(check(rep(0, I)))
#' })
build_quadratic_system <- function(gamma, tau, moments) {
  components <- compute_identified_set_components(gamma, moments)
  quadratic <- compute_identified_set_quadratic(tau, components, moments)
  list(components = components, quadratic = quadratic)
}
