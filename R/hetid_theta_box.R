#' The hetid_theta_box Container
#'
#' Container for identified-set coordinate bounds and structural-coefficient
#' bounds at a slack \eqn{\tau}. It retains the reduced-form residuals and
#' quadratic constraints used to construct the box.
#'
#' @seealso \code{\link{compute_identified_set_box}} for the public constructor
#'   and the approximation and membership caveats.
#'
#' @name hetid_theta_box
#' @keywords internal
NULL

#' Construct a hetid_theta_box Object
#'
#' Constructs a box from previously computed bounds, attaining points, and
#' reduced-form sources.
#'
#' @details
#' Only \code{tau}, \code{n_grid}, and \code{n_obs} are checked here. Bounds,
#' witness shapes, and residual dimensions are checked by
#' \code{\link{validate_hetid_theta_box}}, which the public constructor
#' \code{\link{compute_identified_set_box}} always runs. Internal callers
#' rebuilding a box from known-good parts may skip that validation.
#' Invalid scalar metadata raises a structured \code{hetid_error} condition.
#' All other inputs are retained without validation or missing-value removal.
#' The validator does not test witness feasibility or verify that the residuals
#' and quadratic constraints describe the same estimated system.
#'
#' Here \eqn{I = \mathrm{ncol}(w2)} is the theta-axis dimension, and
#' \eqn{p} is the number of structural coefficients, including the intercept.
#'
#' @param bounds Data frame with columns \code{coef}, \code{lower}, and
#'   \code{upper}, one row per theta coordinate. Coefficient labels are unique
#'   and non-missing; numeric bounds are non-missing, with \code{lower <= upper}.
#'   Infinite sides are represented by \code{-Inf} and \code{Inf}, respectively.
#' @param arg_lower,arg_upper Numeric \eqn{I \times I} matrices whose row
#'   \eqn{k} holds the theta attaining the lower or upper bound for coordinate
#'   \eqn{k}, respectively; the row is \code{NA} when that bound is infinite.
#' @param beta1_bounds Data frame with \code{coef}, \code{lower},
#'   \code{upper}, one row per structural coefficient, with the same bounds
#'   conventions as \code{bounds}.
#' @param beta1_arg_lower,beta1_arg_upper Numeric \eqn{p \times I} matrices
#'   whose row \eqn{k} holds the theta whose image attains structural bound
#'   \eqn{k}, or \code{NA} when it is infinite. For flagged coefficients,
#'   attainment refers to the map with their loadings treated as zero.
#' @param null_loading Non-missing logical vector of length \eqn{p}, named by
#'   \code{beta1_bounds$coef}; \code{TRUE} where the structural coefficient's
#'   loading was treated as zero.
#' @param w1,w2 Numeric residual vector of length \code{n_obs} and numeric
#'   residual matrix with \code{n_obs} rows and \eqn{I} columns, respectively,
#'   from the same reduced-form system and aligned sample.
#' @param quadratic Quadratic constraint list at this slack, containing
#'   \code{A_i}, \code{b_i}, and \code{c_i}.
#' @param tau Single finite numeric slack. This constructor does not restrict
#'   its range; the public constructor requires a value in \code{(0, 1)}.
#' @param n_grid Integer-valued numeric scalar between \code{2} and
#'   \code{.Machine$integer.max}, inclusive, giving the search points per
#'   gridded coordinate. The public constructor additionally requires an odd
#'   value of at least \code{3}.
#' @param n_obs Integer-valued numeric observation count between \code{1} and
#'   \code{.Machine$integer.max}, inclusive.
#' @return A list of class \code{hetid_theta_box} with the supplied bounds,
#'   witness matrices, and reduced-form sources as elements. Attributes retain
#'   \code{tau}, \code{n_components = ncol(w2)}, \code{n_grid}, \code{n_obs},
#'   and \code{null_loading}; the counts \code{n_grid} and \code{n_obs} are stored
#'   as integers.
#' @keywords internal
new_hetid_theta_box <- function(bounds, arg_lower, arg_upper,
                                beta1_bounds, beta1_arg_lower, beta1_arg_upper,
                                null_loading, w1, w2, quadratic, tau, n_grid,
                                n_obs) {
  assert_scalar_finite(tau, "tau")
  assert_scalar_integer_in_range(n_grid, "n_grid", 2, .Machine$integer.max)
  assert_scalar_integer_in_range(n_obs, "n_obs", 1, .Machine$integer.max)

  structure(
    list(
      bounds = bounds,
      arg_lower = arg_lower,
      arg_upper = arg_upper,
      beta1_bounds = beta1_bounds,
      beta1_arg_lower = beta1_arg_lower,
      beta1_arg_upper = beta1_arg_upper,
      w1 = w1,
      w2 = w2,
      quadratic = quadratic
    ),
    tau = tau,
    n_components = ncol(w2),
    n_grid = as.integer(n_grid),
    n_obs = as.integer(n_obs),
    null_loading = null_loading,
    class = "hetid_theta_box"
  )
}
