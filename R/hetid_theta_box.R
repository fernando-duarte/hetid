#' The hetid_theta_box Container
#'
#' Constructor and validator for the identified-set box at a slack
#' \eqn{\tau}. The box carries the reduced-form pieces it was built from,
#' so a downstream profile cannot be run against a different system by
#' accident.
#'
#' @name hetid_theta_box
#' @keywords internal
NULL

#' Construct a hetid_theta_box Object
#'
#' Low-level cheap constructor. It checks the identity attributes and
#' trusts the shapes of the bounds and witnesses themselves; the full
#' sweep lives in \code{validate_hetid_theta_box()}, which the public
#' boundary \code{compute_identified_set_box()} always runs. Hot paths
#' rebuilding a box from known-good parts may call this directly and skip
#' it.
#'
#' @param bounds Data frame with \code{coef}, \code{lower}, \code{upper}
#' @param arg_lower,arg_upper Numeric I x I matrices whose row k holds the
#'   theta that attains the bound for coordinate k, or \code{NA} when that
#'   bound is infinite
#' @param beta1_bounds Data frame with \code{coef}, \code{lower},
#'   \code{upper}, one row per structural coefficient
#' @param beta1_arg_lower,beta1_arg_upper Numeric p x I matrices whose row
#'   k holds the theta whose image attains the structural bound k, or
#'   \code{NA} when it is infinite
#' @param null_loading Named logical of length p, TRUE where the structural
#'   coefficient's loading was treated as zero
#' @param w1,w2 Reduced-form pieces the box was built from
#' @param quadratic Quadratic form list at this slack
#' @param tau Scalar slack
#' @param n_grid Points per gridded coordinate used by the search
#' @param n_obs Observation count
#' @return A \code{hetid_theta_box} object
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
