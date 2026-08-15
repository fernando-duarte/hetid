#' Identified-Set Box at a Positive Slack
#'
#' Computes the coordinate bounds of the identified set for the
#' mean-equation news coefficients at a slack \eqn{\tau > 0}, from a
#' \code{hetid_tau0_fit}. The set is
#' \eqn{\{b : b' A_i b + b_i'b + c_i \le 0\}}, and the returned bounds are
#' the extremes of each coordinate over the part of that set the search
#' reaches.
#'
#' @details
#' The search grids all but one coordinate and solves the remaining one
#' exactly: on any line the constraints reduce to univariate quadratics,
#' so the feasible segment is available in closed form rather than by
#' sampling. Every reported bound is therefore attained at a point that
#' satisfies every constraint, and \code{arg_lower} / \code{arg_upper}
#' name those points.
#'
#' The grid lives in a frame in which the set is locally a cube, built
#' from the \eqn{Q_i} stack and the slack at the center. Gridding the raw
#' coordinates instead loses ill-conditioned sets: when the news columns
#' are nearly collinear the set is a thin diagonal tube, and an
#' axis-aligned grid can miss it between nodes.
#'
#' The center is the \eqn{\tau = 0} point, which lies in every
#' \eqn{\Theta(\tau)} because each constraint relaxes as \eqn{\tau} grows.
#' That is a statement about exact arithmetic, so the point is checked
#' against the assembled system before it is used rather than assumed.
#'
#' @section Interpretation:
#' The result is an \strong{inner approximation} of the axis-aligned
#' bounding box of a \strong{non-convex} set. A point inside the box need
#' not be in the set: \code{\link{make_system_checker}} remains the
#' membership test. Bounds tighten as \code{n_grid} rises, and a spike
#' narrower than roughly one grid step is missed, which matters only when
#' \eqn{\tau} sits very close to the slack at which the set stops being
#' bounded.
#'
#' @param fit A \code{hetid_tau0_fit} from \code{\link{compute_tau0_system}}
#' @param tau Scalar slack in \code{(0, 1)}. Expanded to one value per
#'   component internally
#' @param n_grid Points per gridded coordinate; defaults to
#'   \code{IDENTIFIED_SET_CONTROL$N_GRID}
#' @param center Optional numeric length-I center for the search,
#'   required when the fit carries no \eqn{\tau = 0} point
#' @return A \code{hetid_theta_box} object:
#' \describe{
#'   \item{bounds}{Data frame with \code{coef}, \code{lower}, \code{upper}}
#'   \item{arg_lower, arg_upper}{Row k holds the theta that attains the
#'     bound for coordinate k, \code{NA} where that bound is infinite}
#'   \item{w1, w2, quadratic}{The pieces the box was built from, so a
#'     downstream profile cannot be run against another system}
#' }
#' @template section-maturity-convention
#' @seealso \code{\link{compute_tau0_system}} for the fit this consumes,
#'   \code{\link{make_system_checker}} for membership of the set itself
#' @export
#'
#' @examples
#' set.seed(42)
#' n_obs <- 200
#' x <- cbind(x1 = rnorm(n_obs), x2 = rnorm(n_obs))
#' z <- rnorm(n_obs)
#' e2 <- sqrt(exp(0.5 + 0.9 * z)) * matrix(rnorm(n_obs * 2), n_obs, 2)
#' y2 <- x %*% matrix(c(1, 0.5, -0.3, 0.7), 2, 2) + e2
#' colnames(y2) <- c("news1", "news2")
#' y1 <- drop(0.3 + x %*% c(0.2, -0.1) + y2 %*% c(0.8, -0.5) + rnorm(n_obs))
#'
#' fit <- compute_tau0_system(y1, y2, x, z)
#' box <- compute_identified_set_box(fit, tau = 0.05)
#' box$bounds
compute_identified_set_box <- function(fit, tau,
                                       n_grid = IDENTIFIED_SET_CONTROL$N_GRID,
                                       center = NULL) {
  assert_hetid_tau0_fit(fit)
  assert_scalar_finite(tau, "tau")
  assert_tau_values_ok(tau)
  assert_bad_argument_ok(
    tau > 0,
    "tau must be strictly positive; the tau = 0 point is compute_tau0_point()",
    arg = "tau"
  )
  assert_scalar_integer_in_range(n_grid, "n_grid", 2, .Machine$integer.max)

  n_components <- ncol(fit$w2)
  built <- build_quadratic_system(
    fit$gamma, rep(tau, n_components), fit$moments
  )
  quadratic <- built$quadratic
  center <- resolve_box_center(fit, center, quadratic, n_components)
  basis <- identified_set_basis(built$components, center, quadratic)

  found <- identified_set_search(center, basis, quadratic, n_grid)
  found <- apply_recession_bounds(found, quadratic)

  out <- validate_hetid_theta_box(new_hetid_theta_box(
    bounds = data.frame(
      coef = theta_box_labels(fit$w2, n_components),
      lower = found$lower,
      upper = found$upper,
      row.names = NULL
    ),
    arg_lower = found$arg_lower,
    arg_upper = found$arg_upper,
    w1 = fit$w1,
    w2 = fit$w2,
    quadratic = quadratic,
    tau = tau,
    n_grid = n_grid,
    n_obs = length(fit$w1)
  ))
  out
}

#' Resolve and Check the Search Center
#'
#' @param fit A \code{hetid_tau0_fit}
#' @param center Optional caller-supplied center
#' @param quadratic Quadratic form list at this slack
#' @param n_components Theta-axis dimension
#' @return Numeric length-I feasible center
#' @noRd
resolve_box_center <- function(fit, center, quadratic, n_components) {
  if (is.null(center)) {
    assert_bad_argument_ok(
      !is.null(fit$point),
      paste0(
        "fit carries no tau = 0 point to center the search on; ",
        "supply center explicitly"
      ),
      arg = "center"
    )
    center <- fit$point$theta
  }
  assert_dimension_ok(
    length(center) == n_components,
    paste0(
      "center must have one value per component: length = ", length(center),
      "; n_components = ", n_components
    )
  )
  assert_numeric_finite_values(center, "center")
  slack <- max(make_system_checker(quadratic)(center))
  assert_bad_argument_ok(
    slack < 0,
    paste0(
      "center is not strictly inside the set at this tau (largest ",
      "constraint value ", format(slack), "); supply a feasible center"
    ),
    arg = "center"
  )
  center
}

#' Component Labels for the Bounds Frame
#'
#' @param w2 Reduced-form news matrix
#' @param n_components Theta-axis dimension
#' @return Character vector of component labels
#' @noRd
theta_box_labels <- function(w2, n_components) {
  coef_labels <- colnames(w2)
  if (is.null(coef_labels)) {
    coef_labels <- maturity_names(seq_len(n_components))
  }
  coef_labels
}
