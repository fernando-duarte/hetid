#' Identified-Set Box at a Positive Slack
#'
#' Computes the coordinate bounds of the identified set for the
#' mean-equation news coefficients at a slack \eqn{\tau > 0}, and the
#' bounds of the structural coefficients
#' \eqn{\beta_1(\theta) = \beta_1^R - (\beta_2^R)'\theta} over the same
#' set, from a \code{hetid_tau0_fit}. The set is
#' \eqn{\{\theta : \theta' A_i \theta + b_i'\theta + c_i \le 0\}}, and the
#' returned bounds are the extremes of each coordinate, and of each
#' structural coefficient, over the part of that set the search reaches.
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
#' The structural block is the affine image of the same set under
#' \code{\link{recover_structural_coefficients}}, so its bounds are the
#' extremes of \eqn{p} linear functionals of \eqn{\theta}. A linear
#' functional is affine along any line and therefore attains its extreme
#' on a line at a hull endpoint, exactly as a coordinate does, so the
#' block is read off the same line hulls. The window grows for the
#' coordinates first, along exactly the path they take on their own, and
#' only then for the structural coefficients, so the theta block equals a
#' coordinates-only search after that first phase and can only widen
#' afterwards. A loading column that is zero up to rounding, with no entry
#' above \code{null_loading_rtol} times its row's largest loading, is
#' treated as exactly zero and flagged in the \code{null_loading}
#' attribute: the structural bounds are then those of the map with that
#' column zeroed, and the coefficient is reported as the point
#' \code{fit$beta1r[k]}, which the unsnapped map reproduces to rounding.
#' The rule assumes the columns of \code{x} are comparably scaled; with a
#' column scaled up by a factor of order \code{1 / null_loading_rtol} its
#' genuine loading would be snapped too, so pass \code{null_loading_rtol =
#' 0} to snap exact zeros only. When \code{fit} was built with
#' \code{impose_null = TRUE} every loading is exactly zero and every
#' structural row is such a point at any tolerance. When a recession
#' direction is found, every coordinate and every structural coefficient
#' with a non-zero loading is unbounded on both sides.
#'
#' @section Interpretation:
#' The result is an \strong{inner approximation} of the axis-aligned
#' bounding box of a \strong{non-convex} set. A point inside the box need
#' not be in the set: \code{\link{make_system_checker}} remains the
#' membership test. Bounds tighten as \code{n_grid} rises, and a spike
#' narrower than roughly one grid step is missed, which matters only when
#' \eqn{\tau} sits very close to the slack at which the set stops being
#' bounded.
#' The structural block is likewise the bounding box of the image of a
#' non-convex set: a vector of structural coefficients inside it need not
#' be attainable jointly, and the only membership check is to map a
#' feasible \eqn{\theta} through \code{\link{recover_structural_coefficients}}.
#'
#' @param fit A \code{hetid_tau0_fit} from \code{\link{compute_tau0_system}}
#' @param tau Scalar slack in \code{(0, 1)}. Expanded to one value per
#'   component internally
#' @param n_grid Odd number of points per gridded coordinate, so the grid
#'   contains the centre; defaults to \code{IDENTIFIED_SET_CONTROL$N_GRID}
#' @param center Optional numeric length-I center for the search,
#'   required when the fit carries no \eqn{\tau = 0} point
#' @param null_loading_rtol Scalar in \code{[0, 1)}; defaults to
#'   \code{IDENTIFIED_SET_CONTROL$NULL_LOADING_RTOL}. A structural loading
#'   column with no entry above this fraction of its row's largest loading
#'   is treated as exactly zero; \code{0} snaps only exact zeros
#' @return A \code{hetid_theta_box} object:
#' \describe{
#'   \item{bounds}{Data frame with \code{coef}, \code{lower}, \code{upper}}
#'   \item{arg_lower, arg_upper}{Row k holds the theta that attains the
#'     bound for coordinate k, \code{NA} where that bound is infinite}
#'   \item{beta1_bounds}{Data frame with \code{coef}, \code{lower},
#'     \code{upper}, one row per element of \code{fit$beta1r} (the intercept
#'     and each column of \code{x})}
#'   \item{beta1_arg_lower, beta1_arg_upper}{Row k holds the theta whose
#'     image attains that bound, \code{NA} where it is infinite}
#'   \item{w1, w2, quadratic}{The pieces the box was built from, so a
#'     downstream profile cannot be run against another system}
#' }
#' The attribute \code{null_loading} is a named logical, one entry per
#' structural coefficient, TRUE where its loading was treated as zero.
#' @template section-maturity-convention
#' @seealso \code{\link{compute_tau0_system}} for the fit this consumes,
#'   \code{\link{make_system_checker}} for membership of the set itself,
#'   \code{\link{recover_structural_coefficients}} for the map behind the
#'   structural block
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
#' box$beta1_bounds
compute_identified_set_box <- function(fit, tau,
                                       n_grid = IDENTIFIED_SET_CONTROL$N_GRID,
                                       center = NULL,
                                       null_loading_rtol =
                                         IDENTIFIED_SET_CONTROL$NULL_LOADING_RTOL) {
  assert_hetid_tau0_fit(fit)
  assert_scalar_finite(tau, "tau")
  assert_tau_values_ok(tau)
  assert_bad_argument_ok(
    tau > 0,
    "tau must be strictly positive; the tau = 0 point is compute_tau0_point()",
    arg = "tau"
  )
  assert_scalar_integer_in_range(n_grid, "n_grid", 3, .Machine$integer.max)
  assert_bad_argument_ok(
    n_grid %% 2L == 1L,
    "n_grid must be odd so the grid contains the centre",
    arg = "n_grid"
  )
  assert_scalar_finite(null_loading_rtol, "null_loading_rtol")
  assert_bad_argument_ok(
    null_loading_rtol >= 0 && null_loading_rtol < 1,
    "null_loading_rtol must lie in [0, 1); 0 snaps only exact zeros",
    arg = "null_loading_rtol"
  )

  n_components <- ncol(fit$w2)
  validate_box_fit(fit, n_components)
  built <- build_quadratic_system(
    fit$gamma, rep(tau, n_components), fit$moments
  )
  quadratic <- built$quadratic
  center <- resolve_box_center(fit, center, quadratic, n_components)
  basis <- identified_set_basis(built$components, center, quadratic)
  objectives <- identified_set_objectives(fit, n_components, null_loading_rtol)
  theta_rows <- seq_len(n_components)
  beta1_rows <- n_components + seq_along(fit$beta1r)

  found <- identified_set_search(
    center, basis, quadratic, n_grid, objectives,
    n_primary = n_components
  )
  found <- apply_recession_bounds(found, quadratic, objectives)

  out <- validate_hetid_theta_box(new_hetid_theta_box(
    bounds = identified_set_bounds_frame(
      theta_box_labels(fit$w2, n_components), 0, found, theta_rows
    ),
    arg_lower = found$arg_lower[theta_rows, , drop = FALSE],
    arg_upper = found$arg_upper[theta_rows, , drop = FALSE],
    beta1_bounds = identified_set_bounds_frame(
      names(fit$beta1r), unname(fit$beta1r), found, beta1_rows
    ),
    beta1_arg_lower = found$arg_lower[beta1_rows, , drop = FALSE],
    beta1_arg_upper = found$arg_upper[beta1_rows, , drop = FALSE],
    null_loading = stats::setNames(
      colSums(objectives[, beta1_rows, drop = FALSE] != 0) == 0L,
      names(fit$beta1r)
    ),
    w1 = fit$w1,
    w2 = fit$w2,
    quadratic = quadratic,
    tau = tau,
    n_grid = n_grid,
    n_obs = length(fit$w1)
  ))
  out
}
