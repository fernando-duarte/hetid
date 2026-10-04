#' Profile Coordinate and Structural Coefficient Ranges
#'
#' Runs scaled linear endpoint searches and multistart widening over the joint
#' quadratic set. Geometry establishes boundedness separately from optimization.
#'
#' @param quadratic Nonempty finite symmetric system with A_i, b_i and c_i.
#' @param beta1r Named finite structural-offset vector.
#' @param beta2r Finite matrix with theta-named rows and columns named as beta1r.
#' @param points Optional finite matrix of candidate anchors, one point per row.
#' @param warm Optional list of finite starting vectors in theta order.
#' @param control List containing the QUADRATIC_PROFILE_CONTROL settings.
#' @return A list with beta1 and theta tables. Both contain coef, set_lower,
#'   set_upper, status, lower_status and upper_status. Theta additionally contains
#'   outer_lower and outer_upper, verified containing bounds. Side statuses are
#'   bounded, unbounded or unreliable. Bounded denotes geometry plus an accepted
#'   attained candidate; it does not prove that the candidate is a global extremum.
#'   A structural coefficient whose beta2r loading moves it by at most rounding
#'   error over the verified theta enclosure is bounded by that enclosure instead.
#'   Missing endpoints remain unreliable. The profile_points attribute contains
#'   checked points from multistart widening. Their order is part of the warm path.
#' @details Structural values use beta1r minus \code{sum(beta2r[, j] * theta)} to retain
#'   the donor arithmetic. Positive beta2r objectives swap the lower and upper
#'   sides in this affine map. The search fixes the RNG kind and restores caller
#'   state through with_rng_scope(); its documented Box-Muller caveat applies.
#'   An unrepresentable derived endpoint box supplies no candidate. A mandatory
#'   multistart box that exceeds the numeric range raises hetid_error_solver.
#' @export
#' @examples
#' quadratic <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -1)
#' beta1r <- c(intercept = 0.5)
#' beta2r <- matrix(0.2, dimnames = list("news", "intercept"))
#' control <- QUADRATIC_PROFILE_CONTROL
#' control$SOLVER_BOXES <- c(10, 100, 1000)
#' control$MULTISTART_ROUNDS <- 1L
#' profile <- profile_quadratic_coefficients(
#'   quadratic, beta1r, beta2r,
#'   points = matrix(0), control = control
#' )
#' profile$theta
#' profile$beta1
profile_quadratic_coefficients <- function(quadratic, beta1r, beta2r,
                                           points = NULL, warm = NULL,
                                           control = QUADRATIC_PROFILE_CONTROL) {
  dimension <- quadratic_validate_system(quadratic)
  validate_profile_betas(beta1r, beta2r, dimension)
  validate_profile_control(control)
  points <- quadratic_validate_rows(points, "points", dimension)
  assert_bad_argument_ok(is.null(warm) || is.list(warm),
    "warm must be a list of starting vectors",
    arg = "warm"
  )
  for (point in warm) {
    assert_bad_argument_ok(quadratic_real_finite(point) && is.null(dim(point)) &&
      length(point) == dimension, "warm contains an invalid point", arg = "warm")
  }
  with_rng_scope(profile_tables_widened(quadratic, beta1r, beta2r, points, warm, control),
    kind = c("Mersenne-Twister", "Inversion", "Rejection")
  )
}
