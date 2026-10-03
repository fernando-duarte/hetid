#' Controls for Variance Share Searches
#'
#' Defaults for the full containing-box grid and local quadratic share searches.
#' These controls extend \code{\link{QUADRATIC_PROFILE_CONTROL}} without changing it.
#'
#' @format A named list. Requested slacks are c(0.05, 0.10, 0.20). Each grid axis
#'   has 101 points, with at most 2e6 points in total. Coherence ratio is 0.98,
#'   coherence slack is 1e-9, within-block correlation tolerance is 1e-8, and
#'   tau-zero point tolerance is 1e-8. Remaining entries are the quadratic
#'   profile controls.
#' @export
VARIANCE_SHARE_CONTROL <- c(list(
  taus = c(0.05, 0.10, 0.20),
  grid_points_per_axis = 101L,
  grid_points_limit = 2e6,
  coherence_ratio = 0.98,
  coherence_slack = 1e-9,
  orthogonality_tolerance = 1e-8,
  point_tolerance = 1e-8
), QUADRATIC_PROFILE_CONTROL)

validate_variance_share_control <- function(control) {
  assert_bad_argument_ok(
    is.list(control) && !anyDuplicated(names(control)) &&
      all(names(VARIANCE_SHARE_CONTROL) %in% names(control)),
    "control lacks variance share settings",
    arg = "control"
  )
  validate_profile_control(control)
  validate_profile_taus(control$taus, "control$taus")
  assert_bad_argument_ok(!anyDuplicated(control$taus),
    "control$taus must be distinct",
    arg = "control"
  )
  assert_scalar_integer_in_range(
    control$grid_points_per_axis,
    "control$grid_points_per_axis", 2, .Machine$integer.max
  )
  for (key in c(
    "grid_points_limit", "coherence_ratio", "coherence_slack",
    "orthogonality_tolerance", "point_tolerance"
  )) {
    assert_scalar_finite(control[[key]], paste0("control$", key))
  }
  assert_bad_argument_ok(
    all(c(control$grid_points_limit, control$point_tolerance, control$coherence_ratio) > 0) &&
      control$coherence_ratio <= 1 &&
      control$coherence_slack >= 0 && control$orthogonality_tolerance >= 0,
    "Invalid variance share controls",
    arg = "control"
  )
  invisible(control)
}

variance_share_grid_capacity <- function(dimension, control) {
  assert_bad_argument_ok(
    control$grid_points_per_axis^dimension <= control$grid_points_limit,
    "The share grid has too many points for this many news coefficients.",
    arg = "control"
  )
}
