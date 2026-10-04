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
  TAUS = c(0.05, 0.10, 0.20),
  GRID_POINTS_PER_AXIS = 101L,
  GRID_POINTS_LIMIT = HETID_CONSTANTS$GRID_POINTS_LIMIT,
  COHERENCE_RATIO = 0.98,
  COHERENCE_SLACK = 1e-9,
  ORTHOGONALITY_TOLERANCE = 1e-8,
  POINT_TOLERANCE = 1e-8
), QUADRATIC_PROFILE_CONTROL)

validate_variance_share_control <- function(control) {
  assert_bad_argument_ok(
    is.list(control) && !anyDuplicated(names(control)) &&
      all(names(VARIANCE_SHARE_CONTROL) %in% names(control)),
    "control lacks variance share settings",
    arg = "control"
  )
  validate_profile_control(control)
  validate_profile_taus(control$TAUS, "control$TAUS")
  assert_bad_argument_ok(!anyDuplicated(control$TAUS),
    "control$TAUS must be distinct",
    arg = "control"
  )
  assert_scalar_integer_in_range(
    control$GRID_POINTS_PER_AXIS,
    "control$GRID_POINTS_PER_AXIS", 2, .Machine$integer.max
  )
  for (key in c(
    "GRID_POINTS_LIMIT", "COHERENCE_RATIO", "COHERENCE_SLACK",
    "ORTHOGONALITY_TOLERANCE", "POINT_TOLERANCE"
  )) {
    assert_scalar_finite(control[[key]], paste0("control$", key))
  }
  assert_bad_argument_ok(
    all(c(control$GRID_POINTS_LIMIT, control$POINT_TOLERANCE, control$COHERENCE_RATIO) > 0) &&
      control$COHERENCE_RATIO <= 1 &&
      control$COHERENCE_SLACK >= 0 && control$ORTHOGONALITY_TOLERANCE >= 0,
    "Invalid variance share controls",
    arg = "control"
  )
  invisible(control)
}

variance_share_grid_capacity <- function(dimension, control) {
  assert_bad_argument_ok(
    control$GRID_POINTS_PER_AXIS^dimension <= control$GRID_POINTS_LIMIT,
    "The share grid has too many points for this many news coefficients.",
    arg = "control"
  )
}
