#' Controls for Mean-Set Tau Brackets and Grids
#'
#' @format A named list. The sweep cap is 0.99 and step is 0.005, followed by at
#'   most 40 bisections. Grids use a 25-point backbone and subdivide intervals
#'   in the last 10 percent into four parts, preserving the backbone maximum.
#' @export
MEAN_TAU_CONTROL <- list(
  CAP = 0.99, SWEEP_STEP = 0.005, BISECTION_ITERATIONS = 40L,
  GRID_BACKBONE = 25L, GRID_TAIL_FRACTION = 0.9, GRID_TAIL_SUBDIVISIONS = 4L
)

validate_mean_tau_control <- function(control) {
  assert_bad_argument_ok(
    is.list(control) && !anyDuplicated(names(control)) &&
      all(names(MEAN_TAU_CONTROL) %in% names(control)),
    "control lacks mean tau settings",
    arg = "control"
  )
  for (key in c("CAP", "SWEEP_STEP", "GRID_TAIL_FRACTION")) {
    assert_scalar_finite(control[[key]], paste0("control$", key))
  }
  validate_mean_tau_ranges(control)
  for (key in c("BISECTION_ITERATIONS", "GRID_BACKBONE", "GRID_TAIL_SUBDIVISIONS")) {
    minimum <- switch(key,
      BISECTION_ITERATIONS = 1,
      GRID_BACKBONE = 3,
      2
    )
    assert_scalar_integer_in_range(
      control[[key]], paste0("control$", key),
      minimum, .Machine$integer.max
    )
  }
  invisible(control)
}

validate_mean_tau_ranges <- function(control) {
  assert_bad_argument_ok(
    control$CAP > 0 && control$CAP < 1 &&
      control$SWEEP_STEP > 0 && control$SWEEP_STEP <= control$CAP &&
      control$GRID_TAIL_FRACTION > 0 && control$GRID_TAIL_FRACTION < 1,
    "Invalid mean tau controls",
    arg = "control"
  )
  invisible(control)
}
