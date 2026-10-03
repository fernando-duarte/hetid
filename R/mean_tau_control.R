#' Controls for Mean-Set Tau Brackets and Grids
#'
#' @format A named list. The sweep cap is 0.99 and step is 0.005, followed by at
#'   most 40 bisections. Grids use a 25-point backbone and subdivide intervals
#'   in the last 10 percent into four parts, preserving the backbone maximum.
#' @export
MEAN_TAU_CONTROL <- list(
  cap = 0.99, sweep_step = 0.005, bisection_iterations = 40L,
  grid_backbone = 25L, grid_tail_fraction = 0.9, grid_tail_subdivisions = 4L
)

validate_mean_tau_control <- function(control) {
  assert_bad_argument_ok(
    is.list(control) && !anyDuplicated(names(control)) &&
      all(names(MEAN_TAU_CONTROL) %in% names(control)),
    "control lacks mean tau settings",
    arg = "control"
  )
  for (key in c("cap", "sweep_step", "grid_tail_fraction")) {
    assert_scalar_finite(control[[key]], paste0("control$", key))
  }
  validate_mean_tau_ranges(control)
  for (key in c("bisection_iterations", "grid_backbone", "grid_tail_subdivisions")) {
    minimum <- switch(key,
      bisection_iterations = 1,
      grid_backbone = 3,
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
    control$cap > 0 && control$cap < 1 &&
      control$sweep_step > 0 && control$sweep_step <= control$cap &&
      control$grid_tail_fraction > 0 && control$grid_tail_fraction < 1,
    "Invalid mean tau controls",
    arg = "control"
  )
  invisible(control)
}
