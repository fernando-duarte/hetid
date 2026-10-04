#' Controls for Mean-Set Tau Brackets
#'
#' @format A named list. The sweep cap is 0.99 and step is 0.005, followed by at
#'   most 40 bisections.
#' @export
MEAN_TAU_CONTROL <- list(cap = 0.99, sweep_step = 0.005, bisection_iterations = 40L)

validate_mean_tau_control <- function(control) {
  assert_bad_argument_ok(
    is.list(control) && !anyDuplicated(names(control)) &&
      all(names(MEAN_TAU_CONTROL) %in% names(control)),
    "control lacks mean tau settings",
    arg = "control"
  )
  for (key in c("cap", "sweep_step")) {
    assert_scalar_finite(control[[key]], paste0("control$", key))
  }
  validate_mean_tau_ranges(control)
  assert_scalar_integer_in_range(
    control$bisection_iterations, "control$bisection_iterations",
    1, .Machine$integer.max
  )
  invisible(control)
}

validate_mean_tau_ranges <- function(control) {
  assert_bad_argument_ok(
    control$cap > 0 && control$cap < 1 &&
      control$sweep_step > 0 && control$sweep_step <= control$cap,
    "Invalid mean tau controls",
    arg = "control"
  )
  invisible(control)
}
