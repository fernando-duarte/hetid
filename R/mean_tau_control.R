#' Controls for Mean-Set Tau Brackets
#'
#' @format A named list. The sweep cap is 0.99 and step is 0.005, followed by at
#'   most 40 bisections.
#' @export
MEAN_TAU_CONTROL <- list(CAP = 0.99, SWEEP_STEP = 0.005, BISECTION_ITERATIONS = 40L)

validate_mean_tau_control <- function(control) {
  assert_bad_argument_ok(
    is.list(control) && !anyDuplicated(names(control)) &&
      all(names(MEAN_TAU_CONTROL) %in% names(control)),
    "control lacks mean tau settings",
    arg = "control"
  )
  for (key in c("CAP", "SWEEP_STEP")) {
    assert_scalar_finite(control[[key]], paste0("control$", key))
  }
  validate_mean_tau_ranges(control)
  assert_scalar_integer_in_range(
    control$BISECTION_ITERATIONS, "control$BISECTION_ITERATIONS",
    1, .Machine$integer.max
  )
  invisible(control)
}

validate_mean_tau_ranges <- function(control) {
  assert_bad_argument_ok(
    control$CAP > 0 && control$CAP < 1 &&
      control$SWEEP_STEP > 0 && control$SWEEP_STEP <= control$CAP,
    "Invalid mean tau controls",
    arg = "control"
  )
  invisible(control)
}
