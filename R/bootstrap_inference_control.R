# Numerical search defaults; eligibility thresholds remain explicit caller choices
BOOTSTRAP_INFERENCE_DEFAULTS <- list(tolerance = 1e-4, max_evals = 10000L)
BOOTSTRAP_ENDPOINT_STATUS <- c("bounded", "unbounded", "unreliable", "failed")

bootstrap_inference_control <- function(control) {
  defaults <- BOOTSTRAP_INFERENCE_DEFAULTS
  assert_bad_argument_ok(
    is.list(control) && (length(control) == 0L ||
      (!is.null(names(control)) && !anyNA(names(control)) &&
        !anyDuplicated(names(control)) && all(names(control) %in% names(defaults)))),
    "control must be a uniquely named list of supported search controls",
    arg = "control"
  )
  for (key in names(control)) defaults[key] <- control[key]
  assert_scalar_finite(defaults$tolerance, "tolerance")
  assert_bad_argument_ok(defaults$tolerance > 0, "tolerance must be positive", arg = "control")
  assert_scalar_integer_in_range(defaults$max_evals, "max_evals", 2, .Machine$integer.max)
  defaults
}

validate_bootstrap_gate <- function(min_reps, stability, alpha = NULL) {
  assert_scalar_integer_in_range(min_reps, "min_reps", 1, .Machine$integer.max)
  assert_scalar_finite(stability, "stability")
  assert_bad_argument_ok(stability >= 0 && stability <= 1,
    "stability must lie between zero and one",
    arg = "stability"
  )
  if (!is.null(alpha)) {
    assert_scalar_finite(alpha, "alpha")
    assert_bad_argument_ok(alpha > 0 && alpha < 1,
      "alpha must lie strictly between zero and one",
      arg = "alpha"
    )
  }
  invisible(TRUE)
}

bootstrap_finite_arithmetic <- function(x, operation) {
  if (any(!is.finite(x))) {
    stop_hetid(paste0(operation, " exceeds the finite numeric range"))
  }
  x
}

bootstrap_is_numeric <- function(x) is.numeric(x) && !is.complex(x)
