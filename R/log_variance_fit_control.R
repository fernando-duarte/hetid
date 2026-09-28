# Fitting-only controls; scientific sampling and recession policy stay with callers
log_variance_fit_defaults <- function(estimator) {
  controls <- list(
    ppml = c(LOG_VARIANCE_CONTROL[c(
      "GLM_EPSILON", "GLM_MAXIT", "SCORE_TOLERANCE", "RANK_TOLERANCE",
      "RCOND_TOLERANCE"
    )], list(START_ORDER = c("supplied", "fallback", "intercept_only", "glm_default"))),
    harvey = c(LOG_VARIANCE_HARVEY_CONTROL[setdiff(
      names(LOG_VARIANCE_HARVEY_CONTROL), "SE_TYPES"
    )], list(AUTO_INTERCEPT = TRUE))
  )
  assert_bad_argument_ok(estimator %in% names(controls), "Unknown estimator", arg = "estimator")
  defaults <- controls[[estimator]]
  defaults$SKIP_NONFINITE_STARTS <- FALSE
  defaults
}

log_variance_control_scalar_ok <- function(value, type) {
  isTRUE(type(value) && is.null(dim(value)) &&
    length(value) == 1L && is.finite(value))
}

log_variance_control_number_ok <- function(value, key) {
  if (!log_variance_control_scalar_ok(value, is.numeric)) {
    return(FALSE)
  }
  if (value < 0 || (value == 0 && key != "LINE_SEARCH_HALVINGS")) {
    return(FALSE)
  }
  if (key %in% c("GLM_MAXIT", "MAXIT", "LINE_SEARCH_HALVINGS")) {
    return(value == floor(value) && value <= .Machine$integer.max)
  }
  TRUE
}

log_variance_control_order_ok <- function(value) {
  if (!is.character(value) || !is.null(dim(value)) || length(value) != 4L) {
    return(FALSE)
  }
  !anyNA(value) && !anyDuplicated(value) &&
    setequal(value, c("supplied", "fallback", "intercept_only", "glm_default"))
}

log_variance_control_value_ok <- function(value, key) {
  if (key %in% c("AUTO_INTERCEPT", "SKIP_NONFINITE_STARTS")) {
    return(log_variance_control_scalar_ok(value, is.logical))
  }
  if (key == "START_ORDER") {
    return(log_variance_control_order_ok(value))
  }
  log_variance_control_number_ok(value, key)
}

log_variance_fit_control <- function(estimator, control = list()) {
  defaults <- log_variance_fit_defaults(estimator)
  assert_bad_argument_ok(
    is.list(control) && (length(control) == 0L ||
      (!is.null(names(control)) && !anyNA(names(control)) &&
        !anyDuplicated(names(control)) && all(names(control) %in% names(defaults)))),
    "control must be a uniquely named list of supported fitting controls",
    arg = "control"
  )
  for (key in names(control)) {
    assert_bad_argument_ok(log_variance_control_value_ok(control[[key]], key),
      paste0("Invalid fitting control: ", key),
      arg = "control"
    )
    defaults[key] <- control[key]
  }
  defaults
}
