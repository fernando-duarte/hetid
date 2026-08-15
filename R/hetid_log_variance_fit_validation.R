#' Shape Validation for hetid_log_variance_fit Objects
#'
#' Internal helpers behind \code{validate_hetid_log_variance_fit()}: the
#' data-shape sweep (\code{y}, \code{x_design} against \code{n_obs} and
#' \code{coef_labels}), the \code{coef}/\code{warm_start} name checks, the
#' \code{diagnostics} shape check, and the success/failure cross-field
#' consistency sweep.
#'
#' @name hetid_log_variance_fit_validation
#' @keywords internal
NULL

#' Validate a hetid_log_variance_fit Object
#'
#' Full structural-alignment gate for the \code{hetid_log_variance_fit}
#' class, checked against the object's own attributes. Run by the public
#' boundary \code{fit_log_variance()} on every object it returns; call it
#' directly on containers assembled via \code{new_hetid_log_variance_fit()}
#' from parts that are not known-good.
#'
#' @param x A classed \code{hetid_log_variance_fit} object
#' @return \code{x}, invisibly
#' @keywords internal
validate_hetid_log_variance_fit <- function(x) {
  assert_hetid_log_variance_fit(x, arg = "x")
  n_obs <- attr(x, "n_obs")
  assert_scalar_integer_in_range(n_obs, "n_obs", 1, .Machine$integer.max)
  response_scale <- attr(x, "response_scale")
  assert_scalar_finite(response_scale, "response_scale")
  assert_bad_argument_ok(
    response_scale > 0, "response_scale must be positive",
    arg = "response_scale"
  )
  coef_labels <- attr(x, "coef_labels")
  assert_bad_argument_ok(
    is.character(coef_labels) && length(coef_labels) >= 1 &&
      !anyNA(coef_labels),
    "coef_labels must be a non-empty character vector",
    arg = "coef_labels"
  )
  assert_bad_argument_ok(
    isTRUE(x$fit_status %in% LOG_VARIANCE_FIT_STATUS),
    paste0(
      "fit_status must be one of: ",
      paste(LOG_VARIANCE_FIT_STATUS, collapse = ", ")
    ),
    arg = "fit_status"
  )
  validate_log_variance_fit_data(x, n_obs, coef_labels)
  validate_log_variance_fit_names(x, coef_labels)
  validate_log_variance_fit_diag(x)
  if (identical(x$fit_status, LOG_VARIANCE_FIT_STATUS[["ok"]])) {
    validate_log_variance_fit_ok(x)
  } else {
    validate_log_variance_fit_nonconv(x)
  }
  invisible(x)
}

#' Validate y and x_design Against n_obs and coef_labels
#'
#' @noRd
validate_log_variance_fit_data <- function(x, n_obs, coef_labels) {
  assert_bad_argument_ok(
    is.numeric(x$y) && is.null(dim(x$y)), "y must be a numeric vector",
    arg = "y"
  )
  assert_dimension_ok(length(x$y) == n_obs, "y must have length n_obs")
  assert_numeric_finite_values(x$y, "y")
  assert_bad_argument_ok(all(x$y >= 0), "y must be nonnegative", arg = "y")

  assert_bad_argument_ok(
    is.matrix(x$x_design) && is.numeric(x$x_design),
    "x_design must be a numeric matrix",
    arg = "x_design"
  )
  assert_dimension_ok(
    nrow(x$x_design) == n_obs, "x_design must have n_obs rows"
  )
  assert_numeric_finite_values(x$x_design, "x_design")
  assert_dimension_ok(
    ncol(x$x_design) == length(coef_labels),
    "x_design must have length(coef_labels) columns"
  )
  assert_bad_argument_ok(
    identical(colnames(x$x_design), coef_labels),
    "colnames(x_design) must equal coef_labels",
    arg = "x_design"
  )
  invisible(TRUE)
}

#' Validate coef and warm_start Names Against coef_labels
#'
#' Checked only when present, matching the fail-closed contract where a
#' failed fit carries \code{NULL} for either.
#'
#' @noRd
validate_log_variance_fit_names <- function(x, coef_labels) {
  for (field in c("coef", "warm_start")) {
    val <- x[[field]]
    if (is.null(val)) next
    assert_bad_argument_ok(
      is.numeric(val) && is.null(dim(val)),
      paste0(field, " must be a numeric vector or NULL"),
      arg = field
    )
    assert_bad_argument_ok(
      identical(names(val), coef_labels),
      paste0("names(", field, ") must equal coef_labels"),
      arg = field
    )
  }
  invisible(TRUE)
}

#' Validate the diagnostics List Shape
#'
#' @noRd
validate_log_variance_fit_diag <- function(x) {
  diagnostics <- x$diagnostics
  assert_bad_argument_ok(
    is.list(diagnostics), "diagnostics must be a list",
    arg = "diagnostics"
  )
  assert_bad_argument_ok(
    all(c("error_class", "start_attempts") %in% names(diagnostics)),
    "diagnostics must contain error_class and start_attempts",
    arg = "diagnostics"
  )
  invisible(TRUE)
}

#' Validate the Fields Required When fit_status Is ok
#'
#' @noRd
validate_log_variance_fit_ok <- function(x) {
  assert_bad_argument_ok(
    isTRUE(x$converged), "converged must be TRUE when fit_status is ok",
    arg = "converged"
  )
  for (field in c("coef", "warm_start")) {
    assert_bad_argument_ok(
      !is.null(x[[field]]) && all(is.finite(x[[field]])),
      paste0(field, " must be non-NULL and finite when fit_status is ok"),
      arg = field
    )
  }
  assert_scalar_finite(x$objective, "objective")
  assert_scalar_finite(x$score_norm, "score_norm")
  assert_bad_argument_ok(
    is.numeric(x$convergence_code) && length(x$convergence_code) == 1 &&
      is.finite(x$convergence_code) && x$convergence_code %% 1 == 0 &&
      x$convergence_code >= 0,
    "convergence_code must be a non-negative integer when fit_status is ok",
    arg = "convergence_code"
  )
  invisible(TRUE)
}

#' Validate the Fields Required When fit_status Is nonconvergence
#'
#' @noRd
validate_log_variance_fit_nonconv <- function(x) {
  for (field in c("coef", "warm_start")) {
    assert_bad_argument_ok(
      is.null(x[[field]]),
      paste0(field, " must be NULL when fit_status is nonconvergence"),
      arg = field
    )
  }
  for (field in c("objective", "score_norm")) {
    assert_bad_argument_ok(
      isTRUE(length(x[[field]]) == 1 && is.na(x[[field]])),
      paste0(field, " must be NA when fit_status is nonconvergence"),
      arg = field
    )
  }
  assert_bad_argument_ok(
    isTRUE(x$convergence_code == -1),
    "convergence_code must be -1 when fit_status is nonconvergence",
    arg = "convergence_code"
  )
  err <- x$diagnostics$error_class
  assert_bad_argument_ok(
    isTRUE(!is.null(err) && !is.na(err)),
    paste0(
      "diagnostics$error_class must be non-missing when fit_status is ",
      "nonconvergence"
    ),
    arg = "diagnostics"
  )
  invisible(TRUE)
}
