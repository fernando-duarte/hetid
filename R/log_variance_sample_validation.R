# Validate retained axes and failure masks without refitting the sample.
validate_hetid_log_variance_sample <- function(x) {
  assert_bad_argument_ok(inherits(x, "hetid_log_variance_sample") && is.list(x),
    "object must be a hetid_log_variance_sample",
    arg = "object"
  )
  validate_log_variance_sources(x$box)
  validate_log_variance_sample_design(x)
  validate_log_variance_sample_axes(x)
  coef_matrix <- x$coefficients
  ok <- vapply(x$fits, log_variance_sample_fit_ok, logical(1))
  assert_bad_argument_ok(
    all(is.finite(coef_matrix[ok, , drop = FALSE])) &&
      all(is.na(coef_matrix[!ok, , drop = FALSE])),
    "retained coefficient rows disagree with fit statuses",
    arg = "object"
  )
  validate_log_variance_summary(x, ok)
  log_variance_estimator(x$estimator)
  validate_log_variance_dates(x$dates, nrow(x$x_design))
  assert_bad_argument_ok(
    isTRUE(x$reason %in% c(
      "sampled", "infinite_box", "no_candidates",
      "all_fits_failed"
    )) && identical(x$reason == "sampled", any(ok)),
    "retained sampling reason disagrees with fitted rows",
    arg = "object"
  )
  invisible(x)
}

validate_log_variance_sources <- function(box) {
  validate_hetid_theta_box(box)
  assert_bad_argument_ok(is.numeric(box$w1) && is.null(dim(box$w1)) && is.numeric(box$w2),
    "box residuals must be numeric",
    arg = "box"
  )
  assert_numeric_finite_values(box$w1, "box$w1")
  assert_numeric_finite_values(box$w2, "box$w2")
  assert_instrument_names(colnames(box$w2), "box$w2")
  assert_bad_argument_ok(identical(box$bounds$coef, colnames(box$w2)),
    "box theta labels must match its residual columns",
    arg = "box"
  )
}

validate_log_variance_dates <- function(dates, n) {
  if (is.null(dates)) {
    return(invisible(TRUE))
  }
  assert_bad_argument_ok(
    inherits(dates, "Date") && length(dates) == n &&
      all(is.finite(dates)) && !anyDuplicated(dates),
    "dates must be unique non-missing Date labels, one per row",
    arg = "dates"
  )
  invisible(TRUE)
}

validate_log_variance_sample_design <- function(x) {
  assert_bad_argument_ok(is.matrix(x$x_design) && is.numeric(x$x_design),
    "retained design must be a numeric matrix",
    arg = "object"
  )
  assert_numeric_finite_values(x$x_design, "retained design")
  assert_instrument_names(colnames(x$x_design), "retained design")
  assert_dimension_ok(nrow(x$x_design) == nrow(x$box$w2), "retained design rows disagree")
  assert_bad_argument_ok(colnames(x$x_design)[1] == LOG_VARIANCE_INTERCEPT_LABEL &&
    all(x$x_design[, 1] == 1), "retained design must start with an intercept", arg = "object")
  invisible(TRUE)
}

log_variance_sample_fit_ok <- function(fit) {
  assert_bad_argument_ok(
    is.list(fit) && isTRUE(fit$fit_status %in% LOG_VARIANCE_FIT_STATUS) &&
      (isTRUE(fit$converged) || isFALSE(fit$converged)),
    "retained fit status is malformed",
    arg = "object"
  )
  success <- identical(fit$fit_status, "ok")
  assert_bad_argument_ok(identical(success, fit$converged),
    "retained fit status and convergence disagree",
    arg = "object"
  )
  success
}

validate_log_variance_sample_axes <- function(x) {
  candidates <- x$candidates
  coef_matrix <- x$coefficients
  assert_bad_argument_ok(
    is.matrix(candidates) && is.numeric(candidates) &&
      is.matrix(coef_matrix) && is.numeric(coef_matrix),
    "retained candidates and coefficients must be numeric matrices",
    arg = "object"
  )
  assert_numeric_finite_values(candidates, "retained candidates")
  n <- nrow(candidates)
  assert_dimension_ok(
    ncol(candidates) == ncol(x$box$w2) &&
      nrow(coef_matrix) == n && ncol(coef_matrix) == ncol(x$x_design),
    "retained candidate and coefficient dimensions disagree"
  )
  assert_bad_argument_ok(
    identical(colnames(candidates), colnames(x$box$w2)) &&
      identical(colnames(coef_matrix), colnames(x$x_design)) &&
      identical(rownames(candidates), rownames(coef_matrix)),
    "retained candidate or coefficient axes disagree",
    arg = "object"
  )
  assert_bad_argument_ok(
    is.list(x$fits) && length(x$fits) == n &&
      identical(names(x$fits), rownames(candidates)),
    "retained fit records must match candidate rows",
    arg = "object"
  )
  invisible(TRUE)
}

validate_log_variance_summary <- function(x, ok) {
  fits <- list(
    coefs = if (any(ok)) x$coefficients[ok, , drop = FALSE] else NULL,
    n_failed = sum(!ok)
  )
  expected <- log_variance_profile_bounds(
    fits, nrow(x$candidates), colnames(x$coefficients), x$estimator
  )
  assert_bad_argument_ok(isTRUE(all.equal(x$bounds, expected, tolerance = 0)),
    "retained profile summary disagrees with fit records",
    arg = "object"
  )
  invisible(TRUE)
}
