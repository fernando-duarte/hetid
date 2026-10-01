#' Validate One Log-Variance Start Vector
#'
#' Checks the length and optional names of a numeric \code{start} vector or
#' \code{fallback_starts} element. Values must be finite unless
#' \code{skip_nonfinite = TRUE}. Unnamed starts are positional; named starts
#' must match the design labels in order to avoid changing their interpretation.
#'
#' @param val Numeric vector of length \code{p}, with no dimensions.
#' @param p Required vector length, given by \code{ncol(x_mat)}.
#' @param labels Character vector of design column labels, given by
#'   \code{colnames(x_mat)}.
#' @param arg Character string identifying the argument in a structured error.
#' @param skip_nonfinite Logical scalar. If \code{TRUE}, allows correctly
#'   shaped starts containing \code{NA}, \code{NaN}, or infinite values to reach
#'   the solver's failed-attempt recovery path. Defaults to \code{FALSE}.
#'
#' @return Invisible \code{TRUE} when valid. Otherwise signals a
#'   \code{hetid_error_bad_argument} condition with the \code{arg} field.
#' @noRd
assert_log_variance_start <- function(val, p, labels, arg, skip_nonfinite = FALSE) {
  assert_bad_argument_ok(
    is.numeric(val) && is.null(dim(val)) && length(val) == p &&
      (skip_nonfinite || all(is.finite(val))),
    paste0(arg, " must be a finite numeric vector of length ", p),
    arg = arg
  )
  nm <- names(val)
  if (!is.null(nm)) {
    assert_bad_argument_ok(
      identical(nm, labels),
      paste0(arg, " names, when supplied, must equal the design labels exactly"),
      arg = arg
    )
  }
  invisible(TRUE)
}
