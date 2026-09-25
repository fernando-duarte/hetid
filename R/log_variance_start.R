#' Validate One Log-Variance Start Vector
#'
#' Shared gate for \code{start} and each \code{fallback_starts} element: a
#' bare finite numeric vector of the right length, positional when unnamed,
#' exact-order names when named -- a permuted named start would otherwise be
#' silently reinterpreted against a different design.
#'
#' @param val Candidate start vector
#' @param p Required length (\code{ncol(x_mat)})
#' @param labels Design column labels (\code{colnames(x_mat)})
#' @param arg Argument name for the structured error
#' @param skip_nonfinite Whether correctly shaped nonfinite starts may reach
#'   the solver's failed-attempt recovery path
#'
#' @return Invisible TRUE when valid
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
