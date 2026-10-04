# Validate the Fields Required When fit_status Is nonconvergence
# @param x A classed \code{hetid_log_variance_fit} object.
# @return Invisible \code{TRUE}; otherwise signals a \code{hetid_error_bad_argument}
#   condition naming the first inconsistent field.
validate_log_variance_fit_nonconv <- function(x) {
  assert_bad_argument_ok(
    isFALSE(x$converged), "converged must be FALSE when fit_status is nonconvergence",
    arg = "converged"
  )
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
