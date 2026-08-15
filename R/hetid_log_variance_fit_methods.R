#' Methods and Assertions for hetid_log_variance_fit Objects
#'
#' @name hetid_log_variance_fit_methods
#' @keywords internal
NULL

#' Assert a Valid hetid_log_variance_fit Object
#'
#' @param x Object to check
#' @param arg Argument name for the structured error
#'
#' @return Invisible TRUE when valid
#' @keywords internal
assert_hetid_log_variance_fit <- function(x, arg = "fit") {
  assert_bad_argument_ok(
    inherits(x, "hetid_log_variance_fit"),
    paste0(
      arg, " must be a hetid_log_variance_fit object created by ",
      "new_hetid_log_variance_fit()"
    ),
    arg = arg
  )
  invisible(TRUE)
}

#' Check Whether a Log-Variance Fit Is Usable for Inference
#'
#' Ports \code{logvar_fit_ok()} from the paper's PPML engine
#' (\code{scripts-paper/log_variance/engine/contracts.R}) exactly: a fit is
#' usable only when it reports success, the underlying solver converged, and
#' the recovered coefficients are present and all finite. This is
#' deliberately a raw predicate, not a validator -- it does not require
#' \code{x} to have passed \code{validate_hetid_log_variance_fit()}, so
#' callers can probe an in-progress or hand-built fit list directly.
#'
#' @param fit A \code{hetid_log_variance_fit} object, or any list with
#'   \code{fit_status}, \code{converged}, and \code{coef} elements
#'
#' @return \code{TRUE} when the fit is ok, converged, and has finite
#'   coefficients; \code{FALSE} otherwise
#' @keywords internal
log_variance_fit_ok <- function(fit) {
  is.list(fit) &&
    identical(fit$fit_status, LOG_VARIANCE_FIT_STATUS[["ok"]]) &&
    isTRUE(fit$converged) &&
    !is.null(fit$coef) &&
    all(is.finite(fit$coef))
}

#' Print a hetid_log_variance_fit Object
#'
#' @param x A \code{hetid_log_variance_fit} object
#' @param ... Unused, for method consistency
#'
#' @return \code{x}, invisibly
#' @export
#'
#' @examples
#' set.seed(1)
#' t_obs <- 80
#' x <- cbind(v1 = rnorm(t_obs), v2 = rnorm(t_obs))
#' eta <- drop(cbind(1, x) %*% c(-0.5, 0.6, -0.4))
#' y <- exp(eta) * rchisq(t_obs, df = 1)
#' fit <- fit_log_variance(y, x)
#' print(fit)
print.hetid_log_variance_fit <- function(x, ...) {
  cat("<hetid_log_variance_fit>\n")
  cat("  estimator: ", attr(x, "estimator"), "\n", sep = "")
  cat("  fit_status: ", x$fit_status, "\n", sep = "")
  cat("  n_obs: ", attr(x, "n_obs"), "\n", sep = "")
  invisible(x)
}
