#' Methods and Assertions for hetid_log_variance_fit Objects
#'
#' Internal class and inference checks, and the print method for
#' \code{hetid_log_variance_fit} objects.
#'
#' @name hetid_log_variance_fit_methods
#' @keywords internal
NULL

#' Assert the hetid_log_variance_fit Class
#'
#' Checks class inheritance only.
#'
#' @param x Object to check.
#' @param arg Character string naming the argument in the structured error.
#'   Defaults to \code{"fit"}.
#'
#' @return \code{TRUE}, invisibly, when \code{x} inherits from
#'   \code{hetid_log_variance_fit}. Otherwise signals a
#'   \code{hetid_error_bad_argument} condition carrying \code{arg}.
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
#' Checks whether a fit reports success, the underlying solver converged,
#' and the recovered coefficients are present and all finite. This is
#' deliberately a raw predicate, not a validator -- it does not require
#' \code{fit} to be structurally valid, so
#' callers can probe an in-progress or hand-built fit list directly.
#'
#' This checks \code{fit_status} and \code{converged}, not an evaluator's
#' \code{status}. A successful report does not establish existence of a
#' minimizer, endpoint reliability, or validity of inference.
#'
#' No coefficient length or shape is checked: an empty numeric vector
#' passes the finite-value check. Malformed coefficient objects for which
#' \code{is.finite()} is undefined can raise an error.
#'
#' @param fit A \code{hetid_log_variance_fit} object, or any list with
#'   \code{fit_status}, \code{converged}, and \code{coef} elements. For a
#'   well-formed fit these are a status string, a logical scalar, and a
#'   numeric coefficient vector or \code{NULL}, respectively.
#'
#' @return A logical scalar: \code{TRUE} when \code{fit} is a list with
#'   \code{fit_status = "ok"}, \code{converged = TRUE}, and non-\code{NULL}
#'   coefficients that are all finite; \code{FALSE} when any check fails.
#'   Missing or nonfinite coefficient values fail the finite-value check.
#' @export
#' @examples
#' log_variance_fit_ok(list(fit_status = "ok", converged = TRUE, coef = c(0.2, -0.1)))
#' log_variance_fit_ok(list(fit_status = "nonconvergence", converged = FALSE, coef = NULL))
log_variance_fit_ok <- function(fit) {
  is.list(fit) &&
    identical(fit$fit_status, LOG_VARIANCE_FIT_STATUS[["ok"]]) &&
    isTRUE(fit$converged) &&
    !is.null(fit$coef) &&
    all(is.finite(fit$coef))
}

#' Print a hetid_log_variance_fit Object
#'
#' Prints the estimator, fit status, and observation count to the console.
#'
#' @param x A \code{hetid_log_variance_fit} object.
#' @param ... Unused arguments accepted for method consistency.
#'
#' @return \code{x}, invisibly.
#' @seealso \code{\link[base:print]{print}}
#' @export
#'
#' @examples
#' local({
#'   old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
#'     get(".Random.seed", envir = .GlobalEnv)
#'   } else {
#'     NULL
#'   }
#'   on.exit({
#'     if (is.null(old_seed)) {
#'       rm(".Random.seed", envir = .GlobalEnv)
#'     } else {
#'       assign(".Random.seed", old_seed, envir = .GlobalEnv)
#'     }
#'   })
#'   set.seed(1)
#'   t_obs <- 80
#'   x <- cbind(v1 = rnorm(t_obs), v2 = rnorm(t_obs))
#'   eta <- drop(cbind(1, x) %*% c(-0.5, 0.6, -0.4))
#'   y <- exp(eta) * rchisq(t_obs, df = 1)
#'   fit <- fit_log_variance(y, x)
#'   print(fit)
#' })
print.hetid_log_variance_fit <- function(x, ...) {
  cat("<hetid_log_variance_fit>\n")
  cat("  estimator: ", attr(x, "estimator"), "\n", sep = "")
  cat("  fit_status: ", x$fit_status, "\n", sep = "")
  cat("  n_obs: ", attr(x, "n_obs"), "\n", sep = "")
  invisible(x)
}
