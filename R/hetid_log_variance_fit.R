#' Construct hetid_log_variance_fit Objects
#'
#' Internal helpers behind the \code{hetid_log_variance_fit} container: the
#' cheap \code{new_hetid_log_variance_fit()} constructor. The full shape
#' sweep lives in \code{validate_hetid_log_variance_fit()}
#' (\code{R/hetid_log_variance_fit_validation.R}). The public boundary that
#' builds these objects is \code{fit_log_variance()}.
#'
#' @name hetid_log_variance_fit
#' @keywords internal
NULL

# fit_status vocabulary for hetid_log_variance_fit. The paper's engine-only
# statuses (nonexistence, domain_failure, nonfinite_fitted_log_variance) are
# deliberately dropped here; adding one later is non-breaking.
LOG_VARIANCE_FIT_STATUS <- c(ok = "ok", nonconvergence = "nonconvergence")

#' Construct a hetid_log_variance_fit Object
#'
#' Low-level cheap constructor for the \code{hetid_log_variance_fit} class:
#' checks the four container-identity attributes, trusting the shapes of the
#' fit fields themselves. The full structural sweep lives in
#' \code{validate_hetid_log_variance_fit()}, which the public boundary
#' \code{fit_log_variance()} always runs; hot paths rebuilding containers
#' from known-good parts may call this constructor directly and skip it.
#'
#' @param coef Named numeric vector of original-scale coefficients, or
#'   \code{NULL} on failure
#' @param fit_status One of \code{LOG_VARIANCE_FIT_STATUS}: \code{"ok"} or
#'   \code{"nonconvergence"}
#' @param converged Logical, whether the underlying solver converged
#' @param objective The stored Poisson criterion
#'   \code{sum(mu) - sum(y * log(mu))} over positive-y terms, on the scaled
#'   response (a quasi-likelihood criterion up to response-only constants,
#'   not a full deviance), or \code{NA} on failure
#' @param score_norm Numeric score-norm diagnostic, or \code{NA} on failure
#' @param convergence_code Integer, IRLS iterations on success or \code{-1L}
#'   on failure
#' @param warm_start Named numeric vector of scaled-scale coefficients, or
#'   \code{NULL} on failure
#' @param diagnostics List with at least \code{error_class} and
#'   \code{start_attempts}
#' @param y Numeric vector, the original-scale response the fit ran on
#' @param x_design Numeric matrix, the full design matrix the fit ran on
#'   (intercept column included), with \code{colnames == coef_labels}
#' @param estimator Single string identifying the estimator (\code{"ppml"})
#' @param response_scale Positive finite numeric scalar, the response
#'   scaling factor applied before fitting
#' @param n_obs Number of observations the fit was computed from
#' @param coef_labels Character vector naming the coefficient axis
#' @return A classed \code{hetid_log_variance_fit} list
#' @keywords internal
new_hetid_log_variance_fit <- function(coef, fit_status, converged, objective,
                                       score_norm, convergence_code,
                                       warm_start, diagnostics, y, x_design,
                                       estimator, response_scale, n_obs,
                                       coef_labels) {
  assert_scalar_integer_in_range(n_obs, "n_obs", 1, .Machine$integer.max)
  assert_scalar_finite(response_scale, "response_scale")
  assert_bad_argument_ok(
    response_scale > 0, "response_scale must be positive",
    arg = "response_scale"
  )
  assert_bad_argument_ok(
    is.character(coef_labels) && length(coef_labels) >= 1 &&
      !anyNA(coef_labels),
    "coef_labels must be a non-empty character vector",
    arg = "coef_labels"
  )
  assert_bad_argument_ok(
    is.character(estimator) && length(estimator) == 1 && !is.na(estimator),
    "estimator must be a single non-NA string",
    arg = "estimator"
  )

  structure(
    list(
      coef = coef, fit_status = fit_status, converged = converged,
      objective = objective, score_norm = score_norm,
      convergence_code = convergence_code, warm_start = warm_start,
      diagnostics = diagnostics, y = y, x_design = x_design
    ),
    estimator = estimator,
    response_scale = response_scale,
    n_obs = as.integer(n_obs),
    coef_labels = coef_labels,
    class = "hetid_log_variance_fit"
  )
}
