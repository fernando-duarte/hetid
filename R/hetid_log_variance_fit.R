#' Construct hetid_log_variance_fit Objects
#'
#' Container for accepted and failed log-variance fits, built by
#' \code{\link{fit_log_variance}}. Internal callers can assemble one with
#' \code{\link{new_hetid_log_variance_fit}} and check its structure with
#' \code{\link{validate_hetid_log_variance_fit}}.
#'
#' @details
#' The list contains \code{coef}, \code{fit_status}, \code{converged},
#' \code{objective}, \code{score_norm}, \code{convergence_code},
#' \code{warm_start}, \code{diagnostics}, \code{y}, and \code{x_design}.
#' Attributes record \code{estimator}, \code{response_scale}, \code{n_obs},
#' and \code{coef_labels}. See \code{\link{new_hetid_log_variance_fit}} for
#' the field definitions.
#'
#' Accepted fits have \code{fit_status = "ok"}, finite coefficients,
#' objectives and score norms, and \code{converged = TRUE}. Failed fits have
#' \code{fit_status = "nonconvergence"}, \code{converged = FALSE},
#' \code{NULL} coefficients and warm starts, \code{NA_real_} objectives and
#' score norms, and \code{convergence_code = -1L}. The failure reason is
#' stored in \code{diagnostics$error_class}; the response and design are
#' retained in both cases.
#'
#' @name hetid_log_variance_fit
#' @keywords internal
NULL

LOG_VARIANCE_FIT_STATUS <- c(ok = "ok", nonconvergence = "nonconvergence")

#' Construct a hetid_log_variance_fit Object
#'
#' Low-level constructor for the \code{hetid_log_variance_fit} class.
#' Checks \code{n_obs}, \code{response_scale}, \code{coef_labels}, and
#' \code{estimator}, but stores the fit fields without validating them.
#'
#' @details
#' The parameter descriptions state the contract for validated fits.
#' Call \code{\link{validate_hetid_log_variance_fit}} when assembling a
#' container from parts that are not known to satisfy that contract.
#' Results from \code{\link{fit_log_variance}} undergo this validation.
#' Invalid container-identity attributes signal a
#' \code{hetid_error_bad_argument} condition; missing values are rejected
#' in these attributes rather than removed.
#'
#' @param coef Named numeric vector of original-scale coefficients, or
#'   \code{NULL} on failure. Names must equal \code{coef_labels}.
#' @param fit_status One of \code{LOG_VARIANCE_FIT_STATUS}: \code{"ok"} or
#'   \code{"nonconvergence"}.
#' @param converged Logical scalar indicating whether the solver converged.
#' @param objective The stored criterion on the scaled response, or
#'   \code{NA_real_} on failure. With \code{y_scaled = y / response_scale},
#'   \code{eta = x_design \%*\% warm_start}, and \code{mu = exp(eta)}, PPML
#'   stores \code{sum(mu) - sum(y_scaled[pos] * log(mu[pos]))}, where
#'   \code{pos = y_scaled > 0}. This is a quasi-likelihood criterion up to
#'   response-only constants, not a full deviance. Harvey stores the Gaussian
#'   negative log-likelihood \code{0.5 * sum(eta + y_scaled * exp(-eta))}.
#' @param score_norm Numeric scalar score-norm diagnostic on the scaled
#'   response, or \code{NA_real_} on failure.
#' @param convergence_code Integer scalar counting solver iterations on
#'   success, or \code{-1L} on failure.
#' @param warm_start Named numeric vector of coefficients for the scaled
#'   response \code{y / response_scale}, or \code{NULL} on failure. Names
#'   must equal \code{coef_labels}. On success, \code{coef} differs only by
#'   adding \code{log(response_scale)} to its intercept coefficient.
#' @param diagnostics List with at least \code{error_class} and
#'   \code{start_attempts} entries.
#' @param y Finite nonnegative numeric vector of length \code{n_obs}, the
#'   original-scale response the fit ran on.
#' @param x_design Finite numeric matrix with \code{n_obs} rows and
#'   \code{length(coef_labels)} columns, the full design matrix including
#'   the intercept. Column names must equal \code{coef_labels}.
#' @param estimator Single non-missing string identifying the estimator.
#'   Public fits use \code{"ppml"} or \code{"harvey"}; this constructor
#'   checks the string's type and length, not membership in the registry.
#' @param response_scale Positive finite numeric scalar, the response
#'   divisor applied before fitting: the solver uses \code{y / response_scale}.
#' @param n_obs Positive finite integer-valued numeric scalar no greater
#'   than \code{.Machine$integer.max}, the number of observations fitted.
#' @param coef_labels Non-empty character vector without missing values,
#'   naming the coefficient axis.
#' @return A \code{hetid_log_variance_fit} list, visibly, with the ten fit
#'   fields stored as supplied and the four container-identity attributes.
#'   The \code{n_obs} attribute is converted to integer. The result has not
#'   undergone full structural validation; see \code{\link{hetid_log_variance_fit}}.
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

#' Assemble a Failed Log-Variance Fit
#'
#' Shares the nonconvergence fields across estimators while retaining each
#' estimator's diagnostics and the original response and design.
#'
#' @inheritParams harvey_failure
#' @param estimator Single string identifying the estimator.
#' @param diagnostics Estimator-specific list of failure diagnostics.
#' @return A validated \code{hetid_log_variance_fit} object, visibly.
#' @noRd
log_variance_failure_fit <- function(y, x_mat, response_scale, estimator,
                                     diagnostics) {
  out <- validate_hetid_log_variance_fit(new_hetid_log_variance_fit(
    coef = NULL, fit_status = LOG_VARIANCE_FIT_STATUS[["nonconvergence"]],
    converged = FALSE, objective = NA_real_, score_norm = NA_real_,
    convergence_code = -1L, warm_start = NULL,
    diagnostics = diagnostics,
    y = y, x_design = x_mat, estimator = estimator,
    response_scale = response_scale, n_obs = length(y),
    coef_labels = colnames(x_mat)
  ))
  out
}
