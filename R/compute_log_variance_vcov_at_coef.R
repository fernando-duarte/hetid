#' Log-Variance Covariance Matrices at Supplied Coefficients
#'
#' Evaluate the PPML or Harvey covariance formulas without fitting again.
#' Prefer \code{\link{compute_log_variance_vcov}} when a package fit object
#' is available: it also checks the fit's acceptance status and owns its data.
#' This lower-level entrypoint is for coefficients fitted elsewhere, with the
#' matching response and complete design supplied explicitly.
#'
#' @param coef Numeric vector with one entry per design column, on the same
#'   response scale as \code{y}. Names may be absent; when present they must
#'   equal \code{colnames(x_design)} in exactly the same order.
#' @param y Numeric response vector with one entry per design row.
#' @param x_design Numeric matrix with at least one column and unique,
#'   nonmissing, nonempty column names. Supply the complete fitted design,
#'   including its intercept column if used; none is added. Rows must be
#'   aligned with \code{y}. For HAC they must be in chronological order.
#' @param estimator Single estimator name, \code{"ppml"} or \code{"harvey"}.
#' @param hac_lags Nonnegative integer Bartlett lag truncation, at most
#'   \code{.Machine$integer.max}. Lags beyond the sample length contribute no
#'   cross-products; the supplied bandwidth still determines their weights.
#' @param rcond_tol Optional finite positive scalar conditioning tolerance
#'   on diagonally normalized information matrices. \code{NULL} uses the
#'   estimator's package default. This is a covariance gate, not a fitting
#'   control or a change to the estimator's convergence criteria.
#'
#' @return A named list of covariance matrices, labelled on both axes by
#'   \code{colnames(x_design)}. PPML returns \code{naive}, \code{hc0},
#'   \code{hc1}, and \code{hac}; Harvey returns \code{expected},
#'   \code{observed}, \code{opg}, \code{robust}, and \code{hac}.
#'
#' @details
#' This function evaluates formulas at the supplied coefficient; it does not
#' establish convergence, existence, or optimality of that coefficient. The
#' caller owns that evidence and the association between coefficients, data,
#' and estimator. Inference at an estimated first-stage coefficient remains
#' conditional: first-stage uncertainty is not propagated.
#'
#' Malformed types, dimensions, labels, or controls raise structured errors.
#' Numeric but unusable data (nonfinite entries, negative responses, invalid
#' fitted means, or no residual degrees of freedom) return named all-NA
#' matrices. A singular or ill-conditioned information matrix makes only the
#' variants that require it unavailable. Later arithmetic overflow can still
#' produce nonfinite covariance entries; no clamping or refitting is done.
#' Zero responses are allowed. At lag zero, HAC equals HC0 for PPML and the
#' robust sandwich for Harvey. Row order affects HAC; it is not checked here.
#'
#' @seealso \code{\link{compute_log_variance_vcov}},
#'   \code{\link{fit_log_variance}}
#' @export
#' @examples
#' x_design <- cbind("(Intercept)" = 1, v = c(-1, 0, 1, 2))
#' compute_log_variance_vcov_at_coef(c(0.2, -0.1), c(1, 2, 3, 4), x_design)
compute_log_variance_vcov_at_coef <- function(
  coef, y, x_design, estimator = "ppml",
  hac_lags = LOG_VARIANCE_CONTROL$HAC_LAGS, rcond_tol = NULL
) {
  spec <- log_variance_estimator(estimator)
  assert_bad_argument_ok(
    is.matrix(x_design) && is.numeric(x_design) && ncol(x_design) > 0L,
    "x_design must be a numeric matrix with at least one column",
    arg = "x_design"
  )
  coef_labels <- colnames(x_design)
  assert_instrument_names(coef_labels, "x_design")
  assert_bad_argument_ok(
    is.numeric(coef) && is.null(dim(coef)),
    "coef must be a numeric vector",
    arg = "coef"
  )
  assert_bad_argument_ok(
    is.numeric(y) && is.null(dim(y)), "y must be a numeric vector",
    arg = "y"
  )
  assert_dimension_ok(
    length(coef) == ncol(x_design),
    "coef must have one entry per x_design column"
  )
  assert_dimension_ok(
    length(y) == nrow(x_design),
    "y must have one entry per x_design row"
  )
  assert_bad_argument_ok(
    is.null(names(coef)) || identical(names(coef), coef_labels),
    "coef names must equal x_design column names in order",
    arg = "coef"
  )
  assert_scalar_integer_in_range(hac_lags, "hac_lags", 0, .Machine$integer.max)
  if (is.null(rcond_tol)) {
    return(spec$vcov(coef, y, x_design, hac_lags))
  }
  assert_scalar_finite(rcond_tol, "rcond_tol")
  assert_bad_argument_ok(rcond_tol > 0,
    "rcond_tol must be positive",
    arg = "rcond_tol"
  )
  spec$vcov(coef, y, x_design, hac_lags, rcond_tol = rcond_tol)
}
