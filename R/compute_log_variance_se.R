#' Standard-Error Frame for a Log-Variance Fit
#'
#' Thin assembly over \code{\link{compute_log_variance_vcov}}: turns its named
#' list of covariance matrices into one \code{data.frame} with a row per
#' coefficient. The SE column names come from the vcov list itself, which
#' \code{compute_log_variance_vcov} always keys by the estimator's registry
#' spec (\code{\link{log_variance_estimator}}'s \code{se_types}) -- so a
#' future estimator with different variant names needs no change here.
#'
#' @inheritParams compute_log_variance_vcov
#'
#' @return A \code{data.frame} with \code{length(coef_labels)} rows and
#'   default integer row names: a \code{term} column (the fit's
#'   \code{coef_labels}), a \code{coef} column (\code{fit$coef}, or
#'   \code{NA_real_} on a failed fit), and one numeric column per SE type
#'   named after it. Each SE is \code{sqrt(diag(.))} of the matching
#'   covariance matrix, with a negative or non-finite diagonal entry
#'   rendered \code{NA} rather than clamped to zero.
#'
#' @details
#' Malformed \emph{arguments} are errors, exactly as in
#' \code{\link{compute_log_variance_vcov}}: a first argument that is not a
#' \code{hetid_log_variance_fit}, or an \code{hac_lags} that is not a scalar
#' nonnegative integer, signals \code{hetid_error_bad_argument}. A failed fit
#' is not an argument error -- it renders the fully-\code{NA} frame instead
#' (\code{term} intact, every other column \code{NA_real_}), matching
#' \code{compute_log_variance_vcov}'s fail-closed contract.
#'
#' @section Inference caveats:
#' Same as \code{\link{compute_log_variance_vcov}}: these are
#' \strong{conditional second-stage} standard errors at a fixed plug-in
#' \code{b}, and the \code{hac} column assumes \code{fit$y}/\code{fit$x_design}
#' rows are in chronological order.
#'
#' @seealso \code{\link{compute_log_variance_vcov}} for the underlying
#'   covariance matrices, \code{\link{fit_log_variance}},
#'   \code{\link{fit_log_variance_at_b}}
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' t_obs <- 200
#' x <- cbind(v1 = rnorm(t_obs), v2 = rnorm(t_obs))
#' eta <- drop(cbind(1, x) %*% c(-0.5, 0.6, -0.4))
#' y <- exp(eta) * rchisq(t_obs, df = 1)
#' fit <- fit_log_variance(y, x)
#' compute_log_variance_se(fit)
compute_log_variance_se <- function(fit,
                                    hac_lags = LOG_VARIANCE_CONTROL$HAC_LAGS) {
  assert_hetid_log_variance_fit(fit)
  assert_scalar_integer_in_range(
    hac_lags, "hac_lags", 0, .Machine$integer.max
  )
  vcov_list <- compute_log_variance_vcov(fit, hac_lags)
  coef_labels <- attr(fit, "coef_labels")
  coef_values <- if (log_variance_fit_ok(fit)) {
    fit$coef
  } else {
    rep(NA_real_, length(coef_labels))
  }
  log_variance_se_frame(coef_values, coef_labels, vcov_list)
}

#' Assemble the Log-Variance SE Frame From a Vcov List
#'
#' Ports \code{logvar_se_frame()} from the paper pipeline
#' (\code{scripts-paper/log_variance/inference/standard_error_estimators.R}):
#' one SE column per \code{vcov_list} entry, \code{sqrt(diag(.))} with a
#' negative or non-finite diagonal rendered \code{NA}. \code{pmax(d, 0)}
#' guards \code{sqrt()} from ever seeing a negative value -- \code{ifelse()}
#' evaluates its \code{yes} branch for every element regardless of the test,
#' so without it a negative diagonal would still trigger a "NaNs produced"
#' warning even though the result is discarded in favor of \code{NA}.
#'
#' @param coef_values Numeric vector, one entry per coefficient (may be
#'   \code{NA_real_} on a failed fit)
#' @param labels Character vector of coefficient labels, same length as
#'   \code{coef_values}
#' @param vcov_list Named list of square covariance matrices, one per SE type
#'
#' @return A \code{data.frame} with \code{term}, \code{coef}, and one column
#'   per \code{vcov_list} entry, default integer row names
#' @noRd
log_variance_se_frame <- function(coef_values, labels, vcov_list) {
  se <- lapply(vcov_list, function(v) {
    d <- diag(v)
    ifelse(is.finite(d) & d >= 0, sqrt(pmax(d, 0)), NA_real_)
  })
  data.frame(
    term = labels, coef = coef_values, se,
    row.names = NULL, check.names = FALSE
  )
}
