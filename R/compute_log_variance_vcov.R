#' Covariance Matrices for a Log-Variance Fit
#'
#' Boundary wrapper around the log-variance estimator registry's \code{vcov}
#' worker (for \code{"ppml"}, \code{\link{ppml_vcov_variants}}). The fit object
#' is the only data source: the estimator comes from the fit's
#' \code{estimator} attribute, and the coefficients, response, and design come
#' from \code{fit$coef}, \code{fit$y}, and \code{fit$x_design}. Nothing is
#' re-passed, so a mismatched response or a permuted same-width design is
#' impossible by construction -- this is why the container carries its data.
#'
#' @param fit A \code{hetid_log_variance_fit} object from
#'   \code{\link{fit_log_variance}} or \code{\link{fit_log_variance_at_b}}.
#' @param hac_lags Single nonnegative integer: the Newey-West lag truncation
#'   for the \code{hac} variant. Default
#'   \code{LOG_VARIANCE_CONTROL$HAC_LAGS} (the paper's quarterly heuristic).
#'   \code{0} collapses \code{hac} to \code{hc0}.
#'
#' @return A named list of \code{(k + 1) x (k + 1)} covariance matrices keyed
#'   by the estimator's \code{se_types} (for \code{"ppml"}: \code{"naive"},
#'   \code{"hc0"}, \code{"hc1"}, \code{"hac"}), each labelled with the fit's
#'   \code{coef_labels} on both axes. Standard errors are
#'   \code{sqrt(diag(.))}.
#'
#' @details
#' Malformed \emph{arguments} are errors: a first argument that is not a
#' \code{hetid_log_variance_fit}, or an \code{hac_lags} that is not a scalar
#' nonnegative integer, signals \code{hetid_error_bad_argument}. Data-quality
#' failures are fail-closed instead, returning all-NA matrices of the right
#' shape so a vcov failure cannot kill a caller's estimation loop: a failed fit
#' (\code{!\link{log_variance_fit_ok}(fit)}), non-finite coefficients, a
#' singular or ill-conditioned bread, or \code{n <= p}.
#'
#' @section Inference caveats:
#' These are \strong{conditional second-stage} standard errors, computed at a
#' fixed plug-in \code{b}: the Stage 1 (Lewbel point) sampling uncertainty in
#' \code{b} is not propagated, so they understate the total uncertainty of a
#' log-variance coefficient estimated at an estimated \code{b}. The paper
#' handles that with a bootstrap over the whole chain, which is out of scope
#' here.
#'
#' The \code{hac} variant assumes the rows of \code{fit$y} and
#' \code{fit$x_design} are in \strong{chronological order}: the container
#' carries no date index, so this is a caller precondition, not something the
#' function can check. Rows in an arbitrary order make the lag autocovariances
#' meaningless (the other three variants are order-invariant).
#'
#' @seealso \code{\link{fit_log_variance}},
#'   \code{\link{fit_log_variance_at_b}}, \code{\link{LOG_VARIANCE_CONTROL}}
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
#' vcov_list <- compute_log_variance_vcov(fit)
#' sqrt(diag(vcov_list$hac))
compute_log_variance_vcov <- function(fit,
                                      hac_lags = LOG_VARIANCE_CONTROL$HAC_LAGS) {
  assert_hetid_log_variance_fit(fit)
  assert_scalar_integer_in_range(
    hac_lags, "hac_lags", 0, .Machine$integer.max
  )
  spec <- log_variance_estimator(attr(fit, "estimator"))
  if (!log_variance_fit_ok(fit)) {
    return(se_preflight(
      fit$coef, fit$y, fit$x_design, hac_lags, spec$se_types
    )$na_out)
  }
  spec$vcov(fit$coef, fit$y, fit$x_design, hac_lags)
}
