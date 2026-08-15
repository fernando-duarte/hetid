#' Fit the Log-Variance Equation via PPML
#'
#' Boundary wrapper around the log-variance estimator registry
#' (\code{\link{log_variance_estimator}}): validates \code{y}, \code{x},
#' \code{start}, \code{fallback_starts}, and \code{response_scale}, builds
#' the design matrix (\code{\link{log_variance_design}}), and dispatches to
#' the resolved estimator's \code{fit_response} worker. The only estimator
#' currently registered is \code{"ppml"} (\code{\link{ppml_fit_response}});
#' the registry, not this wrapper, owns which estimator strings are valid.
#'
#' @param y Numeric vector of length \eqn{T}: the nonnegative response
#'   (e.g. a squared or absolute residual). Must be finite and nonnegative.
#' @param x Numeric matrix (or data-frame-coercible object) of \eqn{T} rows:
#'   the volatility regressors, without an intercept column (one is
#'   prepended by \code{\link{log_variance_design}}). Requires at least
#'   \code{ncol(x) + 2} observations (see
#'   \code{\link{min_obs_for_pc_regression}}).
#' @param estimator Single string naming the estimator; passed unchecked to
#'   \code{\link{log_variance_estimator}}, which is the sole owner of the
#'   valid-estimator set. Default \code{"ppml"}.
#' @param start \code{NULL}, or a finite numeric vector of length
#'   \code{ncol(x) + 1} giving a starting value for
#'   \code{\link[stats]{glm.fit}} \strong{on the scaled response}
#'   (\code{y / response_scale}); see the \strong{Start-scale contract}
#'   section. When named, the names must equal the design column labels
#'   exactly -- a permuted named start against a different design is a
#'   silent trap, not accepted positionally.
#' @param fallback_starts List of finite numeric vectors, each following the
#'   same length and naming rule as \code{start}, tried in order after
#'   \code{start} fails or is not supplied.
#' @param response_scale Single finite positive numeric scalar dividing
#'   \code{y} before fitting. Default \code{1}. See the
#'   \strong{Start-scale contract} section.
#'
#' @return A validated \code{hetid_log_variance_fit} object; see
#'   \code{\link{hetid_log_variance_fit}} for the container contract and
#'   \code{\link{log_variance_fit_ok}} to check whether it is usable for
#'   inference.
#'
#' @details
#' The PPML estimator solves the log-link Poisson moment (score) equation
#' \deqn{X^\top (y - \exp(X \theta)) = 0}
#' by quasi-Poisson IRLS (\code{\link[stats]{glm.fit}} with
#' \code{family = quasipoisson(link = "log")}). Using quasi-Poisson rather
#' than Poisson changes only the reported dispersion (and so only the
#' standard errors downstream): the log link and mean structure are
#' identical, so the point estimate solves the same score equation either
#' way.
#'
#' A fitted rung is accepted only when every gate below holds; failing any
#' gate is fail-closed, not an error -- the returned object reports
#' \code{fit_status = "nonconvergence"} and the failing gate in
#' \code{diagnostics$error_class}:
#' \describe{
#'   \item{Scaled-response guards}{\code{y / response_scale} must not
#'     underflow a positive entry to zero, overflow to non-finite, or
#'     collapse to all-zero.}
#'   \item{Design rank}{the positive-response rows of the design must have
#'     full column rank.}
#'   \item{Finite, positive fit}{the fitted coefficients and
#'     \eqn{\exp(X\theta)} must be finite, with \eqn{\exp(X\theta) > 0}.}
#'   \item{IRLS convergence}{\code{glm.fit} must report convergence and no
#'     boundary solution.}
#'   \item{Score tolerance}{the scaled score norm must not exceed
#'     \code{LOG_VARIANCE_CONTROL$SCORE_TOLERANCE}.}
#'   \item{Conditioning}{the information matrix's reciprocal condition
#'     number must not fall below
#'     \code{LOG_VARIANCE_CONTROL$RCOND_TOLERANCE}.}
#' }
#'
#' Centering the columns of \code{x} before calling this function (as the
#' paper does for its volatility regressors) is a caller choice: it changes
#' only the intercept's interpretation (the fitted log-variance at the
#' centering point), not the slope coefficients or the fitted values.
#'
#' @section Start-scale contract:
#' \code{start}, \code{fallback_starts}, and the returned \code{warm_start}
#' all live on the scaled-response fit (\code{y / response_scale}) -- the
#' scale \code{glm.fit} actually sees. This is what lets a returned
#' \code{warm_start} be fed back as \code{start} at the same
#' \code{response_scale}. At the default \code{response_scale = 1} this
#' scaled fit is simply the natural scale. Only the returned \code{coef} is
#' on the original \code{y} scale (\code{coef[1] == warm_start[1] +
#' log(response_scale)}, other coefficients unchanged). Extreme
#' \code{response_scale} values degrade numerical precision without
#' changing the estimand.
#'
#' @seealso \code{\link{log_variance_estimator}},
#'   \code{\link{log_variance_design}}, \code{\link{ppml_fit_response}}
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
#' fit$coef
fit_log_variance <- function(y, x, estimator = "ppml", start = NULL,
                             fallback_starts = list(), response_scale = 1) {
  validate_numeric_inputs(y = y)
  assert_numeric_finite_values(y, "y")
  assert_bad_argument_ok(all(y >= 0), "y must be nonnegative", arg = "y")

  assert_tabular(x, "x")
  x <- as.matrix(x)
  assert_numeric_finite_values(x, "x")
  assert_dimension_ok(nrow(x) == length(y), "x must have length(y) rows")

  min_obs <- min_obs_for_pc_regression(ncol(x))
  assert_insufficient_data_ok(
    length(y) >= min_obs,
    paste0(
      "Insufficient observations for the log-variance fit: got ", length(y),
      ", need at least ", min_obs, " (ncol(x) + 2)"
    )
  )

  assert_scalar_finite(response_scale, "response_scale")
  assert_bad_argument_ok(
    response_scale > 0, "response_scale must be positive",
    arg = "response_scale"
  )

  spec <- log_variance_estimator(estimator)
  x_mat <- log_variance_design(x)
  p <- ncol(x_mat)
  design_labels <- colnames(x_mat)

  if (!is.null(start)) {
    assert_log_variance_start(start, p, design_labels, "start")
  }
  assert_bad_argument_ok(
    is.list(fallback_starts), "fallback_starts must be a list",
    arg = "fallback_starts"
  )
  for (i in seq_along(fallback_starts)) {
    assert_log_variance_start(
      fallback_starts[[i]], p, design_labels, paste0("fallback_starts[[", i, "]]")
    )
  }

  spec$fit_response(y, x_mat, start, fallback_starts, response_scale)
}

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
#'
#' @return Invisible TRUE when valid
#' @noRd
assert_log_variance_start <- function(val, p, labels, arg) {
  assert_bad_argument_ok(
    is.numeric(val) && is.null(dim(val)) && length(val) == p &&
      all(is.finite(val)),
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
