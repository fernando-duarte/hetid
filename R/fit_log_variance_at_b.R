#' Fit the Log-Variance Equation at a Fixed Structural Parameter
#'
#' Completes the tau = 0 chain: given the mean-equation reduced-form
#' residuals \code{w1}, \code{w2} and a fixed value \code{b} of the
#' structural parameter (e.g. \code{compute_tau0_system()}'s
#' \code{point$theta}), forms \eqn{\varepsilon = w_1 - w_2 b} and delegates
#' \eqn{\varepsilon^2} to \code{\link{fit_log_variance}} as the log-variance
#' equation's response.
#'
#' @param b Numeric vector of length \code{ncol(w2)}: the fixed structural
#'   parameter. When named, the names must equal \code{colnames(w2)} exactly
#'   -- a silently permuted \code{b} would change which residual multiplies
#'   which column, with no other symptom. Unnamed \code{b} is positional.
#' @param w1 Numeric vector of length \code{nrow(w2)}: the mean-equation
#'   reduced-form residual (\code{compute_tau0_system()}'s \code{w1}).
#' @param w2 Numeric matrix, \code{ncol(w2) == length(b)}: the news-equation
#'   reduced-form residuals (\code{compute_tau0_system()}'s \code{w2}).
#' @param x Numeric matrix (or data-frame-coercible object) of
#'   \code{nrow(w2)} rows: the volatility-equation regressors, passed
#'   through to \code{\link{fit_log_variance}}. \strong{Distinct from the
#'   mean equation's regressors} -- see the \strong{Two designs} section.
#' @param estimator,start,fallback_starts,response_scale Passed through to
#'   \code{\link{fit_log_variance}} unchanged; see that function -- in
#'   particular its \strong{Start-scale contract} section -- for the exact
#'   contract.
#'
#' @return A validated \code{hetid_log_variance_fit} object (see
#'   \code{\link{hetid_log_variance_fit}}), with one extra
#'   \code{diagnostics$min_abs_eps} field: \code{min(abs(eps))} for
#'   \eqn{\varepsilon = w_1 - w_2 b}, a cheap check for a residual sitting
#'   at (or near) zero.
#'
#' @section Guard the composition:
#' \code{compute_tau0_system()} returns \code{point = NULL} whenever the
#' stacked tau = 0 system has no unique consistent solution, and then there
#' is no \code{point$theta} to pass. Callers must check
#' \code{is.null(fit$point)} before calling this function with
#' \code{fit$point$theta}; see the example.
#'
#' @section Two designs:
#' The mean equation and the log-variance equation use different regressor
#' sets in the intended application: the mean equation conditions on SDF
#' principal components, while the volatility equation conditions on lagged
#' return principal components. \code{x} here is the volatility design, not
#' the \code{x} passed to \code{\link{compute_tau0_system}}.
#'
#' @seealso \code{\link{compute_tau0_system}}, \code{\link{fit_log_variance}}
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' t_obs <- 150
#' x <- cbind(x1 = rnorm(t_obs), x2 = rnorm(t_obs))
#' z <- rnorm(t_obs)
#' e2 <- sqrt(exp(0.5 + 0.9 * z)) * matrix(rnorm(t_obs * 2), t_obs, 2)
#' y2 <- x %*% matrix(c(1, 0.5, -0.3, 0.7), 2, 2) + e2
#' colnames(y2) <- c("news1", "news2")
#' theta_true <- c(0.8, -0.5)
#' y1 <- drop(0.3 + x %*% c(0.2, -0.1) + y2 %*% theta_true + rnorm(t_obs))
#' fit <- compute_tau0_system(y1, y2, x, z)
#'
#' # the volatility design is distinct from the mean-equation design above
#' x_var <- cbind(v1 = rnorm(t_obs), v2 = rnorm(t_obs))
#' if (!is.null(fit$point)) {
#'   logvar_fit <- fit_log_variance_at_b(fit$point$theta, fit$w1, fit$w2, x_var)
#'   logvar_fit$coef
#' }
fit_log_variance_at_b <- function(b, w1, w2, x, estimator = "ppml", start = NULL,
                                  fallback_starts = list(), response_scale = 1) {
  assert_bad_argument_ok(
    is.matrix(w2) && is.numeric(w2), "w2 must be a numeric matrix",
    arg = "w2"
  )
  assert_numeric_finite_values(w2, "w2")

  assert_bad_argument_ok(
    is.numeric(b) && is.null(dim(b)), "b must be a numeric vector",
    arg = "b"
  )
  assert_numeric_finite_values(b, "b")
  assert_dimension_ok(
    length(b) == ncol(w2),
    paste0("length(b) (", length(b), ") must equal ncol(w2) (", ncol(w2), ")")
  )

  assert_bad_argument_ok(
    is.numeric(w1) && is.null(dim(w1)), "w1 must be a numeric vector",
    arg = "w1"
  )
  assert_numeric_finite_values(w1, "w1")
  assert_dimension_ok(
    length(w1) == nrow(w2),
    paste0("length(w1) (", length(w1), ") must equal nrow(w2) (", nrow(w2), ")")
  )

  b_names <- names(b)
  w2_names <- colnames(w2)
  if (!is.null(b_names) && !is.null(w2_names)) {
    assert_bad_argument_ok(
      identical(b_names, w2_names),
      paste0(
        "names(b), when supplied, must equal colnames(w2) exactly -- a ",
        "permuted b silently changes eps"
      ),
      arg = "b"
    )
  }

  eps <- drop(w1 - w2 %*% b)
  min_abs_eps <- min(abs(eps))

  fit <- fit_log_variance(
    eps^2, x,
    estimator = estimator, start = start,
    fallback_starts = fallback_starts, response_scale = response_scale
  )
  fit$diagnostics$min_abs_eps <- min_abs_eps
  validate_hetid_log_variance_fit(fit)
}
