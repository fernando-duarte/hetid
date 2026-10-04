#' Fit the Log-Variance Equation at a Fixed Structural Parameter
#'
#' Completes the tau = 0 chain: given the mean-equation reduced-form
#' residuals \code{w1}, \code{w2} and a fixed value \code{b} of the
#' structural parameter (e.g. \code{compute_tau0_system()}'s
#' \code{point$theta}), forms \eqn{\varepsilon = w_1 - w_2 b} and delegates
#' \eqn{\varepsilon^2} to \code{\link{fit_log_variance}} as the log-variance
#' equation's response.
#'
#' @param b Finite numeric vector of length \code{ncol(w2)}: the fixed
#'   structural parameter. If both \code{b} and the columns of \code{w2}
#'   are named, \code{names(b)} must equal \code{colnames(w2)} exactly,
#'   including their order. Otherwise, \code{b} is used positionally.
#' @param w1 Numeric vector of length \code{nrow(w2)}: the mean-equation
#'   reduced-form residual (\code{compute_tau0_system()}'s \code{w1}).
#' @param w2 Numeric matrix, \code{ncol(w2) == length(b)}: the news-equation
#'   reduced-form residuals (\code{compute_tau0_system()}'s \code{w2}).
#' @param x Numeric matrix or numeric data frame with \code{nrow(w2)} rows:
#'   the volatility-equation regressors, without an intercept column.
#'   An intercept is added internally. Requires at least \code{ncol(x) + 2}
#'   observations; see \code{\link{fit_log_variance}} for the full design
#'   contract and the \strong{Two designs} section below.
#' @param estimator,start,fallback_starts,response_scale,control Passed through to
#'   \code{\link{fit_log_variance}} unchanged; see that function -- in
#'   particular its \strong{Start-scale contract} section -- for the exact
#'   contract. The estimator is \code{"ppml"} by default, or
#'   \code{"harvey"}; the remaining defaults are \code{NULL},
#'   \code{list()}, \code{1}, and \code{list()}, respectively.
#'
#' @return A \code{hetid_log_variance_fit} object (see
#'   \code{\link{hetid_log_variance_fit}}), with one extra
#'   \code{diagnostics$min_abs_eps} field: \code{min(abs(eps))} for
#'   \eqn{\varepsilon = w_1 - w_2 b}, a cheap check for a residual sitting
#'   at (or near) zero. A failure reported by \code{\link{fit_log_variance}}
#'   returns an object with
#'   \code{fit_status = "nonconvergence"}, \code{coef = NULL}, and
#'   \code{warm_start = NULL}, retaining the response, design, and diagnostics.
#'   This includes an all-zero squared-residual response.
#'
#' @section Input alignment:
#' \code{b}, \code{w1}, \code{w2}, and \code{x} must contain only finite
#' numeric values; missing values are rejected rather than filtered.
#' The computed squared residuals must also remain finite.
#' \code{w1}, \code{w2}, and \code{x} must already be row-aligned by the
#' caller, merging time series by calendar date upstream. Dates are not
#' stored or checked here. Keep chronological row order for subsequent HAC
#' inference. Invalid values or names raise a structured
#' \code{hetid_error_bad_argument}; incompatible dimensions raise
#' \code{hetid_error_dimension_mismatch}, and too few rows in \code{x} raise
#' \code{hetid_error_insufficient_data}.
#'
#' @section Guard the composition:
#' \code{compute_tau0_system()} returns \code{point = NULL} whenever the
#' stacked tau = 0 system has no unique consistent solution, and then there
#' is no \code{point$theta} to pass. Callers must check
#' \code{is.null(fit$point)} before calling this function with
#' \code{fit$point$theta}; see the example.
#'
#' @section Two designs:
#' The mean and log-variance equations need not use the same regressors.
#' Principal components in the intended application are of nominal financial
#' asset returns. \code{x} here supplies the volatility regressors, chosen
#' separately from \code{x} passed to \code{\link{compute_tau0_system}}.
#'
#' @seealso \code{\link{compute_tau0_system}}, \code{\link{fit_log_variance}},
#'   and \code{\link{compute_log_variance_vcov}} for the fit's covariance
#'   matrices -- read its \strong{Inference caveats}: those standard errors are
#'   conditional on the \code{b} passed here and do not propagate its own
#'   sampling uncertainty.
#'
#' @export
#'
#' @examples
#' local({
#'   old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
#'     get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
#'   } else {
#'     NULL
#'   }
#'   on.exit(if (is.null(old_seed)) {
#'     rm(".Random.seed", envir = .GlobalEnv)
#'   } else {
#'     assign(".Random.seed", old_seed, envir = .GlobalEnv)
#'   })
#'   set.seed(42)
#'   t_obs <- 150
#'   x <- cbind(x1 = rnorm(t_obs), x2 = rnorm(t_obs))
#'   z <- rnorm(t_obs)
#'   e2 <- sqrt(exp(0.5 + 0.9 * z)) * matrix(rnorm(t_obs * 2), t_obs, 2)
#'   y2 <- x %*% matrix(c(1, 0.5, -0.3, 0.7), 2, 2) + e2
#'   colnames(y2) <- c("news1", "news2")
#'   theta_true <- c(0.8, -0.5)
#'   y1 <- drop(0.3 + x %*% c(0.2, -0.1) + y2 %*% theta_true + rnorm(t_obs))
#'   fit <- compute_tau0_system(y1, y2, x, z)
#'
#'   x_var <- cbind(v1 = rnorm(t_obs), v2 = rnorm(t_obs))
#'   if (!is.null(fit$point)) {
#'     logvar_fit <- fit_log_variance_at_b(fit$point$theta, fit$w1, fit$w2, x_var)
#'     print(logvar_fit$coef)
#'     print(compute_log_variance_se(logvar_fit))
#'   }
#' })
fit_log_variance_at_b <- function(b, w1, w2, x, estimator = "ppml", start = NULL,
                                  fallback_starts = list(), response_scale = 1,
                                  control = list()) {
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

  fit <- fit_log_variance(
    eps^2, x,
    estimator = estimator, start = start,
    fallback_starts = fallback_starts, response_scale = response_scale, control = control
  )
  fit$diagnostics$min_abs_eps <- min(abs(eps))
  fit
}
