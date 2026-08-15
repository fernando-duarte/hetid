#' Estimate the Tau = 0 Mean-Equation System
#'
#' Top-level orchestrator for the tau = 0 special case of the Lewbel (2012)
#' triangular system: reduced-form residualization of \eqn{Y_1} and
#' \eqn{Y_2} on the common conditioning vector \eqn{X}, followed by the
#' closed-form structural-parameter point solve at \eqn{\tau = 0}. This is
#' the mean-equation entry point of the pipeline; the log-variance equation
#' is a separate estimator built on the same reduced-form machinery.
#'
#' @param y1 Numeric vector of length \eqn{T}: the mean-equation outcome
#'   \eqn{Y_1} (e.g. consumption growth).
#' @param y2 Numeric matrix (or vector) of \eqn{T} rows: the \eqn{I}
#'   news/innovation variables \eqn{Y_2}, with unique, non-blank column names.
#' @param x Numeric matrix (or vector) of \eqn{T} rows: the common
#'   conditioning regressors \eqn{X} (principal components, own-lags, ...).
#'   See the \strong{x contract} section below.
#' @param z Numeric matrix (or vector) of \eqn{T} rows: the \eqn{J}
#'   instrument(s) that enter the heteroskedasticity condition.
#' @param impose_null Logical; if \code{TRUE}, impose \eqn{B = 0}
#'   structurally on the \eqn{Y_2} reduced form instead of estimating it
#'   (\code{w2 <- y2}, \code{beta2r} all zero). Default \code{FALSE}.
#' @param gamma \code{NULL} or a numeric \eqn{J \times I} matrix of
#'   instrument weights. Defaults to \code{matrix(1, 1, ncol(y2))} only when
#'   \code{ncol(z) == 1} (the paper's benchmark instrument); with a
#'   multi-column \code{z}, \code{gamma} is required, since an implicit
#'   equal-weight direction is units-dependent and would silently change
#'   the estimand. A supplied \code{gamma} must have \code{dim ==
#'   c(ncol(z), ncol(y2))}; if it carries dimnames they must equal
#'   \code{colnames(z)} and \code{colnames(y2)} exactly (a correctly sized
#'   but permuted named \code{gamma} silently changes the estimand).
#' @param tol Positive numeric scalar, the point-solve tolerance passed to
#'   \code{\link{compute_tau0_point}} (default
#'   \code{HETID_CONSTANTS$TAU0_POINT_TOLERANCE}).
#'
#' @return A validated \code{hetid_tau0_fit} object; see
#'   \code{\link{hetid_tau0_fit}} for the full container contract.
#' \describe{
#'   \item{beta1r, beta2r}{Reduced-form OLS coefficients of \eqn{Y_1} and
#'     \eqn{Y_2} on \eqn{X}.}
#'   \item{w1, w2}{Reduced-form residuals.}
#'   \item{z, gamma}{The de-meaned instrument matrix and the resolved
#'     instrument weights.}
#'   \item{moments, point}{The \code{hetid_moments} object and the tau = 0
#'     point solve (\code{NULL} when the stacked system has no unique
#'     consistent solution).}
#'   \item{beta1}{The recovered structural coefficients, or \code{NULL}
#'     exactly when \code{point} is \code{NULL}.}
#' }
#'
#' @section Alignment contract:
#' \code{y1}, \code{y2}, \code{x}, and \code{z} must already be row-aligned
#' by the caller (merged by calendar date upstream), with identical row
#' counts and no missing rows. Any \code{NA}, \code{NaN}, or infinite value
#' anywhere in the inputs is a structured \code{hetid_error_bad_argument}
#' and is never silently filtered: an independent \code{complete.cases}
#' filter per input would desynchronize the mean and volatility samples.
#' Row order is assumed chronological, for downstream HAC lag structure,
#' but that ordering is documented, not enforced.
#'
#' @section The x contract:
#' \code{x} must \strong{not} contain an intercept/constant column: the
#' internal regressions add their own, and a constant column aliases it,
#' which \code{\link{run_pc_regression}} raises as a hard
#' \code{hetid_error} (rank-deficient design). No column of \code{x} may be
#' named \code{"y"} (it collides with the internal regression response).
#' Non-syntactic column names are sanitized by \code{\link[base]{make.names}}
#' inside \code{\link{run_pc_regression}}, and the sanitized names propagate
#' into \code{beta1r}, \code{beta2r}, and the returned container.
#'
#' @details
#' At \eqn{\tau = 0} the heteroskedasticity condition
#' \eqn{E[\varepsilon_1 \varepsilon_2 \mid Z] = \tau \,
#' \mathrm{Var}(\varepsilon_2 \mid Z)} collapses each maturity's quadratic
#' identified-set constraint to the linear equality
#' \deqn{\mathbf{Q}_i^\top \boldsymbol{\theta} = L_i,}
#' so stacking across maturities gives a single linear system, solved (when
#' full rank and consistent) by \code{\link{compute_tau0_point}}; see that
#' function for the exact solve. The structural coefficients then follow
#' from the exact affine identity in
#' \code{\link{recover_structural_coefficients}}.
#'
#' @seealso \code{\link{tau0_reduced_forms}}, \code{\link{compute_tau0_point}},
#'   \code{\link{recover_structural_coefficients}}
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
#' fit$point$theta
compute_tau0_system <- function(y1, y2, x, z, impose_null = FALSE,
                                gamma = NULL,
                                tol = HETID_CONSTANTS$TAU0_POINT_TOLERANCE) {
  validated <- validate_tau0_inputs(y1, y2, x, z, gamma, impose_null, tol)
  reduced <- tau0_reduced_forms(validated$y1, validated$y2, validated$x, impose_null)

  moments <- compute_identification_moments(reduced$w1, reduced$w2, validated$z)
  components <- compute_identified_set_components(validated$gamma, moments)
  point <- compute_tau0_point(components, tol)

  beta1 <- if (is.null(point)) {
    NULL
  } else {
    recover_structural_coefficients(reduced$beta1r, reduced$beta2r, point$theta)
  }

  validate_hetid_tau0_fit(new_hetid_tau0_fit(
    beta1r = reduced$beta1r, beta2r = reduced$beta2r, w1 = reduced$w1, w2 = reduced$w2,
    z = validated$z, gamma = validated$gamma, moments = moments,
    point = point, beta1 = beta1, n_obs = validated$n_obs,
    impose_null = impose_null, tol = tol
  ))
}
