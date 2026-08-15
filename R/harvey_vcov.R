#' Harvey Covariance Variants
#'
#' The five analytic (non-bootstrap) QMLE covariance matrices for the Harvey
#' Gaussian multiplicative-heteroskedasticity log-variance fit, ported from
#' the paper pipeline
#' (\code{scripts-paper/log_variance/estimators/harvey/standard_errors.R}).
#' \eqn{\hat\theta} minimizes \eqn{0.5 \sum_t (\eta_t + y_t e^{-\eta_t})} with
#' \eqn{\eta = X\theta}, so every variant is a pure function of the accepted
#' coefficient, the response \code{y}, and the design \code{X}: no fit object
#' and no \code{response_scale} are needed, the map being scale-invariant with
#' the original-scale coefficient reproducing \eqn{\mu = \exp(X\theta)}. The
#' variants, with \eqn{r = y / \mu}, per-observation score rows
#' \eqn{g_t = 0.5 (1 - r_t) x_t}, and observed information
#' \eqn{H = 0.5 X' diag(r) X}:
#' \describe{
#'   \item{expected}{Gaussian working-model Fisher information,
#'     \eqn{(0.5 X'X)^{-1}}}
#'   \item{observed}{Gaussian working-model observed information,
#'     \eqn{H^{-1}}}
#'   \item{opg}{outer product of gradients (BHHH), \eqn{(G'G)^{-1}}}
#'   \item{robust}{Eicker-White QMLE sandwich \eqn{H^{-1} G'G H^{-1}}}
#'   \item{hac}{Newey-West Bartlett HAC of the score,
#'     \eqn{H^{-1} M_{hac} H^{-1}}}
#' }
#' The SEs are hand-rolled in base R: the Harvey QMLE is not a \code{glm}
#' object a sandwich package could dispatch on.
#'
#' Each bread is inverted through the shared conditioning gate
#' (\code{\link{se_norm_inv}}); a \code{NULL} inverse -- a non-finite,
#' singular, or ill-conditioned bread -- fails that variant closed to an
#' all-NA matrix, exactly as \code{\link{se_preflight}} does for a bad
#' coefficient, response, or nonpositive \eqn{\mu}. The raw
#' \code{(coef, y, x_mat, hac_lags)} signature is the registry's \code{vcov}
#' contract; the exported boundary is
#' \code{\link{compute_log_variance_vcov}}.
#'
#' @param coef Numeric coefficient vector of length \code{ncol(x_mat)}, on the
#'   same scale as \code{y}
#' @param y Numeric nonnegative response
#' @param x_mat Numeric design matrix, intercept column included, with column
#'   labels naming the coefficient axis
#' @param hac_lags Nonnegative integer Newey-West lag truncation; rows of
#'   \code{x_mat} and \code{y} are assumed chronological
#'
#' @return Named list of \code{ncol(x_mat)} square matrices keyed by
#'   \code{LOG_VARIANCE_HARVEY_CONTROL$SE_TYPES}
#' @keywords internal
harvey_vcov_variants <- function(coef, y, x_mat, hac_lags) {
  se_types <- LOG_VARIANCE_HARVEY_CONTROL$SE_TYPES
  rcond_tol <- LOG_VARIANCE_HARVEY_CONTROL$RCOND_TOLERANCE
  pre <- se_preflight(coef, y, x_mat, hac_lags, se_types)
  if (!pre$ok) {
    return(pre$na_out)
  }
  na_mat <- pre$na_mat
  r <- y / pre$mu # zero-safe: y >= 0, mu > 0 (a zero response gives r = 0)
  g <- 0.5 * (1 - r) * x_mat # per-observation score rows
  h_inv <- se_norm_inv(0.5 * crossprod(x_mat, r * x_mat), rcond_tol)
  ex_inv <- se_norm_inv(0.5 * crossprod(x_mat), rcond_tol)
  meat_opg <- crossprod(g)
  opg_inv <- se_norm_inv(meat_opg, rcond_tol)
  sandwich_v <- function(bread, meat) {
    if (is.null(bread)) na_mat else bread %*% meat %*% bread
  }
  list(
    expected = if (is.null(ex_inv)) na_mat else ex_inv,
    observed = if (is.null(h_inv)) na_mat else h_inv,
    opg = if (is.null(opg_inv)) na_mat else opg_inv,
    robust = sandwich_v(h_inv, meat_opg),
    hac = sandwich_v(h_inv, se_bartlett_meat(g, pre$hac_lags))
  )
}
