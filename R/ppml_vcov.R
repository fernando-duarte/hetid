#' PPML Covariance Variants
#'
#' The four analytic (non-bootstrap) QMLE covariance matrices for the
#' log-link quasi-Poisson log-variance fit, ported from the paper pipeline
#' (\code{scripts-paper/log_variance/estimators/ppml/standard_errors.R}).
#' \eqn{\hat\theta} solves \eqn{X'(y - \exp(X\theta)) = 0}, so every variant is
#' a pure function of the accepted coefficient, the response \code{y}, and the
#' design \code{X}: no fit object and no \code{response_scale} are needed, the
#' map being scale-invariant with the original-scale coefficient reproducing
#' \eqn{\mu}. The variants, with \eqn{A = X' diag(\mu) X} and
#' \eqn{r = y - \mu}:
#' \describe{
#'   \item{naive}{Pearson-dispersion-scaled model information
#'     \eqn{\hat\phi A^{-1}}}
#'   \item{hc0}{Eicker-White sandwich \eqn{A^{-1} X' diag(r^2) X A^{-1}}}
#'   \item{hc1}{\code{hc0} with the \eqn{n / (n - p)} factor}
#'   \item{hac}{Newey-West Bartlett HAC of the score,
#'     \eqn{A^{-1} M_{hac} A^{-1}}}
#' }
#' The SEs are hand-rolled in base R rather than delegated to a reconstructed
#' \code{glm}/\code{sandwich} object: a refit is a second optimization that can
#' drift from the coefficient the fit actually reports.
#'
#' The bread is inverted through the shared conditioning gate
#' (\code{\link{se_norm_inv}}); a \code{NULL} inverse -- a non-finite,
#' singular, or ill-conditioned bread -- fails every variant closed to an
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
#'   \code{LOG_VARIANCE_CONTROL$SE_TYPES}
#' @keywords internal
ppml_vcov_variants <- function(coef, y, x_mat, hac_lags) {
  se_types <- LOG_VARIANCE_CONTROL$SE_TYPES
  pre <- se_preflight(coef, y, x_mat, hac_lags, se_types)
  if (!pre$ok) {
    return(pre$na_out)
  }
  n <- pre$n
  p <- pre$p
  mu <- pre$mu
  na_mat <- pre$na_mat
  a_inv <- se_norm_inv(
    crossprod(x_mat, mu * x_mat), LOG_VARIANCE_CONTROL$RCOND_TOLERANCE
  )
  r <- y - mu
  u <- x_mat * r # per-observation score rows
  phi <- sum(r^2 / mu) / (n - p) # Pearson dispersion
  sandwich_v <- function(meat) {
    if (is.null(a_inv)) na_mat else a_inv %*% meat %*% a_inv
  }
  v_hc0 <- sandwich_v(crossprod(u))
  list(
    naive = if (is.null(a_inv)) na_mat else phi * a_inv,
    hc0 = v_hc0,
    hc1 = (n / (n - p)) * v_hc0,
    hac = sandwich_v(se_bartlett_meat(u, pre$hac_lags))
  )
}
