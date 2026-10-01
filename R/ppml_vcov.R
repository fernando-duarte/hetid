#' PPML Covariance Variants
#'
#' The four analytic (non-bootstrap) QMLE covariance matrices for the
#' log-link quasi-Poisson log-variance fit.
#'
#' @details
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
#' The bread is inverted through the shared conditioning gate
#' (\code{\link{se_norm_inv}}); a \code{NULL} inverse -- a non-finite,
#' singular, or ill-conditioned bread -- fails every variant closed to an
#' all-NA matrix, exactly as \code{\link{se_preflight}} does for a bad
#' coefficient, response, or nonpositive \eqn{\mu}. The raw
#' \code{(coef, y, x_mat, hac_lags)} signature is the registry's \code{vcov}
#' contract. The public entrypoints are \code{\link{compute_log_variance_vcov}}
#' and \code{\link{compute_log_variance_vcov_at_coef}}.
#'
#' No rows are omitted for missing values. This helper evaluates formulas at
#' the supplied coefficient without checking convergence or optimality.
#' Malformed arguments are validated by the public entrypoints. Arithmetic
#' overflow after preflight or inversion can still yield nonfinite entries.
#'
#' @param coef Numeric coefficient vector of length \code{ncol(x_mat)}, on the
#'   response scale of \code{y}, or \code{NULL} for a failed fit. Entries must
#'   follow the design-column order.
#' @param y Numeric nonnegative response vector of length \code{nrow(x_mat)},
#'   aligned with the design rows. Zero responses are allowed.
#' @param x_mat Numeric design matrix with column labels naming the coefficient
#'   axis. Supply the complete fitted design, including its intercept column
#'   if used; none is added.
#' @param hac_lags Single nonnegative integer Newey-West lag truncation, in
#'   observations. Rows of \code{x_mat} and \code{y} are assumed chronological.
#'   Zero makes \code{hac} equal \code{hc0}. Lags at or beyond the sample length
#'   contribute no cross-products; the supplied bandwidth still sets weights.
#'
#' @param rcond_tol Finite positive scalar tolerance for the reciprocal
#'   condition number of the diagonally normalized information matrix.
#'   Defaults to \code{LOG_VARIANCE_CONTROL$RCOND_TOLERANCE}.
#'
#' @return A named list of numeric covariance matrices keyed by
#'   \code{LOG_VARIANCE_CONTROL$SE_TYPES}. Each matrix has
#'   \code{ncol(x_mat)} rows and columns, labelled by \code{colnames(x_mat)}
#'   on both axes. All matrices contain \code{NA_real_} when inputs are
#'   nonfinite or incompatible, responses are negative, fitted means are
#'   nonpositive or nonfinite, \code{nrow(x_mat) <= ncol(x_mat)}, or the
#'   information matrix fails the inversion gate.
#' @keywords internal
ppml_vcov_variants <- function(
  coef, y, x_mat, hac_lags,
  rcond_tol = LOG_VARIANCE_CONTROL$RCOND_TOLERANCE
) {
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
    crossprod(x_mat, mu * x_mat), rcond_tol
  )
  r <- y - mu
  u <- x_mat * r
  phi <- sum(r^2 / mu) / (n - p)
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
