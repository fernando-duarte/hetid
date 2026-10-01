#' Harvey Covariance Variants
#'
#' The five analytic (non-bootstrap) QMLE covariance matrices for the Harvey
#' Gaussian multiplicative-heteroskedasticity log-variance fit.
#'
#' @details
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
#' contract. The public entrypoints are \code{\link{compute_log_variance_vcov}}
#' and \code{\link{compute_log_variance_vcov_at_coef}}.
#'
#' Missing or non-finite coefficients, responses, or design entries, negative
#' responses, inconsistent vector lengths, or
#' \code{nrow(x_mat) <= ncol(x_mat)} return all-NA matrices;
#' no rows are omitted. Zero responses are allowed. Non-finite fitted means
#' also return all-NA matrices. Later arithmetic overflow can still produce
#' non-finite covariance entries. The coefficient's convergence or optimality
#' is not checked here, and first-stage estimation uncertainty is not propagated.
#' The public entrypoints validate matrix structure and scalar controls before
#' calling this internal helper.
#'
#' @param coef Numeric coefficient vector of length \code{ncol(x_mat)}, on the
#'   same response scale as \code{y} and in design-column order. \code{NULL}
#'   represents a failed fit and returns all-NA matrices.
#' @param y Numeric nonnegative response vector of length \code{nrow(x_mat)}.
#' @param x_mat Numeric design matrix, intercept column included, with column
#'   labels naming the coefficient axis. No intercept is added. Rows must align
#'   with \code{y} and be chronological for HAC; order is not checked.
#' @param hac_lags Nonnegative integer Newey-West lag truncation, in observations.
#'   Zero makes \code{hac} equal \code{robust}. Lags beyond the sample length add
#'   no cross-products, but the supplied truncation still determines their weights.
#'
#' @param rcond_tol Finite positive scalar tolerance for the reciprocal condition
#'   number of diagonally normalized information. Defaults to
#'   \code{LOG_VARIANCE_HARVEY_CONTROL$RCOND_TOLERANCE}.
#'
#' @return Named list keyed by \code{LOG_VARIANCE_HARVEY_CONTROL$SE_TYPES}.
#'   Each element is a numeric \code{ncol(x_mat) x ncol(x_mat)} covariance matrix,
#'   labelled on both axes by \code{colnames(x_mat)}. An unusable input returns
#'   all-NA matrices; an unavailable inverse makes the variants requiring it all-NA.
#' @keywords internal
harvey_vcov_variants <- function(
  coef, y, x_mat, hac_lags,
  rcond_tol = LOG_VARIANCE_HARVEY_CONTROL$RCOND_TOLERANCE
) {
  se_types <- LOG_VARIANCE_HARVEY_CONTROL$SE_TYPES
  pre <- se_preflight(coef, y, x_mat, hac_lags, se_types)
  if (!pre$ok) {
    return(pre$na_out)
  }
  na_mat <- pre$na_mat
  r <- y / pre$mu
  g <- 0.5 * (1 - r) * x_mat
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
