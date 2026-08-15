#' Estimator-Agnostic Standard-Error Scaffolding
#'
#' The pieces every log-variance covariance estimator shares: the normalized
#' fail-closed inverse of a bread matrix, the Bartlett/Newey-West HAC meat of a
#' score matrix, and the preflight that either hands back \eqn{\mu} or the
#' all-NA skeleton. Ported from the paper pipeline
#' (\code{scripts-paper/log_variance/inference/standard_error_estimators.R}).
#' Estimator modules keep only their own bread, score, and variant assembly.
#'
#' These internals never raise: malformed arguments are the exported
#' boundary's business (\code{\link{compute_log_variance_vcov}}), and every
#' data-quality failure here comes back as \code{NULL} or the skeleton.
#'
#' @name log_variance_se_utils
#' @keywords internal
NULL

#' Invert a Symmetric Bread Through the Conditioning Gate
#'
#' Normalizes by the diagonal, gates \code{rcond}, Cholesky-inverts, and
#' transforms back. Returns \code{NULL} on a non-finite matrix, a nonpositive
#' or non-finite diagonal scale, a normalized \code{rcond} below
#' \code{rcond_tol}, or a failed Cholesky, so SE availability tracks acceptance
#' of a column-rescaled fit -- do NOT simplify the gate to a raw
#' \code{rcond(m)}, which spuriously rejects a rescaled-but-fine bread.
#'
#' @param m Symmetric numeric matrix to invert
#' @param rcond_tol Positive scalar reciprocal-condition tolerance
#'
#' @return The inverse of \code{m} with \code{m}'s dimnames, or \code{NULL}
#' @keywords internal
se_norm_inv <- function(m, rcond_tol) {
  if (!all(is.finite(m))) {
    return(NULL)
  }
  # gate the diagonal before sqrt: a negative entry reaches the same NULL
  # either way, but sqrt() would emit a spurious NaN warning first
  d <- diag(m)
  if (any(!is.finite(d)) || any(d <= 0)) {
    return(NULL)
  }
  d <- sqrt(d)
  ms <- m / tcrossprod(d)
  if (!all(is.finite(ms)) || rcond(ms) < rcond_tol) {
    return(NULL)
  }
  ch <- tryCatch(chol(ms), error = function(cond) NULL)
  if (is.null(ch)) {
    return(NULL)
  }
  inv <- chol2inv(ch) / tcrossprod(d)
  dimnames(inv) <- dimnames(m)
  inv
}

#' Bartlett/Newey-West HAC Meat of a Score Matrix
#'
#' The outer-product meat \code{crossprod(scores)} plus the
#' triangular-weighted lag autocovariances out to \code{hac_lags}. Rows must be
#' in chronological order. \code{hac_lags = 0} returns the plain
#' outer-product meat.
#'
#' @param scores Numeric matrix of per-observation score rows
#' @param hac_lags Nonnegative integer lag truncation
#'
#' @return A \code{ncol(scores)} square numeric matrix
#' @keywords internal
se_bartlett_meat <- function(scores, hac_lags) {
  meat <- crossprod(scores)
  n <- nrow(scores)
  for (l in seq_len(hac_lags)) {
    if (l >= n) break
    gamma_l <- crossprod(
      scores[(l + 1L):n, , drop = FALSE], scores[1:(n - l), , drop = FALSE]
    )
    meat <- meat + (1 - l / (hac_lags + 1L)) * (gamma_l + t(gamma_l))
  }
  meat
}

#' Validate SE Inputs and Build the All-NA Skeleton
#'
#' Estimator-neutral prologue: reports the dimensions, the canonical all-NA
#' result keyed by \code{se_types}, and -- only when the inputs are usable --
#' the fitted mean \eqn{\mu = \exp(X \theta)}.
#'
#' @param coef Numeric coefficient vector, or \code{NULL} on a failed fit
#' @param y Numeric response on the same scale \code{coef} was fitted on
#' @param x_mat Numeric design matrix, intercept column included
#' @param hac_lags Nonnegative integer lag truncation
#' @param se_types Character vector of variant names
#'
#' @return List with \code{ok}, \code{n}, \code{p}, \code{hac_lags},
#'   \code{na_mat}, \code{na_out}, and \code{mu} when \code{ok}
#' @keywords internal
se_preflight <- function(coef, y, x_mat, hac_lags, se_types) {
  hac_lags <- as.integer(hac_lags)
  n <- nrow(x_mat)
  p <- ncol(x_mat)
  coef_names <- colnames(x_mat)
  na_mat <- matrix(NA_real_, p, p, dimnames = list(coef_names, coef_names))
  out <- list(
    ok = FALSE, n = n, p = p, hac_lags = hac_lags, na_mat = na_mat,
    na_out = stats::setNames(rep(list(na_mat), length(se_types)), se_types)
  )
  if (!se_inputs_ok(coef, y, x_mat, n, p)) {
    return(out)
  }
  mu <- exp(drop(x_mat %*% coef))
  if (any(!is.finite(mu)) || any(mu <= 0)) {
    return(out)
  }
  out$ok <- TRUE
  out$mu <- mu
  out
}

#' Are the SE Inputs Usable?
#'
#' The preflight's validity chain, split out to keep each piece readable. The
#' order is load-bearing and short-circuits: the type check must run before the
#' finiteness check, since \code{is.finite()} errors on a character vector.
#'
#' @param coef,y,x_mat,n,p As in \code{\link{se_preflight}}
#'
#' @return \code{TRUE} when every input is usable
#' @noRd
se_inputs_ok <- function(coef, y, x_mat, n, p) {
  n > p &&
    is.numeric(x_mat) && all(is.finite(x_mat)) &&
    se_vector_ok(coef, p) &&
    se_vector_ok(y, n) && all(y >= 0)
}

#' Is This a Finite Numeric Vector of the Expected Length?
#'
#' @param v Candidate vector
#' @param len Required length
#'
#' @return \code{TRUE} when \code{v} is numeric, of length \code{len}, finite
#' @noRd
se_vector_ok <- function(v, len) {
  is.numeric(v) && length(v) == len && all(is.finite(v))
}
