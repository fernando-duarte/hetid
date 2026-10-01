#' Shared Log-Variance Covariance Calculations
#'
#' Shared helpers invert diagonally normalized information matrices, sum
#' Bartlett-weighted score cross-products, and check inputs for covariance
#' evaluation. Estimator modules assemble their own covariance variants.
#'
#' These internals assume valid argument types and shapes. Use the public
#' entrypoints
#' \code{\link{compute_log_variance_vcov}} and
#' \code{\link{compute_log_variance_vcov_at_coef}} for covariance evaluation.
#' Failed matrix-inversion and preflight checks return \code{NULL} and the
#' all-NA skeleton, respectively.
#'
#' @name log_variance_se_utils
#' @keywords internal
NULL

#' Invert a Symmetric Bread Through the Conditioning Gate
#'
#' Normalizes by the diagonal, gates \code{rcond}, Cholesky-inverts, and
#' transforms back. Returns \code{NULL} on a non-finite matrix, a nonpositive
#' or non-finite diagonal scale, a normalized \code{rcond} below
#' \code{rcond_tol}, or a failed Cholesky factorization. The conditioning
#' check uses diagonal scaling rather than the raw \code{rcond(m)}.
#'
#' @param m Square symmetric numeric matrix to invert.
#' @param rcond_tol Finite positive scalar reciprocal-condition tolerance.
#'
#' @return The inverse of \code{m} with \code{m}'s dimnames, or \code{NULL}
#'   when the matrix fails the finiteness, conditioning, or Cholesky checks.
#' @keywords internal
se_norm_inv <- function(m, rcond_tol) {
  if (!all(is.finite(m))) {
    return(NULL)
  }
  # check the diagonal before sqrt() to avoid warnings from negative entries
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
#' Scores are not centered, and cross-products are not divided by the sample
#' size. Lags at or beyond \code{nrow(scores)} contribute no cross-products;
#' the supplied bandwidth still determines the Bartlett weights.
#'
#' @param scores Finite numeric matrix of per-observation score rows.
#' @param hac_lags Single nonnegative integer lag truncation, measured in rows.
#'
#' @return A \code{ncol(scores)} square numeric matrix, with score column names
#'   on both axes when present.
#' @keywords internal
se_bartlett_meat <- function(scores, hac_lags) {
  meat <- crossprod(scores)
  n <- nrow(scores)
  for (l in seq_len(hac_lags)) {
    if (l >= n) break
    gamma_l <- crossprod(
      scores[(l + 1L):n, , drop = FALSE], scores[1:(n - l), , drop = FALSE]
    )
    meat <- meat + (1 - l / (hac_lags + 1)) * (gamma_l + t(gamma_l))
  }
  meat
}

#' Validate SE Inputs and Build the All-NA Skeleton
#'
#' Returns the dimensions and covariance matrices filled with \code{NA},
#' keyed by \code{se_types}. Usable inputs also return the fitted mean
#' \eqn{\mu = \exp(X \theta)}.
#'
#' Usable inputs require more rows than design columns, finite numeric
#' coefficients, responses, and design entries, nonnegative responses, and
#' finite strictly positive fitted means. Missing values are not removed:
#' any failed check returns \code{ok = FALSE} and the all-NA skeleton.
#'
#' @param coef Numeric coefficient vector of length \code{ncol(x_mat)}, or
#'   \code{NULL} on a failed fit.
#' @param y Numeric response vector of length \code{nrow(x_mat)}, on the same
#'   scale \code{coef} was fitted on. Zero responses are allowed.
#' @param x_mat Numeric design matrix, including any fitted intercept column.
#'   No intercept is added.
#' @param hac_lags Single nonnegative integer lag truncation, measured in rows.
#' @param se_types Character vector of covariance variant names.
#'
#' @return A list containing logical \code{ok}, dimensions \code{n} and
#'   \code{p}, integer \code{hac_lags}, an all-NA \code{p x p} matrix
#'   \code{na_mat}, and \code{na_out}, a list of those matrices named by
#'   \code{se_types}. Matrix axes use \code{colnames(x_mat)}. When
#'   \code{ok = TRUE}, the list also contains the length-\code{n} fitted-mean
#'   vector \code{mu}; otherwise \code{mu} is absent.
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
#' Checks types before finiteness because \code{is.finite()} errors on
#' character vectors. Later checks run only when earlier ones pass.
#'
#' @param coef,y,x_mat Inputs as in \code{\link{se_preflight}}.
#' @param n,p Numbers of design rows and columns, respectively.
#'
#' @return \code{TRUE} when every input is usable, otherwise \code{FALSE}.
#' @noRd
se_inputs_ok <- function(coef, y, x_mat, n, p) {
  n > p &&
    is.numeric(x_mat) && all(is.finite(x_mat)) &&
    se_vector_ok(coef, p) &&
    se_vector_ok(y, n) && all(y >= 0)
}

#' Is This a Finite Numeric Vector of the Expected Length?
#'
#' @param v Candidate numeric vector.
#' @param len Required length.
#'
#' @return \code{TRUE} when \code{v} is numeric, of length \code{len}, and
#'   finite, otherwise \code{FALSE}.
#' @noRd
se_vector_ok <- function(v, len) {
  is.numeric(v) && length(v) == len && all(is.finite(v))
}
