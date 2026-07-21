#' Statistics Computation Utilities
#'
#' Higher-order functions for per-maturity statistics computation
#'
#' @name statistics_utils
#' @keywords internal
NULL

#' Compute Per-Maturity Statistics
#'
#' Applies a computation function to each maturity, returning a named
#' list of results. Trusts already-validated inputs: callers must
#' first run \code{validate_statistics_inputs()} (the exported
#' statistics wrappers and \code{compute_identification_moments()} do
#' this once before delegating to the internal workers).
#'
#' @param w1 Numeric vector of W1 residuals
#' @param w2 Numeric matrix of W2 residuals (T x I)
#' @param maturities Vector of validated maturity indices
#' @param compute_fn Function called for each maturity with args
#'   (w1, w2, w2_i, idx, i, ...)
#' @param ... Extra arguments forwarded to compute_fn
#' @return Named list of per-maturity results from compute_fn, one element per
#'   entry of \code{maturities} in order, named \code{maturity_N} where
#'   \code{N} is the w2 column index (\code{maturity_names()}), so element
#'   \code{k} corresponds to \code{maturities[k]}.
#' @keywords internal
compute_per_maturity <- function(w1, w2, maturities,
                                 compute_fn, ...) {
  results <- lapply(
    seq_along(maturities),
    function(idx) {
      i <- maturities[idx]
      compute_fn(
        w1 = w1, w2 = w2, w2_i = w2[, i],
        idx = idx, i = i, ...
      )
    }
  )
  names(results) <- maturity_names(maturities)
  results
}

#' Centered Sample Covariance (1/T normalization)
#'
#' Computes the centered sample covariance between the columns of two
#' conformable inputs (vectors or matrices), using the \eqn{1/T}
#' normalization of the sample analog in Lewbel multivariate set
#' identification (centered cov/var; see the package spec sections on
#' moment notation and centering), \strong{not} the \eqn{1/(T-1)}
#' convention of [stats::cov()]. For inputs \eqn{A} (T x a) and
#' \eqn{B} (T x b),
#' \deqn{\widehat{\mathrm{Cov}}(A, B) = \frac{1}{T} (A - \bar{A})^\top
#'   (B - \bar{B}) \in \mathbb{R}^{a \times b},}
#' where \eqn{\bar{A}} and \eqn{\bar{B}} are the column means. Both
#' inputs are centered before the cross product, which computes the
#' same quantity as the one-pass formula
#' \eqn{A^\top B / T - \bar{A} \bar{B}^\top} but without its
#' catastrophic cancellation when column means dominate the spread.
#'
#' @param a Numeric vector or matrix (T x a).
#' @param b Numeric vector or matrix (T x b).
#' @return An \eqn{a \times b} matrix of centered covariances.
#' @keywords internal
centered_cov <- function(a, b) {
  a <- as.matrix(a)
  b <- as.matrix(b)
  a_centered <- sweep(a, 2, colMeans(a))
  b_centered <- sweep(b, 2, colMeans(b))
  crossprod(a_centered, b_centered) / nrow(a)
}

#' Centered Sample Variance (1/T normalization)
#'
#' Scalar diagonal of \code{\link{centered_cov}}: the 1/T centered
#' variance of a single numeric vector, sharing its centering and
#' divisor.
#'
#' @param x Numeric vector.
#' @return Numeric scalar centered variance.
#' @noRd
centered_var <- function(x) {
  centered_cov(x, x)[1, 1]
}

#' Guarded Centered Variance for Bound Arms
#'
#' The shared overflow policy of the variance-bound arms: a series that
#' is not entirely finite yields \code{Inf} (the arm loses the min it
#' feeds; observations are never dropped, which would understate a
#' variance bound), and a finite series yields its
#' \code{\link{centered_var}} clamped at zero (divisor-N arithmetic can
#' produce a tiny negative value on a near-constant series, and callers
#' take square roots). Non-finite inputs here arise only from
#' \code{exp()} overflow at astronomically large log prices, so the
#' \code{Inf} branch errs conservative, never sharp.
#'
#' @param x Numeric vector.
#' @return Numeric scalar: \code{max(0, centered_var(x))}, or \code{Inf}
#'   when any element of \code{x} is non-finite.
#' @noRd
guarded_centered_var <- function(x) {
  if (!all(is.finite(x))) {
    return(Inf)
  }
  max(0, centered_var(x))
}

#' Warn When Identification Variances Are Degenerate
#'
#' Diagnostic for the regularity assumption of the identification
#' strategy: \eqn{var(W_{2,i}^2) > 0} and
#' \eqn{var(W_1 W_{2,i} - \gamma W_{2,i}^2) > 0} at the true
#' \eqn{\gamma}. The first is checked directly; the second is checked
#' at the \eqn{\gamma} that minimizes it (the residual variance of
#' regressing \eqn{W_1 W_{2,i}} on \eqn{W_{2,i}^2}), so a warning means
#' the condition fails for some \eqn{\gamma}. Both checks are
#' scale-free ratios compared against
#' \code{HETID_CONSTANTS$DEGENERACY_TOLERANCE}. The relevant variances
#' \eqn{var(W_{2,i}^2)} and \eqn{var(W_1 W_{2,i})} are exactly the
#' scalar statistics \code{sigma_i_sq} and \code{s_i_0}, so the caller
#' passes them in and the diagnostic judges the same numbers the
#' \code{hetid_moments} container carries. Degenerate variances make
#' the quadratic constraint ill-defined and the identified set
#' degenerate or unbounded, so surfacing them here catches the problem
#' at the moments stage instead of downstream.
#'
#' @param w1 Numeric vector of W1 residuals
#' @param w2 Numeric matrix of W2 residuals (T x I)
#' @param maturities Integer vector of w2 column indices to check
#' @param sigma_i_sq Numeric vector of sigma_i^2 statistics, element k
#'   corresponding to \code{maturities[k]}
#' @param s_i_0 Numeric vector of S_i^(0) statistics, element k
#'   corresponding to \code{maturities[k]}
#' @return Invisible NULL, called for its warning side effect
#' @keywords internal
warn_if_variance_degenerate <- function(w1, w2, maturities,
                                        sigma_i_sq, s_i_0) {
  tol <- HETID_CONSTANTS$DEGENERACY_TOLERANCE

  first <- logical(length(maturities))
  second <- logical(length(maturities))
  for (k in seq_along(maturities)) {
    w2_i <- w2[, maturities[k]]
    w2_sq <- w2_i^2
    prod_i <- w1 * w2_i
    v_w2_sq <- sigma_i_sq[[k]]
    first[k] <- isTRUE(v_w2_sq <= tol * centered_var(w2_i)^2)
    v_prod <- s_i_0[[k]]
    resid_var <- v_prod
    if (isTRUE(v_w2_sq > 0)) {
      resid_var <- v_prod -
        centered_cov(prod_i, w2_sq)[1, 1]^2 / v_w2_sq
    }
    second[k] <- isTRUE(resid_var <= tol * v_prod)
  }

  flag_msg <- function(flags, label) {
    if (!any(flags)) {
      return(NULL)
    }
    paste0(
      label, " for maturity ",
      paste(maturities[flags], collapse = ", ")
    )
  }
  msgs <- c(
    flag_msg(first, "var(W2^2) is numerically degenerate"),
    flag_msg(second, "var(W1*W2 - gamma*W2^2) is numerically degenerate")
  )
  if (length(msgs) > 0) {
    warn_degenerate_variance(paste0(
      "Variance positivity diagnostic: ",
      paste(msgs, collapse = "; "),
      ". The identification regularity conditions may fail and the ",
      "identified set may be degenerate or unbounded."
    ))
  }
  invisible(NULL)
}
