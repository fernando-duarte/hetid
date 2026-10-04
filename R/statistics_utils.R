#' Statistics Computation Utilities
#'
#' Helpers for per-maturity statistics, centered moments, and variance diagnostics.
#'
#' @name statistics_utils
#' @keywords internal
NULL

#' Compute Per-Maturity Statistics
#'
#' Applies a computation function to each maturity, returning a named
#' list of results.
#'
#' @details
#' Trusts already-validated inputs: callers must
#' first run \code{validate_statistics_inputs()} (the exported
#' statistics wrappers and \code{compute_identification_moments()} do
#' this once before delegating to the internal workers).
#'
#' @param w1 Numeric vector of \eqn{\omega_1} residuals of length T.
#' @param w2 Numeric matrix of \eqn{\omega_2} residuals (T x I).
#' @param maturities Vector of validated w2 column indices (the constraint axis).
#' @param compute_fn Function called with named arguments \code{w1}, \code{w2},
#'   \code{w2_i} (column \code{i}), \code{idx} (position in \code{maturities}),
#'   \code{i} (w2 column index), and \code{...}.
#' @param ... Extra arguments forwarded to \code{compute_fn}.
#' @return Named list of per-maturity results from \code{compute_fn}, one element per
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

#' Centered Sample Covariance (1/T Normalization)
#'
#' Computes the centered sample covariance between the columns of two
#' conformable inputs (vectors or matrices), using the \eqn{1/T}
#' normalization of the sample analog in Lewbel multivariate set
#' identification (centered cov/var; see the sample-implementation section of
#' the package spec), \strong{not} the \eqn{1/(T-1)}
#' convention of \code{\link[stats:cov]{stats::cov()}}.
#'
#' @details
#' For inputs \eqn{A} (T x a) and
#' \eqn{B} (T x b),
#' \deqn{\widehat{\mathrm{Cov}}(A, B) = \frac{1}{T} (A - \bar{A})^\top
#'   (B - \bar{B}) \in \mathbb{R}^{a \times b},}
#' where \eqn{\bar{A}} and \eqn{\bar{B}} are the column means. Both
#' inputs are centered before the cross product, which computes the
#' same quantity as the one-pass formula
#' \eqn{A^\top B / T - \bar{A} \bar{B}^\top} but without its
#' catastrophic cancellation when column means dominate the spread.
#' Inputs must have the same row count; vectors are treated as
#' one-column matrices. Inputs are not validated and observations are not
#' removed: missing values propagate to affected covariance entries.
#'
#' @param a Numeric vector or matrix (T x a).
#' @param b Numeric vector or matrix (T x b).
#' @return An \eqn{a \times b} numeric matrix of centered covariances, with
#'   row and column names taken from the columns of \code{a} and \code{b}.
#'   A single finite observation gives zeros; an empty sample gives \code{NaN} entries.
#' @keywords internal
centered_cov <- function(a, b) {
  a <- as.matrix(a)
  b <- as.matrix(b)
  a_centered <- a - rep(colMeans(a), each = nrow(a))
  b_centered <- if (identical(a, b)) a_centered else b - rep(colMeans(b), each = nrow(b))
  crossprod(a_centered, b_centered) / nrow(a)
}

#' Centered Sample Variance (1/T Normalization)
#'
#' Scalar diagonal of \code{\link{centered_cov}}: the 1/T centered
#' variance of a single numeric vector, sharing its centering and
#' divisor.
#' Missing values propagate; observations are not removed.
#'
#' @param x Numeric vector.
#' @return Numeric scalar centered variance, or \code{NaN} for an empty vector.
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
#' \code{centered_var()} clamped at zero before callers take square roots.
#' Non-finite inputs can arise from overflow in the bound components;
#' the \code{Inf} branch keeps the arm conservative.
#'
#' @param x Numeric vector.
#' @return Numeric scalar: \code{max(0, centered_var(x))}, or \code{Inf}
#'   when any element of \code{x} is non-finite. Empty inputs give \code{NaN}.
#' @noRd
guarded_centered_var <- function(x) {
  if (!all(is.finite(x))) {
    return(Inf)
  }
  max(0, centered_var(x))
}

#' Warn When Identification Variances Are Degenerate
#'
#' Checks identification variances for numerical degeneracy and warns when flagged.
#'
#' @details
#' Screens \eqn{var(\omega_{2,i}^2)} and the minimum of
#' \eqn{var(\omega_1 \omega_{2,i} - a \omega_{2,i}^2)} over scalar \eqn{a}.
#' The minimum is the residual variance from regressing
#' \eqn{\omega_1 \omega_{2,i}} on \eqn{\omega_{2,i}^2}.
#' Here \eqn{a} is a projection coefficient, called gamma in the warning text,
#' not necessarily the structural coefficient \eqn{\theta_i}. Both checks are
#' scale-free ratios compared against
#' \code{HETID_CONSTANTS$DEGENERACY_TOLERANCE}. The relevant variances
#' \eqn{var(\omega_{2,i}^2)} and \eqn{var(\omega_1 \omega_{2,i})} are exactly the
#' scalar statistics \code{sigma_i_sq} and \code{s_i_0}, so the caller
#' passes them in and the diagnostic judges the same numbers the
#' \code{hetid_moments} container carries. Degenerate variances may make
#' the identified set degenerate or unbounded, so surfacing them here catches the problem
#' at the moments stage instead of downstream.
#' Inputs must already be validated and aligned; this helper does not
#' remove observations or validate the supplied statistics. It emits at most
#' one warning of class \code{hetid_warning_degenerate_variance}, naming
#' the affected w2 column indices and checks.
#'
#' @param w1 Numeric vector of \eqn{\omega_1} residuals of length T.
#' @param w2 Numeric matrix of \eqn{\omega_2} residuals (T x I).
#' @param maturities Integer vector of w2 column indices to check.
#' @param sigma_i_sq Numeric vector of \code{sigma_i_sq} statistics, element k
#'   corresponding to \code{maturities[k]}.
#' @param s_i_0 Numeric vector of \code{s_i_0} statistics, element k
#'   corresponding to \code{maturities[k]}.
#' @return Invisible \code{NULL}, called for its warning side effect.
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
    flag_msg(first, "var(omega2^2) is numerically degenerate"),
    flag_msg(second, "var(omega1*omega2 - gamma*omega2^2) is numerically degenerate")
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
