#' Statistics Computation Utilities
#'
#' Higher-order functions for per-maturity statistics computation
#'
#' @name statistics_utils
#' @keywords internal
NULL

#' Compute Per-Maturity Statistics
#'
#' Validates inputs and applies a computation function to each
#' maturity, returning a named list of results. Encapsulates the
#' validate-then-iterate pattern shared by the statistics functions.
#'
#' @param w1 Numeric vector of W1 residuals
#' @param w2 Matrix of W2 residuals (T x I)
#' @param maturities Vector of maturity indices (NULL for all)
#' @param compute_fn Function called for each maturity with args
#'   (w1, w2, w2_i, t_obs, idx, i, ...)
#' @param ... Extra arguments forwarded to compute_fn
#' @return Named list of per-maturity results from compute_fn
#' @keywords internal
compute_per_maturity <- function(w1, w2, maturities,
                                 compute_fn, ...) {
  validated <- validate_statistics_inputs(
    w1, w2, maturities
  )
  w2 <- validated$w2
  t_obs <- validated$t_obs
  maturities <- validated$maturities

  results <- lapply(
    seq_along(maturities),
    function(idx) {
      i <- maturities[idx]
      w2_i <- w2[, i]
      compute_fn(
        w1 = w1, w2 = w2, w2_i = w2_i,
        t_obs = t_obs, idx = idx, i = i, ...
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
#' \deqn{\widehat{\mathrm{Cov}}(A, B) = \frac{1}{T} A^\top B -
#'   \bar{A}\,\bar{B}^\top \in \mathbb{R}^{a \times b},}
#' where \eqn{\bar{A}} and \eqn{\bar{B}} are the column means. Centering
#' both arguments makes each entry a true covariance regardless of whether
#' the inputs were already centered.
#'
#' @param a Numeric vector or matrix (T x a).
#' @param b Numeric vector or matrix (T x b).
#' @param t_obs Number of observations T used for normalization.
#' @return An \eqn{a \times b} matrix of centered covariances.
#' @keywords internal
centered_cov <- function(a, b, t_obs) {
  a <- as.matrix(a)
  b <- as.matrix(b)
  crossprod(a, b) / t_obs - tcrossprod(colMeans(a), colMeans(b))
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
#' \code{HETID_CONSTANTS$DEGENERACY_TOLERANCE}. Degenerate variances
#' make the quadratic constraint ill-defined and the identified set
#' degenerate or unbounded, so surfacing them here catches the problem
#' at the moments stage instead of downstream.
#'
#' @param w1 Numeric vector of W1 residuals
#' @param w2 Matrix of W2 residuals (T x I)
#' @param maturities Integer vector of w2 column indices to check
#' @param t_obs Number of observations T
#' @return Invisible NULL, called for its warning side effect
#' @keywords internal
warn_if_variance_degenerate <- function(w1, w2, maturities, t_obs) {
  tol <- HETID_CONSTANTS$DEGENERACY_TOLERANCE
  var_of <- function(x) centered_cov(x, x, t_obs)[1, 1]

  first <- logical(length(maturities))
  second <- logical(length(maturities))
  for (k in seq_along(maturities)) {
    w2_i <- w2[, maturities[k]]
    w2_sq <- w2_i^2
    prod_i <- w1 * w2_i
    v_w2_sq <- var_of(w2_sq)
    first[k] <- v_w2_sq <= tol * var_of(w2_i)^2
    v_prod <- var_of(prod_i)
    resid_var <- v_prod
    if (v_w2_sq > 0) {
      resid_var <- v_prod -
        centered_cov(prod_i, w2_sq, t_obs)[1, 1]^2 / v_w2_sq
    }
    second[k] <- resid_var <= tol * v_prod
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
    warning(
      "Variance positivity diagnostic: ",
      paste(msgs, collapse = "; "),
      ". The identification regularity conditions may fail and the ",
      "identified set may be degenerate or unbounded.",
      call. = FALSE
    )
  }
  invisible(NULL)
}
