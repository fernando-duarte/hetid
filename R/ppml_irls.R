#' Lean IRLS for an Uneventful PPML Solve
#'
#' Fisher scoring for the quasi-Poisson log-link mean from a supplied start,
#' with each weighted least-squares step solved by \code{stats::.lm.fit()}.
#' It follows \code{glm.fit()}'s conventions (working weights
#' \code{sqrt(mu^2 / mu)}, least-squares tolerance \code{min(1e-7, epsilon / 1000)},
#' and the relative deviance-change stopping rule), so an uneventful solve
#' returns \code{glm.fit()}'s coefficients and iteration count bit for bit
#' without its per-call family setup and post-fit summaries. Anything eventful
#' (a floored or nonfinite mean, an overflowing working weight, a nonfinite
#' deviance, rank loss, or no convergence within \code{maxit}) returns
#' \code{NULL}, and the caller falls back to \code{glm.fit()}, which handles it
#' with its warnings, step halving and boundary flag.
#'
#' @param x Finite numeric design matrix with named columns.
#' @param y Finite nonnegative numeric response vector, one entry per row.
#' @param start Numeric start vector, one element per design column.
#' @param epsilon,maxit Positive IRLS convergence tolerance and iteration cap.
#' @return \code{NULL}, or a list with named \code{coefficients},
#'   \code{converged = TRUE}, \code{boundary = FALSE} and integer \code{iter}:
#'   the \code{glm.fit()} fields the PPML ladder reads.
#' @noRd
ppml_irls <- function(x, y, start, epsilon, maxit) {
  floor_mu <- .Machine$double.eps
  tol <- min(1e-07, epsilon / 1000)
  pos <- which(y > 0)
  quasi_deviance <- function(mu) {
    r <- mu
    r[pos] <- (y * log(y / mu) - (y - mu))[pos]
    sum(2 * r)
  }
  usable <- function(mu) all(is.finite(mu)) && !any(mu <= floor_mu)
  theta <- start
  eta <- drop(x %*% theta)
  mu <- exp(eta)
  if (!usable(mu)) {
    return(NULL)
  }
  dev_old <- quasi_deviance(mu)
  for (iter in seq_len(maxit)) {
    z <- eta + (y - mu) / mu
    w <- sqrt(mu^2 / mu)
    # mu^2 can overflow for a finite mu; glm.fit() then errors in its
    # least-squares step, so leave that rung to it
    if (!all(is.finite(w)) || !all(is.finite(z))) {
      return(NULL)
    }
    wls <- stats::.lm.fit(x * w, z * w, tol = tol)
    if (wls$rank < ncol(x) || !all(is.finite(wls$coefficients))) {
      return(NULL)
    }
    theta[wls$pivot] <- wls$coefficients
    eta <- drop(x %*% theta)
    mu <- exp(eta)
    if (!usable(mu)) {
      return(NULL)
    }
    dev <- quasi_deviance(mu)
    if (!is.finite(dev)) {
      return(NULL)
    }
    if (abs(dev - dev_old) / (abs(dev) + 0.1) < epsilon) {
      names(theta) <- colnames(x)
      return(list(coefficients = theta, converged = TRUE, boundary = FALSE, iter = iter))
    }
    dev_old <- dev
  }
  NULL
}
