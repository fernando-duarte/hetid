# The Harvey multiplicative-variance math primitives: the zero-safe ratio
# r = y / exp(eta), the fitted-variance helper, the Gaussian negative
# log-likelihood objective and score, the moment-convention first-order
# condition (exactly minus twice the score, so joint-GMM can stack it against
# PPML's X'(y - mu) without re-deriving signs), the observed information, the
# exact normal log-chi-square gap constant, and the Jacobian row factor e / mu.
# Every evaluation computes a zero-response row by its exact limit: allocate a
# zero vector, subset the positive or nonzero rows first, and only then
# exponentiate or divide, so an exact zero contributes x' theta / 2 to the
# criterion and never forms 0 * Inf or 0 / 0. Definitions only; sourced by
# solver.R and estimator.R before the response solve,
# the implicit Jacobian, and the estimator. No clamping, no epsilon on y, no eta
# capping -- a nonfinite positive-row ratio is the caller's hard trial failure,
# never a value this layer silences.

# Zero-safe ratio r = y / exp(x' theta). The evaluation order is contractual:
# validate a finite nonnegative response, form eta, mark the positive rows, seed
# r with zeros, and only then fill the positive rows on the log scale. Zero rows
# stay exact zeros without ever forming 0 * Inf; a nonfinite positive-row entry
# is left as is (Inf, never NaN) for the caller to treat as a hard trial failure.
logvar_harvey_ratio <- function(theta, y, x_mat) {
  if (!is.numeric(y) || length(y) != nrow(x_mat) || any(!is.finite(y)) ||
    any(y < 0)) {
    stop("y must be a finite nonnegative vector of length nrow(x_mat)")
  }
  eta <- drop(x_mat %*% theta)
  pos <- y > 0
  r <- numeric(length(y))
  r[pos] <- exp(log(y[pos]) - eta[pos])
  r
}

# Fitted conditional variance mu = exp(x' theta). A pure helper for acceptance
# and the Jacobian; acceptance requires every value finite and strictly
# positive, but that gate lives with the caller, never here.
logvar_harvey_mu <- function(theta, x_mat) {
  exp(drop(x_mat %*% theta))
}

# Gaussian negative log-likelihood Q_H = 0.5 * (sum(eta) + sum(r on y > 0)); a
# zero row contributes only its eta / 2 term. A nonfinite positive-row ratio
# returns Inf (the scalar-trial convention the solver consumes), never a clamp.
logvar_harvey_objective <- function(theta, y, x_mat) {
  r <- logvar_harvey_ratio(theta, y, x_mat)
  eta <- drop(x_mat %*% theta)
  pos <- y > 0
  if (any(!is.finite(r[pos]))) {
    return(Inf)
  }
  0.5 * (sum(eta) + sum(r[pos]))
}

# Negative log-likelihood gradient 0.5 * X' (1 - r) through the zero-safe ratio;
# the solver drives it to zero and the finite-difference tests match it. Named
# by the coefficient columns, mirroring the PPML score primitive.
logvar_harvey_score <- function(theta, y, x_mat) {
  drop(0.5 * crossprod(x_mat, 1 - logvar_harvey_ratio(theta, y, x_mat)))
}

# Moment-convention first-order condition X' (r - 1): exactly minus twice the
# score, exposed with the FOC sign so joint-GMM stacks Harvey against PPML's
# X'(y - mu) without re-deriving factors.
logvar_harvey_moment <- function(theta, y, x_mat) {
  drop(crossprod(x_mat, logvar_harvey_ratio(theta, y, x_mat) - 1))
}

# Observed information 0.5 * X' diag(r) X (the Hessian of Q_H); the expected
# information 0.5 * X'X is trivial and not exported. One definition shared by the
# Jacobian, the rcond gate, and joint-GMM.
logvar_harvey_info <- function(theta, y, x_mat) {
  0.5 * crossprod(x_mat, logvar_harvey_ratio(theta, y, x_mat) * x_mat)
}

# The exact -E[log chi^2_1] = -log 2 - digamma(1/2) = 1.27036284546..., the
# Gaussian gap theta_0^H - theta_0^log. Single-sourced here so the notes, the
# ladder shift, metadata, and tests reference one value with no truncated literal.
logvar_normal_lnchisq_gap <- -log(2) - digamma(0.5)

# Zero-safe Jacobian row factor e / mu = sign(e) * exp(log|e| - eta), evaluated
# on the log scale so an exact-zero residual stays an exact zero without forming
# 0 / 0. Consumed by the implicit Jacobian's RHS row scaling (diag(1 / mu) e).
logvar_harvey_e_over_mu <- function(e, eta) {
  if (length(e) != length(eta)) {
    stop("e and eta must have the same length")
  }
  out <- numeric(length(e))
  nz <- e != 0
  out[nz] <- sign(e[nz]) * exp(log(abs(e[nz])) - eta[nz])
  out
}
