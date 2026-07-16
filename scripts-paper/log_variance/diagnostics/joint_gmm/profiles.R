# Profiled intercepts for the joint log-variance system (joint-GMM,
# logvar-joint-gmm): the closed-form mean-log intercept a_L(b, beta), the
# log-sum-exp-stable PPML intercept a_P(b, beta), and their spelled-out
# softmax-weighted derivatives. Substituting the profiled intercepts collapses
# the log/PPML blocks to the slope system, so the unprofiled Jacobian is not
# enough -- the derivatives below are FD-tested on the profiled system itself.
# R = x_mat[, -1] is the slope design and e = drop(w1 - w2 %*% b) the residual.
# a_P uses log-sum-exp arithmetic throughout (never a floor); an all-zero
# response is a structural degeneracy and fails closed. Definitions only;
# sourced by the joint-GMM test entrypoint and the search/projection driver.

# Overflow-stable log-sum-exp m + log(sum(exp(v - m))). A nonfinite maximum is
# returned as is, so an all -Inf or any +Inf input never forms Inf - Inf.
logvar_logsumexp <- function(v) {
  m <- max(v)
  if (!is.finite(m)) {
    return(m)
  }
  m + log(sum(exp(v - m)))
}

# Profiled log intercept: the mean of log(e^2) - R beta, which drives the log
# block's intercept moment to zero exactly.
logvar_profile_a_L <- function(b, beta, w1, w2, x_mat) {
  e <- drop(w1 - w2 %*% b)
  r_mat <- x_mat[, -1L, drop = FALSE]
  mean(log(e^2) - drop(r_mat %*% beta))
}

# Profiled PPML intercept: log(sum e^2) - logsumexp(R beta), the log-sum-exp
# form of log(sum y / sum exp(R beta)). An all-zero response has no positive
# variance to match: structurally degenerate, so it fails closed, never floored.
logvar_profile_a_P <- function(b, beta, w1, w2, x_mat) {
  e <- drop(w1 - w2 %*% b)
  y <- e^2
  if (all(y == 0)) {
    stop("logvar_profile_a_P: all-zero response is structurally degenerate")
  }
  r_mat <- x_mat[, -1L, drop = FALSE]
  log(sum(y)) - logvar_logsumexp(drop(r_mat %*% beta))
}

# Spelled-out profiled intercept derivatives: da_P/dbeta is a normalized
# softmax-weighted column sum (the weights sum to one, so colSums not colMeans
# -- the accidental extra 1/n is the classic slip). K = ncol(w2),
# R = x_mat[, -1], n = length(e).
logvar_profile_jacobian <- function(b, beta, w1, w2, x_mat) {
  e <- drop(w1 - w2 %*% b)
  n <- length(e)
  r_mat <- x_mat[, -1L, drop = FALSE]
  lp <- drop(r_mat %*% beta)
  wts <- exp(lp - logvar_logsumexp(lp))
  list(
    da_L_db = -(2 / n) * colSums(w2 / e),
    da_L_dbeta = -colMeans(r_mat),
    da_P_db = drop(-2 * crossprod(w2, e) / sum(e^2)),
    da_P_dbeta = -colSums(wts * r_mat)
  )
}
