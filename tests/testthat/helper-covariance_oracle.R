# Independent solve() and observation-pair oracle for covariance tests
covariance_fixture <- function() {
  x <- cbind("(Intercept)" = 1, v = c(-2, -1, 0, 1, 2, 3))
  list(x = x, coef = c(0.2, -0.1), y = c(0, 2, 1, 3, 1, 4))
}

covariance_pair_meat <- function(scores, lag) {
  n <- nrow(scores)
  weights <- pmax(1 - abs(outer(seq_len(n), seq_len(n), "-")) / (lag + 1), 0)
  crossprod(scores, weights %*% scores)
}

covariance_oracle <- function(d, estimator, lag) {
  x <- d$x
  y <- d$y
  mu <- exp(drop(x %*% d$coef))
  if (estimator == "ppml") {
    inv <- solve(crossprod(x, mu * x))
    u <- x * (y - mu)
    hc0 <- inv %*% crossprod(u) %*% inv
    return(list(
      naive = sum((y - mu)^2 / mu) / (nrow(x) - ncol(x)) * inv,
      hc0 = hc0,
      hc1 = nrow(x) / (nrow(x) - ncol(x)) * hc0,
      hac = inv %*% covariance_pair_meat(u, lag) %*% inv
    ))
  }
  ratio <- y / mu
  g <- 0.5 * (1 - ratio) * x
  h <- solve(0.5 * crossprod(x, ratio * x))
  list(
    expected = solve(0.5 * crossprod(x)),
    observed = h,
    opg = solve(crossprod(g)),
    robust = h %*% crossprod(g) %*% h,
    hac = h %*% covariance_pair_meat(g, lag) %*% h
  )
}
