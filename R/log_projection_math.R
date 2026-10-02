# Stable arithmetic shared by the log-projection methods. Logs of squared
# residuals are formed as 2 * log(abs(e)), so an exact zero maps to -Inf
# without squaring first; scales are taken from max-abs rescaled columns so
# neither squares nor sums overflow or underflow.

# column-wise log(mean(x^2)); -Inf for a zero column, NaN never
log_mean_square_cols <- function(x) {
  x <- as.matrix(x)
  u <- apply(abs(x), 2L, max)
  out <- rep(-Inf, ncol(x))
  live <- u > 0
  out[live] <- 2 * log(u[live]) +
    log(colMeans(sweep(x[, live, drop = FALSE], 2L, u[live], "/")^2))
  out
}

# TRUE when every column's mean is negligible relative to its root mean
# square, both computed after a max-abs rescale; a zero column passes
log_projection_mean_zero <- function(x, tol) {
  x <- as.matrix(x)
  all(vapply(seq_len(ncol(x)), function(j) {
    u <- max(abs(x[, j]))
    u == 0 || abs(mean(x[, j] / u)) <= tol * sqrt(mean((x[, j] / u)^2))
  }, logical(1)))
}
