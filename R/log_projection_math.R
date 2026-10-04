# Stable arithmetic shared by the log-projection methods. Logs of squared
# residuals are formed as 2 * log(abs(e)), so an exact zero maps to -Inf
# without squaring first; scales are taken from max-abs rescaled columns so
# neither squares nor sums overflow or underflow.

# column-wise stat(); a single column skips apply(), which bootstrap fits
# call thousands of times on one-column candidates, with apply()'s names
log_projection_col_stat <- function(x, stat) {
  if (ncol(x) == 1L) stats::setNames(stat(x), colnames(x)) else apply(x, 2L, stat)
}

# column-wise log(mean(x^2)); -Inf for a zero column, NaN never
log_mean_square_cols <- function(x) {
  x <- as.matrix(x)
  u <- log_projection_col_stat(abs(x), max)
  out <- rep(-Inf, ncol(x))
  live <- u > 0
  out[live] <- 2 * log(u[live]) +
    log(colMeans((x[, live, drop = FALSE] / rep(u[live], each = nrow(x)))^2))
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

# log(exp(u) + exp(v)) elementwise; at most one argument is -Inf (callers
# guarantee a finite threshold or adjustment)
log_add_exp <- function(u, v) {
  m <- pmax(u, v)
  m + log1p(exp(-abs(u - v)))
}

# column-wise log(sum(exp(z))) after subtracting each column's maximum
log_sum_exp_cols <- function(z) {
  m <- log_projection_col_stat(z, max)
  m + log(colSums(exp(z - rep(m, each = nrow(z)))))
}

# Fuller transform F(x, delta) = log(x + delta) - delta / (x + delta) from
# log x and log delta, with A = log(x + delta) and rho = delta / (x + delta)
log_projection_fuller_transform <- function(log_x, log_delta) {
  a <- log_add_exp(log_x, log_delta)
  rho <- exp(log_delta - a)
  list(value = a - rho, a = a, rho = rho)
}

# d g / d e for g = log(e^2 + delta) (rho NULL) or the Fuller transform:
# sign(e) * 2 |e| / (e^2 + delta) * (1 + rho), with the explicit zero
# branch the composite map's limit requires
log_projection_resid_deriv <- function(e, log_abs_e, a, rho = NULL) {
  extra <- if (is.null(rho)) 0 else log1p(rho)
  d <- sign(e) * exp(log(2) + log_abs_e - a + extra)
  d[e == 0] <- 0
  d
}
