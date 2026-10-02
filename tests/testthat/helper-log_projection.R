# Synthetic mean system with intercept-regression residuals on the mean
# sample and a strict tail as the volatility sample, so the two samples differ
lp_fixture <- function(n_mean = 80L, n_vol = 60L, d_n = 2L, d_r = 2L,
                       seed = 1L) {
  set.seed(seed)
  z <- cbind(1, matrix(stats::rnorm(n_mean * 2L), n_mean, 2L))
  news <- matrix(stats::rnorm(n_mean * d_n), n_mean, d_n,
    dimnames = list(NULL, paste0("n", seq_len(d_n)))
  )
  y <- drop(news %*% seq_len(d_n) / d_n) + stats::rnorm(n_mean)
  w2 <- as.matrix(stats::lm.fit(z, news)$residuals)
  colnames(w2) <- colnames(news)
  ids <- 1000L + seq_len(n_mean)
  x_var <- matrix(stats::rnorm(n_vol * d_r, mean = 3), n_vol, d_r,
    dimnames = if (d_r > 0L) list(NULL, paste0("pc", seq_len(d_r))) else NULL
  )
  list(
    w1 = drop(stats::lm.fit(z, y)$residuals), w2 = w2, z = z, y = y,
    news = news, x_var = x_var, mean_ids = ids,
    vol_ids = utils::tail(ids, n_vol), b = rep(0.4, d_n)
  )
}

lp_prep <- function(fx) {
  prepare_log_projection(fx$w1, fx$w2, fx$x_var, fx$mean_ids, fx$vol_ids)
}

# Hand-built integer system whose residual at b = 0 has exact zeros
lp_zero_fixture <- function() {
  set.seed(3)
  list(
    w1 = rep(c(-2, -1, 0, 1, 2, 0), 2L),
    w2 = cbind(n1 = rep(c(1, -1), 6L)),
    x_var = cbind(pc1 = stats::rnorm(12L)),
    mean_ids = seq_len(12L), vol_ids = seq_len(12L)
  )
}

# Central differences of a coefficient map at one relative step
lp_fd_jacobian <- function(f, b, step) {
  vapply(seq_along(b), function(k) {
    h <- step * max(1, abs(b[[k]]))
    up <- b
    dn <- b
    up[[k]] <- up[[k]] + h
    dn[[k]] <- dn[[k]] - h
    (f(up) - f(dn)) / (2 * h)
  }, numeric(length(f(b))))
}

# Smallest relative error of an analytic Jacobian over several FD steps
lp_fd_error <- function(f, jac, b, steps = c(1e-4, 1e-5, 1e-6)) {
  min(vapply(steps, function(s) {
    max(abs(jac - lp_fd_jacobian(f, b, s))) / max(1, max(abs(jac)))
  }, numeric(1)))
}

# Design with intercept for oracle fits on the centered volatility regressors
lp_design <- function(prep) {
  cbind(1, prep$x_centered)
}
