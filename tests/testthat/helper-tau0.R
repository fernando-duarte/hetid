# Shared tau=0 mean-equation DGP: E[e1 e2 | z] = 0 by construction, so the
# tau=0 point exists and recovers theta_true. x_var is a separate volatility
# design (not consumed here) kept for the later variance-equation tasks.
simulate_tau0_dgp <- function(t_obs = 400, seed = 42) {
  set.seed(seed)
  x <- cbind(x1 = rnorm(t_obs), x2 = rnorm(t_obs))
  z <- rnorm(t_obs)
  theta_true <- c(0.8, -0.5)
  e2 <- sqrt(exp(0.5 + 0.9 * z)) * matrix(rnorm(t_obs * 2), t_obs, 2)
  e1 <- rnorm(t_obs) # E[e1 e2 | z] = 0: tau = 0 holds
  y2 <- x %*% matrix(c(1, 0.5, -0.3, 0.7), 2, 2) + e2
  colnames(y2) <- c("news1", "news2")
  y1 <- drop(0.3 + x %*% c(0.2, -0.1) + y2 %*% theta_true + e1)
  # a separate volatility design: in the application the mean equation (SDF
  # PCs) and the volatility equation (lagged return PCs) use different
  # regressors, so the fixtures keep them distinct too
  x_var <- cbind(v1 = rnorm(t_obs), v2 = rnorm(t_obs))
  list(y1 = y1, y2 = y2, x = x, x_var = x_var, z = z, theta_true = theta_true)
}

# Three-news variant for the identified-set box. Two news columns leave no
# gridded plane at all, so the box search needs three to exercise the sweep.
# collinear = TRUE shares one factor across the news columns, which makes the
# Q stack ill-conditioned and the set a thin diagonal tube -- the geometry an
# axis-aligned grid misses and the search frame exists to handle.
simulate_box_dgp <- function(collinear = FALSE, t_obs = 200, seed = 42) {
  set.seed(seed)
  x <- cbind(x1 = rnorm(t_obs), x2 = rnorm(t_obs))
  z <- rnorm(t_obs)
  theta_true <- c(0.8, -0.5, 0.3)
  shared <- rnorm(t_obs)
  raw <- if (collinear) {
    cbind(
      shared + 0.15 * rnorm(t_obs), shared + 0.15 * rnorm(t_obs),
      shared + 0.15 * rnorm(t_obs)
    )
  } else {
    matrix(rnorm(t_obs * 3), t_obs, 3)
  }
  e2 <- sqrt(exp(0.5 + 0.9 * z)) * raw
  y2 <- x %*% matrix(c(1, 0.5, -0.3, 0.7, 0.2, 0.1), 2, 3) + e2
  colnames(y2) <- c("news1", "news2", "news3")
  y1 <- drop(0.3 + x %*% c(0.2, -0.1) + y2 %*% theta_true + rnorm(t_obs))
  x_var <- cbind(v1 = rnorm(t_obs), v2 = rnorm(t_obs))
  list(y1 = y1, y2 = y2, x = x, x_var = x_var, z = z, theta_true = theta_true)
}

# multiplicative chi-square response on exp(eta), the shape the log-variance
# equation feeds fit_log_variance; shared by Tasks 7 and 9-11
simulate_logvar_data <- function(t_obs = 300, seed = 7) {
  set.seed(seed)
  x <- cbind(v1 = rnorm(t_obs), v2 = rnorm(t_obs))
  eta <- drop(cbind(1, x) %*% c(-0.5, 0.6, -0.4))
  y <- exp(eta) * rchisq(t_obs, df = 1)
  list(y = y, x = x)
}
