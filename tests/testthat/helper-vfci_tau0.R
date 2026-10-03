vfci_tau0_frame <- function(n_obs = 300) {
  withr::local_seed(42)
  x1 <- rnorm(n_obs)
  x2 <- rnorm(n_obs)
  z <- rnorm(n_obs)
  h1 <- rnorm(n_obs)
  h2 <- rnorm(n_obs)
  x <- cbind(x1, x2)
  e2 <- sqrt(exp(0.5 + z %*% t(c(0.9, 0.4)))) * matrix(rnorm(2 * n_obs), n_obs, 2)
  news_values <- x %*% matrix(c(1, 0.5, -0.3, 0.7), 2, 2) + e2
  y <- 0.3 + x %*% c(0.2, -0.1) + news_values %*% c(0.8, -0.5) +
    sqrt(exp(0.8 * h1 - 0.3 * h2)) * rnorm(n_obs)
  observations <- data.frame(
    date = to_period_end(
      seq(as.Date("1950-01-01"), by = "3 months", length.out = n_obs), "quarterly"
    ),
    y = drop(y), x1 = x1, x2 = x2, news1 = news_values[, 1], news2 = news_values[, 2],
    z = z, h1 = h1, h2 = h2
  )
  observations$h1[1] <- NA_real_
  observations
}

vfci_tau0_test_fit <- function(data, ...) {
  fit_args <- list(
    data = data, y = "y", x = c("x1", "x2"), y2 = c("news1", "news2"), z = "z",
    het = c("h1", "h2"), date_begin = as.Date("1950-01-01"),
    date_end = as.Date("2024-12-31")
  )
  overrides <- list(...)
  fit_args[names(overrides)] <- overrides
  do.call(compute_vfci_tau0, fit_args)
}
