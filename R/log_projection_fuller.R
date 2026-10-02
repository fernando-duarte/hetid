# Two-pass Fuller log projection (spec "Two-pass Fuller correction"). Every
# quantity stays in logs: the candidate's mean-sample scale, both adjustment
# profiles, and the transform. Candidates are columns throughout; the screen
# has already removed zero-scale and non-finite columns.
log_projection_fuller <- function(prep, e_mean, e, log_x, multiplier) {
  n_vol <- nrow(e)
  k <- ncol(e)
  log_c <- 2 * log(multiplier) - log(n_vol)
  log_scale <- log_mean_square_cols(e_mean)
  log_d0 <- matrix(log_c + log_scale, n_vol, k, byrow = TRUE)
  first <- log_projection_fuller_transform(log_x, log_d0)
  coef0 <- prep$projection %*% first$value
  eta <- prep$x_centered %*% coef0[-1L, , drop = FALSE]
  log_ratio <- sweep(eta, 2L, log_sum_exp_cols(eta)) + log(n_vol)
  second <- log_projection_fuller_transform(log_x, log_d0 + log_ratio)
  list(
    coef = prep$projection %*% second$value,
    diagnostics = list(
      log_scale = log_scale,
      log_c = rep(log_c, k),
      share_small = colMeans(log_x < log_d0 + log_ratio),
      profile_log_ratio_min = apply(log_ratio, 2L, min),
      profile_log_ratio_max = apply(log_ratio, 2L, max),
      first_pass_coef = coef0
    ),
    work = list(
      e_mean = e_mean, a0 = first$a, rho0 = first$rho, eta = eta,
      log_ratio = log_ratio, a = second$a, rho = second$rho,
      response = second$value
    )
  )
}

# Complete Fuller Jacobian (spec chain rule): the candidate scale, the
# first-pass slopes, and the normalized second-pass profile all depend on b.
# The scale derivative divides by the rescaled sum before the rescale factor,
# so no intermediate overflows at extreme residual magnitudes.
log_projection_fuller_jacobian <- function(prep, e, log_abs_e, work) {
  e_mean <- drop(work$e_mean)
  u <- max(abs(e_mean))
  s_sum <- sum((e_mean / u)^2)
  k_row <- -2 * ((drop(crossprod(prep$w2_mean, e_mean / u)) / s_sum) / u)
  n_vol <- length(e)
  ones_k <- matrix(k_row, n_vol, length(k_row), byrow = TRUE)
  rho0 <- drop(work$rho0)
  rho <- drop(work$rho)
  d0 <- log_projection_resid_deriv(e, log_abs_e, drop(work$a0), rho0)
  j0 <- prep$projection %*% (-d0 * prep$w2 + rho0^2 * ones_k)
  slope_path <- prep$x_centered %*% j0[-1L, , drop = FALSE]
  soft <- exp(drop(work$eta) - log_sum_exp_cols(work$eta))
  h <- ones_k + slope_path -
    matrix(colSums(soft * slope_path), n_vol, ncol(slope_path), byrow = TRUE)
  d <- log_projection_resid_deriv(e, log_abs_e, drop(work$a), rho)
  prep$projection %*% (-d * prep$w2 + rho^2 * h)
}
