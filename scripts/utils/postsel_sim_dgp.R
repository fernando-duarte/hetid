# Synthetic DGP for the post-selection split simulation. Satisfies
# the spec's assumptions with the relative-correlation bound holding
# UNIFORMLY over weight directions at a known, closed-form level:
#   Z_t = phi * Z_{t-1} + sqrt(1 - phi^2) * u_t, u_t iid N(0, I_K)
#         (Z_t ~ N(0, I_K) marginally; serially dependent, phi > 0)
#   eps2_{i,t} = exp(delta_i' Z_t / 2) * eta_{i,t}, eta iid N(0, 1)
#   eps1_t     = a' eps2_t + sigma_nu * nu_t,       nu  iid N(0, 1)
#   X_t = (1, Z_t')', Y2_t = beta20 X_t + eps2_t,
#   Y1_t = X_t' beta10 + theta0' Y2_t + eps1_t  (contemporaneous;
#   the t+1 lag convention is the application's, not the spec's)
# Because the eta components are independent given Z,
#   cov(l'Z, eps1 eps2_i) = a_i * cov(l'Z, exp(delta_i'Z)) and
#   cov(l'Z, eps2_i^2)    =       cov(l'Z, exp(delta_i'Z)),
# so |corr(l'Z, eps1 eps2_i)| = rho_i * |corr(l'Z, eps2_i^2)| for
# EVERY direction l, with
#   rho_i = |a_i| sd(eps2_i^2) / sd(eps1 eps2_i)
# constant in l and available in closed form (lognormal moments).
# A bound with slack tau > max_i rho_i therefore holds uniformly
# over the whole admissibility class: any coverage failure under
# data-selected weights in this DGP is a finite-sample selection
# effect, not population inadmissibility.

# Lognormal moment pieces shared by the rho formula and the sigma_nu
# solver. For Z ~ N(0, I_K) and d a loading column:
#   m1_i  = E[eps2_i^2]   = exp(|d_i|^2 / 2)
#   v_i   = var(eps2_i^2) = 3 exp(2|d_i|^2) - exp(|d_i|^2)
#   x_i   = sum_{m != i} a_m^2 E[eps2_i^2 eps2_m^2]
#         = sum_{m != i} a_m^2 exp((|d_i|^2 + |d_m|^2)/2 + d_i'd_m)
.postsel_moment_pieces <- function(delta, a) {
  nsq <- colSums(delta^2)
  n_comp <- length(a)
  cross <- vapply(seq_len(n_comp), function(i) {
    others <- setdiff(seq_len(n_comp), i)
    sum(vapply(others, function(m) {
      a[m]^2 *
        exp((nsq[i] + nsq[m]) / 2 + sum(delta[, i] * delta[, m]))
    }, numeric(1)))
  }, numeric(1))
  list(
    m1 = exp(nsq / 2),
    v = 3 * exp(2 * nsq) - exp(nsq),
    cross = cross
  )
}

# Closed-form uniform relative-correlation level rho_i (constant in
# the weight direction):
#   rho_i = |a_i| sqrt(v_i) /
#           sqrt(a_i^2 v_i + x_i + sigma_nu^2 m1_i)
postsel_dgp_rho <- function(params) {
  p <- .postsel_moment_pieces(params$delta, params$a)
  a <- params$a
  abs(a) * sqrt(p$v) /
    sqrt(a^2 * p$v + p$cross + params$sigma_nu^2 * p$m1)
}

# Smallest sigma_nu pushing every component's rho_i down to the
# target: solve rho_i = target per component and take the largest
# sigma (rho_i is decreasing in sigma_nu), floored away from zero.
.postsel_sigma_for_rho <- function(delta, a, rho_target) {
  p <- .postsel_moment_pieces(delta, a)
  sig_sq <- (a^2 * p$v * (1 / rho_target^2 - 1) - p$cross) / p$m1
  sqrt(max(c(sig_sq, 0.25)))
}

# DGP parameter bundle. delta columns are per-component instrument
# loadings with geometrically decaying magnitude (every instrument
# weakly relevant; later instruments mostly add selection noise) and
# an alternating sign pattern across components. sigma_nu is derived
# so that max_i rho_i equals rho_target exactly for the binding
# component; the certificate is recomputed and asserted.
postsel_dgp_params <- function(k_inst,
                               phi = 0.5,
                               delta_scale = 0.7,
                               delta_decay = 0.6,
                               a = c(0.4, -0.4),
                               rho_target = 0.10,
                               theta0 = c(0.5, -0.5)) {
  n_comp <- length(a)
  delta <- vapply(seq_len(n_comp), function(i) {
    sgn <- if (i %% 2L == 0L) -delta_decay else delta_decay
    delta_scale * sgn^(seq_len(k_inst) - 1)
  }, numeric(k_inst))
  params <- list(
    k_inst = k_inst, phi = phi, delta = delta, a = a,
    rho_target = rho_target,
    sigma_nu = .postsel_sigma_for_rho(delta, a, rho_target),
    theta0 = theta0,
    beta10 = c(0.5, rep(0.2, k_inst)),
    beta20 = matrix(0.3, nrow = n_comp, ncol = k_inst + 1)
  )
  params$rho <- postsel_dgp_rho(params)
  if (max(params$rho) > rho_target + 1e-8) {
    stop("postsel_dgp_params: sigma_nu solver missed its rho target")
  }
  params
}

# Draw one synthetic sample. ALL randomness is consumed here, up
# front, under the given seed; callers must not draw between this
# and any seeded optimizer call they make in the same replication.
draw_postsel_data <- function(params, t_obs, seed, burn = 200L) {
  set.seed(seed)
  k <- params$k_inst
  n_comp <- length(params$a)
  total <- t_obs + burn
  u <- matrix(rnorm(total * k), total, k)
  z_all <- matrix(0, total, k)
  z_all[1, ] <- u[1, ]
  innov_scale <- sqrt(1 - params$phi^2)
  for (t in seq.int(2L, total)) {
    z_all[t, ] <- params$phi * z_all[t - 1, ] + innov_scale * u[t, ]
  }
  z <- z_all[burn + seq_len(t_obs), , drop = FALSE]
  colnames(z) <- paste0("z", seq_len(k))
  eta <- matrix(rnorm(t_obs * n_comp), t_obs, n_comp)
  nu <- rnorm(t_obs)
  cond_sd <- exp(z %*% params$delta / 2)
  eps2 <- cond_sd * eta
  eps1 <- drop(eps2 %*% params$a) + params$sigma_nu * nu
  x <- cbind(1, z)
  y2 <- x %*% t(params$beta20) + eps2
  y1 <- drop(x %*% params$beta10) + drop(y2 %*% params$theta0) + eps1
  list(y1 = y1, y2 = y2, z = z)
}

# Reduced-form residuals + moments on a window of rows. The first
# stage (OLS of y1 and each y2 column on (1, Z)) is refit on the
# window, mirroring the real-data study's per-block refit.
sim_window_moments <- function(dat, rows) {
  z <- dat$z[rows, , drop = FALSE]
  qr_x <- qr(cbind(1, z))
  hetid::compute_identification_moments(
    qr.resid(qr_x, dat$y1[rows]),
    qr.resid(qr_x, dat$y2[rows, , drop = FALSE]),
    z
  )
}

# Ex ante fixed benchmark weights: equal weight on every primitive
# instrument for every component, chosen before seeing any data.
equal_weight_lambda <- function(k_inst, n_comp) {
  matrix(1 / sqrt(k_inst), nrow = k_inst, ncol = n_comp)
}

# theta0 membership of the estimated set under the package's
# hin <= 0 convention: inside iff every constraint value is
# non-positive. Membership needs no profile-bound solves and is
# well-defined for unbounded sets too.
covers_theta0 <- function(lambda, tau, moments, theta0) {
  qs <- hetid::build_general_quadratic_system(lambda, tau, moments)
  max(hetid::make_system_checker(qs$quadratic)(theta0)) <= 0
}
