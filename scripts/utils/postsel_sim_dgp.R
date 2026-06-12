# Synthetic DGP for the post-selection split simulation. AMENDED
# (D10a, 2026-06-12, user-approved) after the Task 5 premise stop;
# see the plan's decision log and docs/postsel-sim-pilot-log.md.
# Satisfies the spec's assumptions with the relative-correlation
# bound holding UNIFORMLY over weight directions at a known,
# closed-form level:
#   Z_t = phi * Z_{t-1} + sqrt(1 - phi^2) * u_t, u_t iid N(0, I_K)
#         (Z stays GAUSSIAN -- the closed forms require it)
#   eps2_{i,t} = exp(delta_i' Z_t / 2) * eta_{i,t}
#   eps1_t     = a' eps2_t + sigma_nu * nu_t
#   eta, nu iid mean-0 unit-variance, family set by shock_dist:
#   "uniform" (scaled to [-sqrt(3), sqrt(3)], E[eta^4] = 1.8) or
#   "gaussian" (E[eta^4] = 3); kappa_eta = E[eta^4] threads every
#   closed form. The constant-in-direction ratio
#   |corr(l'Z, eps1 eps2_i)| = rho_i * |corr(l'Z, eps2_i^2)|
# uses only conditional independence and E[eta^2] = 1, so it holds
# for BOTH shock families; with tau > max_i rho_i the bound holds
# uniformly over the whole admissibility class and any coverage
# failure under data-selected weights is a finite-sample selection
# effect, not population inadmissibility.
#
# RECORDED REBUTTAL (D10a; never re-try): rescaling (a, sigma_nu) ->
# (kappa*a, kappa*sigma_nu) leaves rho and EVERY scale-free statistic
# -- including membership and coverage -- invariant almost surely
# (U0 scales by kappa realization-wise). sigma_nu's size is not a
# premise lever; the premise levers are the gap (tau - rho), the
# het-signal corr(l'Z, eps2_i^2) per constraint, and the shock tails
# (kappa_eta). a = (0.1, -0.1) is numerical-scale hygiene only.

# Lognormal moment pieces shared by the rho formula and the sigma_nu
# solver. For Z ~ N(0, I_K), d a loading column, and
# kappa_eta = E[eta^4]:
#   m1_i  = E[eps2_i^2]   = exp(|d_i|^2 / 2)
#   v_i   = var(eps2_i^2) = kappa_eta exp(2|d_i|^2) - exp(|d_i|^2)
#   x_i   = sum_{m != i} a_m^2 E[eps2_i^2 eps2_m^2]
#         = sum_{m != i} a_m^2 exp((|d_i|^2 + |d_m|^2)/2 + d_i'd_m)
.postsel_moment_pieces <- function(delta, a, kappa_eta) {
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
    v = kappa_eta * exp(2 * nsq) - exp(nsq),
    cross = cross
  )
}

# Closed-form uniform relative-correlation level rho_i (constant in
# the weight direction):
#   rho_i = |a_i| sqrt(v_i) /
#           sqrt(a_i^2 v_i + x_i + sigma_nu^2 m1_i)
postsel_dgp_rho <- function(params) {
  p <- .postsel_moment_pieces(
    params$delta, params$a, params$kappa_eta
  )
  a <- params$a
  abs(a) * sqrt(p$v) /
    sqrt(a^2 * p$v + p$cross + params$sigma_nu^2 * p$m1)
}

# Smallest sigma_nu pushing every component's rho_i down to the
# target: solve rho_i = target per component and take the largest
# sigma (rho_i is decreasing in sigma_nu), floored away from zero.
.postsel_sigma_for_rho <- function(delta, a, rho_target, kappa_eta) {
  p <- .postsel_moment_pieces(delta, a, kappa_eta)
  sig_sq <- (a^2 * p$v * (1 / rho_target^2 - 1) - p$cross) / p$m1
  sqrt(max(c(sig_sq, 0.25)))
}

# DGP parameter bundle (defaults = D10a recalibration; the Stage-P
# premise screen may move rho_target / shock_dist within its
# pre-registered grid). delta columns: component one front-loaded
# decay, component two REVERSED (back-loaded) decay with NO sign
# alternation -- the original alternating pattern made the
# equal-weight benchmark nearly orthogonal to component two's het
# signal (l_eq'delta_2 = 0.19 vs 0.76 at K = 4), structurally
# crippling the binding constraint. delta_scale = 0.75 puts |delta|^2
# at the analytic signal optimum (~0.86). sigma_nu is derived so
# max_i rho_i equals rho_target exactly; the certificate is
# recomputed and asserted.
postsel_dgp_params <- function(k_inst,
                               phi = 0.5,
                               delta_scale = 0.75,
                               delta_decay = 0.6,
                               a = c(0.1, -0.1),
                               rho_target = 0.03,
                               theta0 = c(0.5, -0.5),
                               shock_dist = c("uniform", "gaussian")) {
  shock_dist <- match.arg(shock_dist)
  kappa_eta <- if (identical(shock_dist, "uniform")) 1.8 else 3
  n_comp <- length(a)
  delta <- vapply(seq_len(n_comp), function(i) {
    expo <- if (i %% 2L == 0L) {
      k_inst - seq_len(k_inst)
    } else {
      seq_len(k_inst) - 1L
    }
    delta_scale * delta_decay^expo
  }, numeric(k_inst))
  params <- list(
    k_inst = k_inst, phi = phi, delta = delta, a = a,
    rho_target = rho_target, shock_dist = shock_dist,
    kappa_eta = kappa_eta,
    sigma_nu = .postsel_sigma_for_rho(
      delta, a, rho_target, kappa_eta
    ),
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
# eta and nu follow params$shock_dist (D10a); Z stays Gaussian (the
# closed-form certificate requires it).
draw_postsel_data <- function(params, t_obs, seed, burn = 200L) {
  set.seed(seed)
  draw_unit <- function(n) {
    if (identical(params$shock_dist, "uniform")) {
      sqrt(3) * (2 * runif(n) - 1)
    } else {
      rnorm(n)
    }
  }
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
  eta <- matrix(draw_unit(t_obs * n_comp), t_obs, n_comp)
  nu <- draw_unit(t_obs)
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
