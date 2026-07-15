# Stage A profile-layer checks (companion to logvar_joint_gmm_moment_checks.R, split to
# stay under the repository line cap): the profiled intercepts and their spelled-out
# softmax-weighted derivatives, log-sum-exp stability and the structurally degenerate
# all-zero response, frozen moment scales with rescaling invariance, Jacobian
# rank/weak-singular-value diagnostics, and the Gaussian Jensen gap. Sourced by
# test_logvar_joint_gmm.R after the moment checks; it owns check(), jg_try, jg_need, and
# the jg_fx fixtures. Every check asserts the not-yet-built symbol exists first, so an
# absent module fails closed for the right reason. Section numbers cite the dossier.
fx <- jg_fx
w1 <- fx$w1
w2 <- fx$w2
x_mat <- fx$x_mat
b_star <- fx$b_star
b_fd <- fx$b_fd

# Profiled intercepts (dossier section 4.3): log-sum-exp stability, the structurally
# degenerate all-zero response, and the softmax-weighted derivative formulas FD-tested
# on the profiled system itself with a non-centered R (n, K, p all unequal).
check("log-sum-exp matches the naive value and stays finite under overflow", jg_try({
  jg_need("logvar_logsumexp")
  v <- c(-1, 0, 2, 0.5)
  ov <- c(700, -700, 690)
  abs(logvar_logsumexp(v) - log(sum(exp(v)))) < 1e-12 && is.finite(logvar_logsumexp(ov))
}))
check("profiled a_P is log-sum-exp stable when R beta approaches the overflow wall", jg_try({
  jg_need("logvar_profile_a_P")
  is.finite(logvar_profile_a_P(b_star, fx$beta_ovf, w1, w2, x_mat))
}))
check("profiled a_P fails structurally degenerate on an all-zero response", jg_try({
  jg_need("logvar_profile_a_P")
  res <- tryCatch(
    {
      logvar_profile_a_P(b_star, rep(0, 4L), fx$w1_az, w2, x_mat)
      "ok"
    },
    error = function(e) "stopped"
  )
  identical(res, "stopped")
}))
check("the four profiled intercept derivatives match finite differences", jg_try({
  jg_need("logvar_profile_a_L", "logvar_profile_a_P", "logvar_profile_jacobian")
  nx <- fx$nc_x
  nw1 <- fx$nc_w1
  nw2 <- fx$nc_w2
  nb <- fx$nc_b
  nbeta <- fx$nc_beta
  jac <- logvar_profile_jacobian(nb, nbeta, nw1, nw2, nx)
  dlb <- jg_fd_grad(function(bb) logvar_profile_a_L(bb, nbeta, nw1, nw2, nx), nb)
  dlbe <- jg_fd_grad(function(be) logvar_profile_a_L(nb, be, nw1, nw2, nx), nbeta)
  dpb <- jg_fd_grad(function(bb) logvar_profile_a_P(bb, nbeta, nw1, nw2, nx), nb)
  dpbe <- jg_fd_grad(function(be) logvar_profile_a_P(nb, be, nw1, nw2, nx), nbeta)
  max(abs(jac$da_L_db - dlb)) < 1e-6 && max(abs(jac$da_L_dbeta - dlbe)) < 1e-6 &&
    max(abs(jac$da_P_db - dpb)) < 1e-6 && max(abs(jac$da_P_dbeta - dpbe)) < 1e-6
}))
# Profiling enforces the two intercept moments exactly (unprofiled oracle agreement).
check("profiling drives both intercept moments to zero (oracle agreement)", jg_try({
  jg_need("logvar_profile_a_L", "logvar_profile_a_P", "logvar_moment_log", "logvar_moment_ppml")
  beta <- c(0.1, -0.05, 0.2, -0.1)
  aL <- logvar_profile_a_L(b_fd, beta, w1, w2, x_mat)
  aP <- logvar_profile_a_P(b_fd, beta, w1, w2, x_mat)
  abs(logvar_moment_log(b_fd, aL, beta, w1, w2, x_mat)[1L]) < 1e-10 &&
    abs(logvar_moment_ppml(b_fd, aP, beta, w1, w2, x_mat)[1L]) < 1e-10
}))

# Frozen moment scales (dossier section 5): RMS of observation contributions, fail
# closed on a zero column, and rescaling invariance of the scaled moment S^{-1} g.
check("moment scales are the RMS contributions and fail closed on a zero column", jg_try({
  jg_need("logvar_moment_log_contrib", "logvar_moment_scales")
  cc <- logvar_moment_log_contrib(b_fd, 0.1, c(0.2, -0.1, 0.15, -0.05), w1, w2, x_mat)
  s <- logvar_moment_scales(cc)
  zero_fail <- tryCatch(
    {
      logvar_moment_scales(cbind(cc, 0))
      "ok"
    },
    error = function(e) "stopped"
  )
  max(abs(s - sqrt(colMeans(cc^2)))) < 1e-12 && identical(zero_fail, "stopped")
}))
check("scaling a moment column and its scale together leaves S^{-1} g unchanged", jg_try({
  jg_need("logvar_moment_log_contrib", "logvar_moment_scales")
  cc <- logvar_moment_log_contrib(b_fd, 0.1, c(0.2, -0.1, 0.15, -0.05), w1, w2, x_mat)
  cc2 <- cc
  cc2[, 2L] <- 3 * cc2[, 2L]
  max(abs(colMeans(cc2) / logvar_moment_scales(cc2) -
    colMeans(cc) / logvar_moment_scales(cc))) < 1e-12
}))

# Jacobian rank / singular values / weak_jacobian (dossier section 4.2, review rank
# qualification): full rank clean, a near-cutoff singular value flagged weak.
check("Jacobian rank helper reports full rank and flags weak singular values", jg_try({
  jg_need("logvar_jac_log_theta", "logvar_jacobian_rank")
  full <- logvar_jacobian_rank(logvar_jac_log_theta(x_mat))
  weak <- logvar_jacobian_rank(diag(c(1, 1, 1, 1, 1e-7)))
  full$rank == ncol(x_mat) && isFALSE(full$weak_jacobian) &&
    length(full$singular_values) == ncol(x_mat) && isTRUE(weak$weak_jacobian)
}))

# Jensen/shape gap (dossier section 8): the balanced multiset has a known gap and the
# midpoint-normal grid reproduces the single-sourced Gaussian value within 0.01.
check("profiled intercept gap recovers the balanced multiset Jensen gap", jg_try({
  jg_need("logvar_profile_a_L", "logvar_profile_a_P")
  aL <- logvar_profile_a_L(b_star, rep(0, 4L), w1, w2, x_mat)
  aP <- logvar_profile_a_P(b_star, rep(0, 4L), w1, w2, x_mat)
  abs((aP - aL) - fx$gap_bal) < 1e-8
}))
check("the midpoint-normal grid reproduces the Gaussian Jensen gap within 0.01", jg_try({
  jg_need("logvar_logsumexp")
  y <- fx$jen_u^2
  aP <- logvar_logsumexp(log(y)) - log(length(y))
  abs((aP - mean(log(y))) - 1.270362845) < 0.01
}))
