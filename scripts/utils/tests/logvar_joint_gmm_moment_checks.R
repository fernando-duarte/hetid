# Stage A moment-layer checks: graph equivalence, residualized-z redundancy and
# representation invariance, the design rank precondition, and moment/Jacobian finite
# differences in b and the variance parameters. The profiled intercepts, moment scales,
# Jacobian rank, and Jensen gap live in the companion logvar_joint_gmm_profile_checks.R
# (split to stay under the repository line cap). Sourced by test_logvar_joint_gmm.R,
# which owns check(), jg_try, jg_need, and the jg_fx fixtures. Every check first asserts
# the not-yet-built symbol exists, so an absent module fails closed for the right reason
# rather than passing by coincidence. Section numbers cite the joint-GMM research dossier.
fx <- jg_fx
w1 <- fx$w1
w2 <- fx$w2
x_mat <- fx$x_mat
proj <- fx$proj
b_star <- fx$b_star
b_fd <- fx$b_fd

# Central-difference Jacobian of a vector-valued moment f at the parameter x.
jg_fd_jac <- function(f, x, h) {
  vapply(seq_along(x), function(k) {
    e <- replace(numeric(length(x)), k, h)
    (f(x + e) - f(x - e)) / (2 * h)
  }, numeric(length(f(x))))
}

# Graph equivalence (dossier section 1): the just-identified log block recovers
# theta_hat = P log(e^2); the benchmark moment vanishes there.
check("just-identified log moment vanishes at the benchmark map theta_hat", jg_try({
  jg_need("logvar_moment_log")
  b <- fx$grid_b[3L, ]
  th <- logvar_theta_hat(b, w1, w2, proj)
  max(abs(logvar_moment_log(b, th[1L], th[-1L], w1, w2, x_mat))) < 1e-10
}))
# The mean moment is the column mean of the observation-level contributions.
check("the log moment equals the mean of its observation contributions", jg_try({
  jg_need("logvar_moment_log", "logvar_moment_log_contrib")
  th <- logvar_theta_hat(b_fd, w1, w2, proj)
  g <- logvar_moment_log(b_fd, th[1L], th[-1L], w1, w2, x_mat)
  cc <- logvar_moment_log_contrib(b_fd, th[1L], th[-1L], w1, w2, x_mat)
  max(abs(g - colMeans(cc))) < 1e-12
}))

# Redundancy (dossier section 3.1): a z in the span of X residualizes to zero rank
# and is reported redundant, never overidentification.
check("redundant z (Z = X A) residualizes to zero rank and is unreliable", jg_try({
  jg_need("logvar_residualize_moment_basis")
  bs <- logvar_residualize_moment_basis(fx$z_redundant, x_mat)
  bs$rank == 0L && identical(bs$status, "unreliable")
}))
check("a genuine two-column z residualizes to full rank two", jg_try({
  jg_need("logvar_residualize_moment_basis")
  bs <- logvar_residualize_moment_basis(fx$z_multi, x_mat)
  bs$rank == 2L && identical(bs$status, "ok") && ncol(bs$basis) == 2L
}))
# The projector-derived basis is invariant to well-conditioned right transforms.
check("the residualized basis is invariant to scale/sign/permutation/mixing", jg_try({
  jg_need("logvar_residualize_moment_basis")
  base <- logvar_residualize_moment_basis(fx$z_multi, x_mat)$basis
  same <- function(zz) {
    max(abs(logvar_residualize_moment_basis(zz, x_mat)$basis - base)) < 1e-8
  }
  same(fx$z_scaled) && same(fx$z_sign) && same(fx$z_perm) && same(fx$z_mix)
}))
check("a near-tied z returns the pinned unreliable rank gap, not a rotated basis", jg_try({
  jg_need("logvar_residualize_moment_basis")
  identical(logvar_residualize_moment_basis(fx$z_tied, x_mat)$status, "unreliable")
}))
# Stage A rank precondition: a rank-deficient design stops before projection.
check("a rank-deficient design stops Stage A with input_rank_deficient", jg_try({
  jg_need("logvar_joint_check_design")
  x_bad <- cbind(x_mat, x_mat[, 2L])
  msg <- tryCatch(
    {
      logvar_joint_check_design(x_bad)
      ""
    },
    error = function(e) paste(c(class(e), conditionMessage(e)), collapse = " ")
  )
  grepl("input_rank_deficient", msg)
}))

# Moment/Jacobian finite differences away from crossings (dossier sections 3.2, 4.2),
# over the pinned step sweep. Log and PPML blocks in b and in the variance params.
check("analytic moment Jacobians match central differences over the step sweep", jg_try({
  jg_need(
    "logvar_moment_log", "logvar_moment_ppml", "logvar_jac_log_b",
    "logvar_jac_ppml_b", "logvar_jac_log_theta", "logvar_jac_ppml_theta"
  )
  aL <- 0.1
  aP <- -0.2
  beta <- c(0.2, -0.1, 0.15, -0.05)
  jlb <- logvar_jac_log_b(b_fd, w1, w2, x_mat)
  jpb <- logvar_jac_ppml_b(b_fd, w1, w2, x_mat)
  jlt <- logvar_jac_log_theta(x_mat)
  jpt <- logvar_jac_ppml_theta(b_fd, aP, beta, w1, w2, x_mat)
  ok <- TRUE
  for (h in c(1e-4, 3e-5, 1e-5)) {
    fl <- jg_fd_jac(function(bb) logvar_moment_log(bb, aL, beta, w1, w2, x_mat), b_fd, h)
    fp <- jg_fd_jac(function(bb) logvar_moment_ppml(bb, aP, beta, w1, w2, x_mat), b_fd, h)
    ft <- jg_fd_jac(function(th) {
      logvar_moment_log(b_fd, th[1L], th[-1L], w1, w2, x_mat)
    }, c(aL, beta), h)
    fq <- jg_fd_jac(function(th) {
      logvar_moment_ppml(b_fd, th[1L], th[-1L], w1, w2, x_mat)
    }, c(aP, beta), h)
    ok <- ok && max(abs(fl - jlb)) < 1e-6 && max(abs(fp - jpb)) < 1e-6 &&
      max(abs(ft - jlt)) < 1e-6 && max(abs(fq - jpt)) < 1e-6
  }
  ok
}))
