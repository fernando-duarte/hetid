# Crossing-geometry, stability, and frozen-witness math/units checks for the
# joint-null diagnostic (distance_objective.R and stability.R): the
# slope-leverage divergence, the zero-leverage and cancellation cases, the
# perturbation aggregation, and the response/objective/gradient/scales
# primitives. Sourced by test_joint_null.R, which owns check(),
# jn_try, jn_fx, and the jn_ball helper and shared fd_jacobian. The leverage and
# zero-leverage checks are pure algebra on logvar_theta_hat and may pass before
# the module lands; the rest fail closed.
fx <- jn_fx
di2 <- fx$d^-2
# Assumed stability signature (plan "Interfaces"; note if the module differs):
#   logvar_joint_null_stability(b_hat, w1, w2, proj, qs, eps_ref, root_tol, ...)
#   -> list(status, stability_status, rank_dir, perturbation_status,
#      membership_result).
jn_stab <- function(b, w1, w2, proj, qs, eps_ref) {
  logvar_joint_null_stability(b, w1, w2, proj, qs, eps_ref, root_tol)
}
jn_unreliable <- function(s) {
  identical(s$status, "unreliable") ||
    identical(s$stability_status, "fail") ||
    !identical(s$membership_result, "compatible_witness")
}
# Leverage crossing: theta_R(b_c + r v) ~ 2 (A P_.i) log|r| + O(1) as r shrinks.
check("jn leverage crossing shows the 2 (A P_.i) log|r| slope divergence", jn_try({
  rs <- 10^seq(-2, -6, length.out = 9L)
  th <- vapply(rs, function(r) {
    logvar_theta_hat(
      fx$b_cross_lev + r * fx$v_lev, fx$w1_lev, fx$w2_lev,
      fx$proj_lev
    )[-1]
  }, numeric(4L))
  pred <- 2 * fx$proj_lev[-1, fx$i_lev]
  est <- vapply(1:4, function(j) {
    unname(stats::coef(stats::lm(th[j, ] ~ log(abs(rs))))[2])
  }, numeric(1L))
  max(abs(est - pred)) / max(abs(pred)) < 0.05
}))
# Zero-leverage crossing: only the intercept diverges (rate 2/n); slopes bounded.
check("jn zero-leverage crossing diverges only in the intercept", jn_try({
  rs <- 10^seq(-2, -7, length.out = 11L)
  th <- vapply(rs, function(r) {
    logvar_theta_hat(
      fx$b_cross_zlev + r * fx$v_zlev, fx$w1_zlev, fx$w2_zlev,
      fx$proj_zlev
    )
  }, numeric(5L))
  int_rate <- unname(stats::coef(stats::lm(th[1, ] ~ log(abs(rs))))[2])
  n_z <- nrow(fx$pcr_zlev)
  abs(int_rate - 2 / n_z) < 0.1 * (2 / n_z) && all(diff(th[1, ]) < 0) &&
    max(abs(th[-1, ])) < 0.05
}))
# Cancellation: the anti-parallel near-puncture is unreliable, never a witness.
check("jn cancellation near a puncture classifies unreliable, not a witness", jn_try({
  qs_can <- jn_ball(fx$b_cross_can, 0.2)
  b_tab <- list(set_lower = fx$b_cross_can - 0.2, set_upper = fx$b_cross_can + 0.2)
  row <- logvar_joint_null_at_tau(
    0.05, 0.05, b_tab, fx$w1_can, fx$w2_can,
    fx$proj_can, (1 / apply(fx$pcr_can, 2, stats::sd))^2, rep(1.5, nrow(fx$pcr_can)),
    qs_can
  )
  identical(row$status, "unreliable") &&
    !identical(row$membership_result, "compatible_witness")
}))
# Machine-precision adjacency (min|e| <= 1e-8 * e_scale_ref) is unreliable.
check("jn a machine-precision-adjacent min|e| is unreliable outright", jn_try({
  s <- jn_stab(
    fx$b_cross_can, fx$w1_can, fx$w2_can, fx$proj_can,
    jn_ball(fx$b_cross_can, 0.2), rep(1.5, nrow(fx$pcr_can))
  )
  jn_unreliable(s)
}))
# An active zero-norm row has no defined normal, so it is unreliable, not an error.
check("jn an active zero-norm row yields unreliable, not a division error", jn_try({
  w2z <- fx$w2_lev
  w2z[fx$i_lev, ] <- 0
  w1z <- fx$w1_lev
  w1z[fx$i_lev] <- 0
  s <- jn_stab(
    fx$b_cross_lev, w1z, w2z, fx$proj_lev,
    jn_ball(fx$b_cross_lev, 0.3), rep(1.2, nrow(fx$pcr_lev))
  )
  jn_unreliable(s)
}))
# Matched full-rank perturbation coverage passes at a clean witness.
check("jn matched full-rank perturbation coverage passes at a clean witness", jn_try({
  s <- jn_stab(fx$b_star, fx$w1, fx$w2, fx$proj, fx$qs_ball, fx$eps_ref)
  identical(s$perturbation_status, "pass") || identical(s$rank_dir, 3L) ||
    identical(s$stability_status, "pass")
}))
# One collapsing perturbation direction fails the candidate (any-collapse rule).
check("jn one collapsing perturbation direction fails the candidate", jn_try({
  s <- jn_stab(
    fx$b_cross_can, fx$w1_can, fx$w2_can, fx$proj_can,
    jn_ball(fx$b_cross_can, 0.2), rep(1.5, nrow(fx$pcr_can))
  )
  identical(s$perturbation_status, "fail") || jn_unreliable(s)
}))
# In-band puncture: an active near-crossing (min|e| above the machine-adjacency
# gate) whose scaled distance and min|e| both collapse as the radius shrinks is
# caught by the two-endpoint signature, not short-circuited before it.
check("jn an in-band puncture signature classifies unreliable", jn_try({
  s <- jn_stab(
    fx$b_cross_can, fx$w1_pnc, fx$w2_can, fx$proj_can,
    jn_ball(fx$b_cross_can, 0.2), rep(1.5, nrow(fx$pcr_can))
  )
  identical(s$reason, "puncture_signature") && jn_unreliable(s)
}))
# A rank-deficient direction set must not round up to full coverage (rank_dir 3).
check("jn a rank-deficient direction set fails the SVD coverage threshold", jn_try({
  s <- jn_stab(
    fx$b_cross_zlev, fx$w1_zlev, fx$w2_zlev, fx$proj_zlev,
    jn_ball(fx$b_cross_zlev, 0.3), rep(1.3, nrow(fx$pcr_zlev))
  )
  is.null(s$rank_dir) || isTRUE(s$rank_dir < 3L) || jn_unreliable(s)
}))
# Known witness: b_star attains zero slopes and classifies compatible_witness.
check("jn known witness at b_star classifies compatible_witness", jn_try({
  b_tab <- list(set_lower = fx$b_star - 0.5, set_upper = fx$b_star + 0.5)
  row <- logvar_joint_null_at_tau(
    0.05, 0.05, b_tab, fx$w1, fx$w2, fx$proj,
    di2, fx$eps_ref, fx$qs_ball
  )
  identical(row$membership_result, "compatible_witness") &&
    row$scaled_linf <= root_tol && identical(row$status, "bounded")
}))
# Objective at b_star has zero slopes and zero distance.
check("jn objective at b_star has zero slopes and zero distance", jn_try({
  o <- logvar_joint_null_objective(fx$b_star, fx$w1, fx$w2, fx$proj, di2)
  max(abs(o$s)) < 1e-8 && o$q < 1e-12
}))
# Response marks an exact-zero column unavailable and keeps the others.
check("jn response marks an exact-zero column unavailable, keeps others", jn_try({
  r <- logvar_joint_null_response(cbind(replace(fx$e_star, 1, 0), fx$e_star))
  isTRUE(r$unavailable[1]) && isFALSE(r$unavailable[2]) &&
    max(abs(r$log_sq[, 1] - 2 * log(abs(fx$e_star)))) < 1e-12
}))
# Response keeps a nonzero underflow finite and matches a scalar column call.
check("jn response keeps underflow finite and matches a scalar column call", jn_try({
  ru <- logvar_joint_null_response(matrix(1e-160, fx$n, 1))
  eps <- fx$w1 - fx$w2 %*% t(rbind(fx$b_star, fx$b_nonwit))
  r <- logvar_joint_null_response(eps)
  s1 <- logvar_joint_null_response(eps[, 1, drop = FALSE])$log_sq
  isFALSE(ru$unavailable[1]) && all(is.finite(ru$log_sq)) &&
    max(abs(r$log_sq[, 1] - s1)) < 1e-12
}))
# Objective q equals half the summed squared scaled slopes (scalar == 1-row).
check("jn objective q equals half the summed squared scaled slopes", jn_try({
  o <- logvar_joint_null_objective(fx$b_nonwit, fx$w1, fx$w2, fx$proj, di2)
  a <- logvar_joint_null_objective(matrix(fx$b_nonwit, 1), fx$w1, fx$w2, fx$proj, di2)
  abs(o$q - 0.5 * sum((o$s / fx$d)^2)) < 1e-10 * max(1, o$q) &&
    abs(o$q - a$q[1]) < 1e-12
}))
# Analytic gradient of q matches central finite differences.
check("jn analytic gradient matches central finite differences", jn_try({
  qf <- function(b) logvar_joint_null_objective(b, fx$w1, fx$w2, fx$proj, di2)$q
  an <- logvar_joint_null_gradient(fx$b_nonwit, fx$w1, fx$w2, fx$proj, di2)
  max(abs(an - fd_jacobian(qf, fx$b_nonwit))) < 1e-6 * max(1, max(abs(an)))
}))
# Scales are 1/sd, strictly positive, with d_inv2 = d^-2.
check("jn scales are 1/sd, strictly positive, with d_inv2 = d^-2", jn_try({
  sc <- logvar_joint_null_scales(fx$pcr)
  all(sc$d > 0) && all(is.finite(sc$d)) && max(abs(sc$d - fx$d)) < 1e-12 &&
    max(abs(sc$d_inv2 - sc$d^-2)) < 1e-12
}))
# Membership is invariant to the m0 residual scale (only the intercept shifts).
check("jn membership is invariant to the m0 residual scale", jn_try({
  o1 <- logvar_joint_null_objective(fx$b_nonwit, fx$w1, fx$w2, fx$proj, di2)
  o2 <- logvar_joint_null_objective(fx$b_nonwit, 3 * fx$w1, 3 * fx$w2, fx$proj, di2)
  max(abs(o1$s - o2$s)) < 1e-8 &&
    abs((o2$theta[1] - o1$theta[1]) - 2 * log(3)) < 1e-8
}))
# Objective ordering survives a common positive rescale of coefficient and scale.
check("jn objective ordering survives a common positive rescale", jn_try({
  o1 <- logvar_joint_null_objective(fx$b_nonwit, fx$w1, fx$w2, fx$proj, di2)
  o2 <- logvar_joint_null_objective(fx$b_nonwit, fx$w1, fx$w2, fx$proj, di2 / 16)
  ow <- logvar_joint_null_objective(fx$b_star, fx$w1, fx$w2, fx$proj, di2 / 16)
  abs(o2$q - o1$q / 16) < 1e-10 * max(1, o1$q) && ow$q < o2$q
}))
