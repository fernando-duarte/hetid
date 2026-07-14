# Scan, polish, at-tau, nesting, language, singleton, L-infinity, and artifact
# checks for the joint-null diagnostic (log_var_eq_joint_null_search.R and the
# driver log_var_eq_joint_null.R), plus the projection-ownership and input-seam
# checks. Sourced by test_logvar_joint_null_helpers.R, which owns check(),
# jn_try, jn_fx, and the jn_ball/jn_fd_grad helpers. Every check is
# module-dependent and fails closed until the search and driver land. Error-path
# checks guard on exists() first so a missing module fails the check rather than
# passing on the wrong error.
fx <- jn_fx
di2 <- fx$d^-2
sd_mnj <- (1 / apply(fx$pcr_mnj, 2, stats::sd))^2
er_mnj <- rep(1.5, nrow(fx$pcr_mnj))
jn_at_tau <- function(b_tab, w1, w2, proj, d_inv2, eps_ref, qs, prior = list(),
                      tau = 0.05) {
  logvar_joint_null_at_tau(tau, tau, b_tab, w1, w2, proj, d_inv2, eps_ref, qs, prior)
}
jn_box <- function(center, rad) {
  list(set_lower = center - rad, set_upper = center + rad)
}
# Projection ownership: the driver's single owned 5 x n projection matches the map.
check("jn projection ownership returns the owned 5 x n matrix", jn_try({
  p <- logvar_joint_null_projection(fx$pcr, fx$qtr, fx$sample_id)
  identical(dim(p), c(5L, fx$n)) && max(abs(p - fx$proj)) < 1e-12 &&
    identical(rownames(p)[-1], colnames(fx$pcr))
}))
# Projection ownership fails closed on an unsorted qtr key (a stateless check:
# the wrapper owns no cache, so a descending qtr is caught on its own terms).
check("jn projection ownership fails closed on a misaligned design", jn_try({
  stopifnot(exists("logvar_joint_null_projection"))
  bad <- tryCatch(
    {
      logvar_joint_null_projection(fx$pcr, rev(fx$qtr), fx$sample_id)
      "ok"
    },
    error = function(e) "stopped"
  )
  identical(bad, "stopped")
}))
# Input seam: eps_ref is qtr-aligned, finite, positive-median, additive-only.
check("jn input producer adds a qtr-aligned finite eps_ref additively", jn_try({
  inp <- list(w1 = fx$w1, w2 = fx$w2, pcr = fx$pcr, qtr = fx$qtr)
  ext <- logvar_joint_null_extend_inputs(inp, fx$eps_ref, fx$qtr)
  length(ext$eps_ref) == fx$n && all(is.finite(ext$eps_ref)) &&
    median(abs(ext$eps_ref)) > 0 && identical(ext$w1, fx$w1) &&
    identical(ext$pcr, fx$pcr)
}))
# Input seam recovers aligned values after a qtr shuffle.
check("jn input producer recovers aligned values after a qtr shuffle", jn_try({
  o <- sample(fx$n)
  inp <- list(w1 = fx$w1[o], w2 = fx$w2[o, ], pcr = fx$pcr[o, ], qtr = fx$qtr[o])
  ext <- logvar_joint_null_extend_inputs(inp, fx$eps_ref[o], fx$qtr[o])
  length(ext$eps_ref) == fx$n && all(is.finite(ext$eps_ref))
}))
# Marginals-not-joint: every marginal straddles zero yet no joint witness exists.
check("jn marginals-not-joint attains positive distance, no witness", jn_try({
  row <- jn_at_tau(fx$b_tab_mnj, fx$w1_mnj, fx$w2_mnj, fx$proj_mnj, sd_mnj, er_mnj, fx$qs_mnj)
  row$scaled_l2 > 0 && identical(row$membership_result, "compatibility_not_demonstrated") &&
    identical(row$status, "bounded")
}))
# Tau nesting: the attained distance does not increase as the ball expands.
check("jn attained distance is nonincreasing across nested taus", jn_try({
  cen <- fx$b_c_mnj
  small <- jn_at_tau(
    jn_box(cen, 0.3), fx$w1_mnj, fx$w2_mnj, fx$proj_mnj, sd_mnj, er_mnj,
    jn_ball(cen, 0.3)
  )
  big <- jn_at_tau(fx$b_tab_mnj, fx$w1_mnj, fx$w2_mnj, fx$proj_mnj, sd_mnj, er_mnj,
    fx$qs_mnj,
    prior = list(c(small$b1, small$b2, small$b3))
  )
  big$scaled_l2 <= small$scaled_l2 + 1e-10
}))
# A fabricated nesting violation stops the run loudly.
check("jn a fabricated nesting violation stops the run", jn_try({
  stopifnot(exists("logvar_joint_null_at_tau"))
  msg <- tryCatch(
    {
      jn_at_tau(fx$b_tab_mnj, fx$w1_mnj, fx$w2_mnj, fx$proj_mnj, sd_mnj, er_mnj,
        fx$qs_mnj,
        prior = list(structure(fx$b_star, q_reported = -1))
      )
      "no_stop"
    },
    error = function(e) conditionMessage(e)
  )
  grepl("nest|monoton", msg, ignore.case = TRUE)
}))
# Row order: a consistent row and qtr shuffle leaves the at-tau output unchanged.
check("jn row shuffle leaves at-tau output identical after the qtr join", jn_try({
  o <- sample(fx$n)
  pcs <- fx$pcr[o, ]
  colnames(pcs) <- colnames(fx$pcr)
  ref <- jn_at_tau(jn_box(fx$b_star, 0.5), fx$w1, fx$w2, fx$proj, di2, fx$eps_ref, fx$qs_ball)
  sh <- jn_at_tau(
    jn_box(fx$b_star, 0.5), fx$w1[o], fx$w2[o, ], logvar_projection(pcs),
    di2, fx$eps_ref[o], fx$qs_ball
  )
  abs(ref$scaled_l2 - sh$scaled_l2) < 1e-8 && abs(ref$b1 - sh$b1) < 1e-6
}))
# No-witness language: no forbidden inference tokens except the disclaimer.
check("jn no-witness output avoids forbidden inference vocabulary", jn_try({
  row <- jn_at_tau(fx$b_tab_mnj, fx$w1_mnj, fx$w2_mnj, fx$proj_mnj, sd_mnj, er_mnj, fx$qs_mnj)
  out <- capture.output(print_logvar_joint_null(row, logvar_joint_null_scales(fx$pcr_mnj)))
  keep <- out[!grepl("not a test rejection", out, fixed = TRUE)]
  bad <- "reject(ed)?|excluded|incompatible|accept(ed)?|joint null false|p ?= ?0\\.|p-value"
  length(out) > 0 && !any(grepl(bad, tolower(keep)))
}))
# Constraint failure: a polished point outside the normalized set is discarded.
check("jn a polished point outside the set is discarded", jn_try({
  stopifnot(exists("logvar_joint_null_polish"))
  res <- logvar_joint_null_polish(
    fx$qs_ball, list(c(50, -50, 50), fx$b_star),
    fx$w1, fx$w2, fx$proj, di2
  )
  .feasibility_residual(fx$qs_ball, c(res$best$b), 1) <= 1e-4
}))
# Multiple basins: multi-start polish keeps the best feasible attained bound.
check("jn multi-start polish keeps the best feasible attained bound", jn_try({
  starts <- list(fx$b_star + c(0.1, 0, 0), fx$b_star - c(0.1, 0, 0), fx$b_star)
  res <- logvar_joint_null_polish(fx$qs_ball, starts, fx$w1, fx$w2, fx$proj, di2)
  res$best$q <= 1e-8 && res$successful_polishes >= 1L
}))
# Direct recomputation: the reported objective equals a fresh evaluation.
check("jn the reported objective equals a fresh recomputation", jn_try({
  row <- jn_at_tau(jn_box(fx$b_star, 0.5), fx$w1, fx$w2, fx$proj, di2, fx$eps_ref, fx$qs_ball)
  b <- c(row$b1, row$b2, row$b3)
  fresh <- logvar_joint_null_objective(b, fx$w1, fx$w2, fx$proj, di2)
  abs(2 * fresh$q - row$scaled_l2^2) < 1e-6
}))
# Singleton tau = 0: the Lewbel point carries the not-applicable statuses.
check("jn the tau = 0 singleton uses the not-applicable statuses", jn_try({
  row <- jn_at_tau(jn_box(fx$b_star, 0.5), fx$w1, fx$w2, fx$proj, di2, fx$eps_ref,
    fx$qs_ball,
    tau = 0
  )
  identical(row$replication_status, "not_applicable_singleton") &&
    identical(row$perturbation_status, "not_applicable_singleton")
}))
# L-infinity sensitivity: every row carries the trigger flag and a valid enum.
check("jn every row records an L-infinity trigger and disposition", jn_try({
  row <- jn_at_tau(fx$b_tab_mnj, fx$w1_mnj, fx$w2_mnj, fx$proj_mnj, sd_mnj, er_mnj, fx$qs_mnj)
  is.logical(row$linf_trigger) && row$linf_sensitivity_status %in%
    c("not_triggered", "polished", "attained_start_only", "unreliable_recompute")
}))
# Artifact schema: fixed labels and closed enums survive a version-3 RDS round trip.
check("jn assembled rows carry fixed labels and round-trip via saveRDS", jn_try({
  r1 <- jn_at_tau(fx$b_tab_mnj, fx$w1_mnj, fx$w2_mnj, fx$proj_mnj, sd_mnj, er_mnj, fx$qs_mnj)
  r2 <- jn_at_tau(fx$b_tab_mnj, fx$w1_mnj, fx$w2_mnj, fx$proj_mnj, sd_mnj, er_mnj,
    fx$qs_mnj,
    tau = 0.1
  )
  obj <- list(
    schema_version = "1.0.0", estimator = "logols",
    diagnostic = "joint_null_theta_r", inference_status = "deferred",
    rows = list(r1, r2)
  )
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(obj, path, version = 3)
  back <- readRDS(path)
  identical(back, obj) && identical(r1$estimator, "logols") &&
    identical(r1$diagnostic, "joint_null_theta_r") &&
    r1$status %in% c("bounded", "unreliable")
}))
