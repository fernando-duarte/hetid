# Offline checks for the joint-null theta_R = 0 distance diagnostic
# in the joint_null module directory. This entrypoint mirrors the Harvey suite: source
# the map/engine/log-OLS/polish and joint-null layers, define
# check() + the fail-closed jn_try wrapper, build the self-validating jn_fx
# fixtures, then source the crossing and pipeline check files. Run from root:
#   Rscript scripts-paper/tests/diagnostics/joint_null/test_joint_null.R

source(file.path("scripts-paper", "config", "paths.R"))
source(paper_path("config", "artifacts.R"))
source(paper_path("support", "identification", "profile_solver_core.R"))
source(paper_path("support", "identification", "profile_bounds_api.R"))
source(paper_path("support", "identification", "tau_star.R"))
source(paper_path("log_variance", "core", "residual_map.R"))
source(paper_path("log_variance", "engine", "api.R"))
source(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))
source(paper_path("log_variance", "core", "endpoint_polish.R"))

# The required joint-null modules.
jn_modules <- vapply(c(
  "inputs.R", "distance_objective.R", "search.R", "stability.R", "run.R"
), function(file) {
  paper_path("log_variance", "diagnostics", "joint_null", file)
}, character(1))
for (file in jn_modules) {
  source(file)
}

.pass <- 0L
.fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    .pass <<- .pass + 1L
    cat(sprintf("PASS  %s\n", label))
  } else {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s\n", label))
  }
}
# Fail a check closed when a required joint-null function errors.
jn_try <- function(expr) tryCatch(isTRUE(expr), error = function(e) FALSE)

root_tol <- 1e-6
# One-constraint ball qs: ||b - center|| <= radius (feasible => residual < 0).
jn_ball <- function(center, radius) {
  list(
    A_i = list(diag(length(center))), b_i = list(-2 * center),
    c_i = sum(center^2) - radius^2
  )
}
# Exactly centered PC design with the l.pc1..l.pc4 names the helpers assert.
jn_centered <- function(mat) {
  out <- scale(mat, center = TRUE, scale = FALSE)
  attr(out, "scaled:center") <- NULL
  colnames(out) <- paste0("l.pc", seq_len(ncol(out)))
  out
}
# Central-difference gradient of a scalar objective f at b.
jn_fd_grad <- function(f, b, h = 1e-6) {
  vapply(seq_along(b), function(k) {
    e <- replace(numeric(length(b)), k, h)
    (f(b + e) - f(b - e)) / (2 * h)
  }, numeric(1))
}

# Shared deterministic fixtures. Each sub-fixture asserts its defining invariant
# so a broken construction errors loudly at source time (correct, not a red).
jn_fx <- local({
  d_of <- function(pcr) 1 / apply(pcr, 2, stats::sd)
  slopes_at <- function(b, w1, w2, proj) logvar_theta_hat(b, w1, w2, proj)[-1]
  # Witness: constant residual magnitude => constant log(e^2) => zero slopes.
  set.seed(20260714L)
  n <- 60L
  k <- 3L
  qtr <- seq_len(n)
  sample_id <- "jn-fixture-v1"
  pcr <- jn_centered(matrix(rnorm(n * 4L), n, 4L))
  proj <- logvar_projection(pcr)
  d <- d_of(pcr)
  b_star <- c(0.3, -0.2, 0.15)
  w2 <- matrix(rnorm(n * k), n, k)
  e_star <- 1.5 * sample(c(-1, 1), n, TRUE)
  w1 <- drop(w2 %*% b_star) + e_star
  qs_ball <- jn_ball(b_star, 0.5)
  b_nonwit <- b_star + c(0.2, 0, 0)
  eps_ref <- rnorm(n)
  stopifnot(
    max(abs(slopes_at(b_star, w1, w2, proj))) < 1e-10,
    .feasibility_residual(qs_ball, b_star, 1) < 0,
    .feasibility_residual(qs_ball, b_nonwit, 1) < 0,
    min(abs(drop(w1 - w2 %*% b_nonwit))) > 0.3,
    max(abs(slopes_at(b_nonwit, w1, w2, proj) * d_of(pcr))) > root_tol,
    median(abs(eps_ref)) > 0
  )
  # Marginals-not-joint: every slope straddles zero over the ball grid, but the
  # joint scaled sup-norm stays away from zero (seed 38).
  set.seed(38L)
  n_mnj <- 40L
  pcr_mnj <- jn_centered(matrix(rnorm(n_mnj * 4L), n_mnj, 4L))
  proj_mnj <- logvar_projection(pcr_mnj)
  w2_mnj <- matrix(rnorm(n_mnj * 3L), n_mnj, 3L)
  b_c_mnj <- c(0.2, -0.1, 0.15)
  rad_mnj <- 0.6
  w1_mnj <- drop(w2_mnj %*% b_c_mnj) + sample(c(-1, 1), n_mnj, TRUE) * runif(n_mnj, 1, 3)
  qs_mnj <- jn_ball(b_c_mnj, rad_mnj)
  b_tab_mnj <- list(set_lower = b_c_mnj - rad_mnj, set_upper = b_c_mnj + rad_mnj)
  grid_mnj <- logvar_feasible_grid(qs_mnj, b_tab_mnj$set_lower, b_tab_mnj$set_upper, 13L)
  eps_mnj <- w1_mnj - w2_mnj %*% t(grid_mnj)
  sl_mnj <- (proj_mnj %*% log(eps_mnj^2))[-1, , drop = FALSE]
  dinf_mnj <- apply(abs(sl_mnj / d_of(pcr_mnj)), 2, max)
  stopifnot(
    all(apply(sl_mnj, 1, min) < 0), all(apply(sl_mnj, 1, max) > 0),
    min(dinf_mnj) > 0.05, min(abs(eps_mnj)) > 0.1
  )
  # Leverage crossing: row i_lev crosses exactly with nonzero slope leverage.
  set.seed(24L)
  n_lev <- 45L
  pcr_lev <- jn_centered(matrix(rnorm(n_lev * 4L), n_lev, 4L))
  proj_lev <- logvar_projection(pcr_lev)
  w2_lev <- matrix(rnorm(n_lev * 3L), n_lev, 3L)
  b_cross_lev <- c(0.3, -0.2, 0.15)
  w1_lev <- drop(w2_lev %*% b_cross_lev) + 1.2 * sample(c(-1, 1), n_lev, TRUE)
  i_lev <- which.max(apply(abs(proj_lev[-1, , drop = FALSE]), 2, max))
  w1_lev[i_lev] <- drop(w2_lev[i_lev, ] %*% b_cross_lev)
  v_lev <- w2_lev[i_lev, ] / sqrt(sum(w2_lev[i_lev, ]^2))
  e_lev0 <- drop(w1_lev - w2_lev %*% b_cross_lev)
  stopifnot(
    max(abs(proj_lev[-1, i_lev])) > 0.05,
    abs(e_lev0[i_lev]) < 1e-12, sort(abs(e_lev0))[2] > 0.5
  )
  # Zero-leverage crossing: row 1 is the exact zero PC vector, so its slope
  # leverage vanishes while the intercept weight stays 1/n.
  set.seed(22L)
  v_zl <- matrix(rnorm(30 * 4L), 30, 4L)
  pcr_zlev <- rbind(0, rbind(v_zl, -v_zl))
  colnames(pcr_zlev) <- paste0("l.pc", 1:4)
  n_zlev <- nrow(pcr_zlev)
  proj_zlev <- logvar_projection(pcr_zlev)
  i_zlev <- 1L
  w2_zlev <- matrix(rnorm(n_zlev * 3L), n_zlev, 3L)
  b_cross_zlev <- c(0.25, -0.15, 0.2)
  w1_zlev <- drop(w2_zlev %*% b_cross_zlev) + 1.3 * sample(c(-1, 1), n_zlev, TRUE)
  w1_zlev[i_zlev] <- drop(w2_zlev[i_zlev, ] %*% b_cross_zlev)
  v_zlev <- w2_zlev[i_zlev, ] / sqrt(sum(w2_zlev[i_zlev, ]^2))
  stopifnot(
    max(abs(proj_zlev[-1, i_zlev])) < 1e-10,
    abs(proj_zlev[1, i_zlev] - 1 / n_zlev) < 1e-10
  )
  # Cancellation: two identical crossing rows with anti-parallel slope leverage,
  # so their divergent parts cancel near a puncture (small norm, tiny e).
  set.seed(33L)
  u_can <- rnorm(4L)
  pcr_can <- rbind(u_can, -u_can, matrix(rnorm(14 * 4L), 14, 4L))
  pcr_can <- rbind(pcr_can, -pcr_can[-(1:2), ])
  colnames(pcr_can) <- paste0("l.pc", 1:4)
  n_can <- nrow(pcr_can)
  proj_can <- logvar_projection(pcr_can)
  i_can1 <- 1L
  i_can2 <- 2L
  w2_can <- matrix(rnorm(n_can * 3L), n_can, 3L)
  w2_can[i_can2, ] <- w2_can[i_can1, ]
  b_cross_can <- c(0.3, -0.2, 0.15)
  w1_can <- drop(w2_can %*% b_cross_can) + 1.5 * sample(c(-1, 1), n_can, TRUE)
  w1_can[i_can1] <- drop(w2_can[i_can1, ] %*% b_cross_can) + 1e-9
  w1_can[i_can2] <- drop(w2_can[i_can2, ] %*% b_cross_can) + 1e-9
  g1 <- proj_can[-1, i_can1]
  g2 <- proj_can[-1, i_can2]
  e_can <- drop(w1_can - w2_can %*% b_cross_can)
  d2_can <- sqrt(sum((slopes_at(b_cross_can, w1_can, w2_can, proj_can) / d_of(pcr_can))^2))
  stopifnot(
    sum(g1 * g2) / sqrt(sum(g1^2) * sum(g2^2)) < -0.99,
    d2_can < 10 * root_tol, min(abs(e_can)) < 1e-6 * median(abs(e_can))
  )
  # Puncture in the band: the same anti-parallel _can design but with the crossing
  # offset lifted into (1e-8, 1e-6] * e_scale_ref (median|eps_ref| = 1.5), so the
  # machine-adjacency gate no longer pre-empts the two-endpoint puncture signature.
  w1_pnc <- w1_can
  w1_pnc[i_can1] <- drop(w2_can[i_can1, ] %*% b_cross_can) + 1e-6
  w1_pnc[i_can2] <- drop(w2_can[i_can2, ] %*% b_cross_can) + 1e-6
  e_pnc <- drop(w1_pnc - w2_can %*% b_cross_can)
  stopifnot(min(abs(e_pnc)) > 1e-8 * 1.5, min(abs(e_pnc)) <= 1e-6 * 1.5)
  # Return the fixture environment so every binding above is reachable as fx$*.
  environment()
})

source(paper_path("tests", "diagnostics", "joint_null", "crossing_checks.R"))
source(paper_path("tests", "diagnostics", "joint_null", "pipeline_checks.R"))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
