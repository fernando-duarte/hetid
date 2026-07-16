# Deterministic offline fixtures for the joint moment-compatibility layer
# logvar-joint-gmm). Balanced designs with fixed squared-innovation multisets, never
# random finite-sample positivity: the sixteen {-1,+1}^4 sign cells each carry a
# squared-innovation multiset so common slopes and separate intercepts are exact in
# sample (dossier sections 4, 9). Alongside sit the shape-varying block, the
# midpoint-normal Jensen grid, the residualized-z redundancy/transform/tied cases, a
# non-centered derivative block with n, K, p all unequal, and the degenerate and
# overflow toys. Every sub-fixture asserts its defining invariant with base R plus the
# existing map helpers so a broken construction errors loudly at source time. Returned
# as the environment jg_fx; sourced by test_joint_gmm.R before the check files.

# One-constraint ball qs: ||b - center|| <= radius (feasible => residual < 0).
jg_ball <- function(center, radius) {
  list(
    A_i = list(diag(length(center))), b_i = list(-2 * center),
    c_i = sum(center^2) - radius^2
  )
}
# Exactly centered PC design with the l.pc1..l.pc4 names the moment layer expects.
jg_centered <- function(mat) {
  out <- scale(mat, center = TRUE, scale = FALSE)
  attr(out, "scaled:center") <- NULL
  colnames(out) <- paste0("l.pc", seq_len(ncol(out)))
  out
}
# Central-difference gradient of a scalar objective f at x.
jg_fd_grad <- function(f, x, h = 1e-6) {
  vapply(seq_along(x), function(k) {
    e <- replace(numeric(length(x)), k, h)
    (f(x + e) - f(x - e)) / (2 * h)
  }, numeric(1))
}
# Residual span check: fraction of z outside span(X) (base R, no joint module).
jg_span_resid <- function(z, x_mat) {
  fit <- lm.fit(x_mat, z)
  max(abs(fit$residuals))
}

jg_fx <- local({
  root_tol <- 1e-8
  # Balanced block: sixteen sign cells, each carrying the squared multiset m so
  # e^2 = m per cell => a_L = mean(log m), a_P = log(mean m), slopes exactly zero.
  m <- c(0.25, 0.5, 1.5, 1.75)
  cells <- as.matrix(expand.grid(rep(list(c(-1, 1)), 4L)))
  dimnames(cells) <- NULL
  pcr <- cells[rep(seq_len(16L), each = 4L), ]
  colnames(pcr) <- paste0("l.pc", 1:4)
  n <- nrow(pcr)
  x_mat <- cbind("(Intercept)" = 1, pcr)
  proj <- logvar_projection(pcr)
  e_star <- rep(sqrt(m), 16L) * rep(c(1, -1, 1, -1), 16L)
  set.seed(20260714L)
  k <- 3L
  w2 <- matrix(rnorm(n * k), n, k)
  b_star <- c(0.3, -0.2, 0.15)
  w1 <- drop(w2 %*% b_star) + e_star
  qtr <- seq_len(n)
  a_l_bal <- mean(log(m))
  gap_bal <- log(mean(m)) - mean(log(m))
  b_fd <- b_star + c(0.05, -0.03, 0.02)
  th_star <- logvar_theta_hat(b_star, w1, w2, proj)
  # All-zero response: e(b_star) forced to zero on every row (structural degeneracy).
  w1_az <- drop(w2 %*% b_star)
  # Log-sum-exp overflow: R beta near +/-700 with the balanced positive response.
  beta_ovf <- c(700, 0, 0, 0)
  stopifnot(
    abs(mean(m) - 1) < 1e-12, max(abs(th_star[-1])) < 1e-10,
    abs(th_star[1] - a_l_bal) < 1e-10, gap_bal > 0.2,
    min(abs(drop(w1 - w2 %*% b_fd))) > 0.1,
    max(abs(drop(w1_az - w2 %*% b_star))) == 0,
    max(abs(drop(x_mat %*% c(0, beta_ovf)))) > 690
  )

  # Shape-varying block: multiset A where R1 = +1, wider multiset B where R1 = -1.
  # Both average to one, so PPML slopes stay flat, but the mean-log gap exceeds 0.2
  # so the log slope on pc1 moves: common slopes fail and the joint criterion > 0.
  m_b <- c(0.05, 0.05, 1.95, 1.95)
  e_sh <- rep(c(1, -1, 1, -1), 16L) *
    ifelse(pcr[, 1] > 0, rep(sqrt(m), 16L), rep(sqrt(m_b), 16L))
  b_sh <- b_star
  w1_sh <- drop(w2 %*% b_sh) + e_sh
  meanlog_gap <- abs(mean(log(m_b)) - mean(log(m)))
  th_sh <- logvar_theta_hat(b_sh, w1_sh, w2, proj)
  stopifnot(
    abs(mean(m_b) - 1) < 1e-12, meanlog_gap > 0.2, abs(th_sh[2]) > 0.05
  )

  # Jensen grid: 4096 midpoint-normal quantiles normalized to mean square one, so
  # log(mean u^2) - mean(log u^2) reproduces the single-sourced Gaussian gap.
  jen_u <- stats::qnorm((seq_len(4096L) - 0.5) / 4096L)
  jen_u <- jen_u / sqrt(mean(jen_u^2))
  jen_gap <- log(mean(jen_u^2)) - mean(log(jen_u^2))
  stopifnot(abs(mean(jen_u^2) - 1) < 1e-12, abs(jen_gap - 1.270362845) < 0.01)

  # Residualized-z cases on the balanced X: redundant (in span X), a genuine two
  # column instrument, its well-conditioned transforms, and a near-tied pair.
  a_red <- matrix(c(1, -2, 0.5, 0, 1, 0, 1, -1, 0.3, 2), 5L, 2L)
  z_redundant <- x_mat %*% a_red
  z_multi <- cbind(cos(qtr / 3), sin(qtr / 5) + qtr / n)
  z_scaled <- z_multi %*% diag(c(3, 0.1))
  z_sign <- z_multi %*% diag(c(1, -1))
  z_perm <- z_multi[, c(2, 1)]
  z_mix <- z_multi %*% matrix(c(1, 0.4, -0.3, 1), 2L, 2L)
  z_tied <- cbind(z_multi[, 1], z_multi[, 1] + 1e-8 * z_multi[, 2])
  stopifnot(
    jg_span_resid(z_redundant, x_mat) < 1e-8,
    jg_span_resid(z_multi, x_mat) > 0.1,
    jg_span_resid(z_tied[, 2, drop = FALSE] - z_tied[, 1], x_mat) < 0.1
  )

  # Non-centered derivative block: n, K, p all unequal (30, 2, 4) and R not centered,
  # so da_L/dbeta = -colMeans(R) is nonzero and orientation slips are exposed.
  set.seed(41L)
  nc_n <- 30L
  nc_r <- matrix(rnorm(nc_n * 3L), nc_n, 3L) + 0.7
  colnames(nc_r) <- paste0("l.pc", 1:3)
  nc_x <- cbind("(Intercept)" = 1, nc_r)
  nc_w2 <- matrix(rnorm(nc_n * 2L), nc_n, 2L)
  nc_b <- c(0.2, -0.1)
  nc_beta <- c(0.3, -0.2, 0.15)
  nc_w1 <- drop(nc_w2 %*% nc_b) + sample(c(-1, 1), nc_n, TRUE) * runif(nc_n, 0.8, 2)
  stopifnot(
    min(abs(drop(nc_w1 - nc_w2 %*% nc_b))) > 0.1,
    max(abs(colMeans(nc_r))) > 0.3, nc_n != 2L, nc_n != 4L
  )

  # Identity payload pieces for joint_input_id sensitivity checks.
  sample_id <- "jg-fixture-v1"
  gamma <- w2
  tau <- c(0.05, 0.1, 0.2, 0.4)
  mean_systems <- list(jg_ball(b_star, 0.5))
  qs_ball <- jg_ball(b_star, 0.5)
  grid_b <- logvar_feasible_grid(qs_ball, b_star - 0.5, b_star + 0.5, 5L)
  stopifnot(nrow(grid_b) > 5L)

  environment()
})
