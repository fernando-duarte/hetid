# Re-test beta2R = 0 with references that survive serial dependence and
# heteroskedasticity, instead of the iid F I ran first. Newey-West Wald per
# equation, plus a null-imposed circular moving-block bootstrap for the joint
# matrix restriction. No new package dependencies: HAC is written out.
source("/private/tmp/claude-502/-Users-fduarte-Library-CloudStorage-Dropbox-Personal-MyPackages-hetid/511ff928-e438-4f98-a805-111cac88d6d2/scratchpad/pilot_setup.R")

X <- cbind(1, as.matrix(dat[xc]))
Y <- as.matrix(dat[yc])
n <- nrow(X)
k <- ncol(X)

# Newey-West long-run covariance of the OLS coefficients, Bartlett kernel.
nw_vcov <- function(x, u, lag) {
  xtx_inv <- solve(crossprod(x))
  h <- x * u
  s <- crossprod(h)
  for (l in seq_len(lag)) {
    w <- 1 - l / (lag + 1)
    g <- crossprod(h[-seq_len(l), , drop = FALSE], h[seq_len(n - l), , drop = FALSE])
    s <- s + w * (g + t(g))
  }
  xtx_inv %*% s %*% xtx_inv
}

lag_nw <- floor(4 * (n / 100)^(2 / 9))
cat(sprintf("T = %d,  Newey-West lag = %d\n\n", n, lag_nw))

wald_one <- function(j) {
  fit <- stats::lm.fit(X, Y[, j])
  b <- fit$coefficients
  v <- nw_vcov(X, fit$residuals, lag_nw)
  sel <- 2:k                                   # slopes only, drop intercept
  drop(t(b[sel]) %*% solve(v[sel, sel]) %*% b[sel])
}

cat("=== per-equation HAC Wald on the three PC_E slopes ===\n")
w_obs <- vapply(seq_along(yc), wald_one, numeric(1))
for (j in seq_along(yc)) {
  cat(sprintf(
    "%-14s  Wald = %7.3f   chisq(3) p = %.4e   iid-F p (earlier) = %s\n",
    yc[j], w_obs[j], stats::pchisq(w_obs[j], k - 1, lower.tail = FALSE),
    c("9.22e-01", "6.87e-01", "5.41e-04")[j]
  ))
}
joint_obs <- sum(w_obs)
cat(sprintf("\njoint (sum of the three Walds) = %.3f\n", joint_obs))
cat(sprintf("naive chisq(9) p = %.4e   <- ignores cross-equation dependence\n",
            stats::pchisq(joint_obs, 3 * (k - 1), lower.tail = FALSE)))

# Null-imposed WILD BLOCK bootstrap. The null being tested is beta2R = 0, i.e.
# E[X u] = 0, so the resampling DGP must satisfy it by construction or the
# p-value means nothing. Flipping residual signs by block imposes E[u|X] = 0
# while each residual stays paired with its own row of X, so the design
# alignment survives and |u_hat_t| does too -- Var(u*|X) keeps whatever
# relationship the data has, which a slope test should not discard.
#
# REJECTED ALTERNATIVE, do not reintroduce: resampling the restricted residuals
# under circular-MBB row indices independently of X. That imposes full
# independence, not zero projection, so it also destroys the nonlinear and
# conditional-variance dependence the null permits. Its reference is too wide:
# it put the joint restriction at p = 0.0917 where this one puts it at 0.0425,
# i.e. it hid a rejection.
cat("\n=== null-imposed wild block bootstrap (B = 20,000, block 10) ===\n")
B <- 20000L
Y_c <- scale(Y, center = TRUE, scale = FALSE)
mu <- attr(Y_c, "scaled:center")
pb <- function(obs, draws) (1 + sum(draws >= obs)) / (length(draws) + 1)

set.seed(boot_seed)
blk <- 10L
nb <- ceiling(n / blk)
block_of <- rep(seq_len(nb), each = blk)[seq_len(n)]
wild <- vapply(seq_len(B), function(b) {
  v <- sample(c(-1, 1), nb, replace = TRUE)[block_of]
  Yb <- sweep(Y_c * v, 2, mu, "+")
  ws <- vapply(seq_along(yc), function(j) {
    fit <- stats::lm.fit(X, Yb[, j])
    bb <- fit$coefficients
    vv <- nw_vcov(X, fit$residuals, lag_nw)
    sel <- 2:k
    drop(t(bb[sel]) %*% solve(vv[sel, sel]) %*% bb[sel])
  }, numeric(1))
  c(ws, sum(ws))
}, numeric(length(yc) + 1L))

for (j in seq_along(yc)) {
  cat(sprintf("%-14s  wild-block p = %.4f\n", yc[j], pb(w_obs[j], wild[j, ])))
}
cat(sprintf("%-14s  wild-block p = %.4f\n", "JOINT", pb(joint_obs, wild[4, ])))

cat("\n=== verdict ===\n")
p3 <- pb(w_obs[3], wild[3, ])
pj <- pb(joint_obs, wild[4, ])
mc <- function(p) 1.96 * sqrt(p * (1 - p) / B)
cat(sprintf("news PC3         p = %.4f +/- %.4f  rejects at 5%%: %s\n", p3, mc(p3), p3 < 0.05))
cat(sprintf("joint restriction p = %.4f +/- %.4f  rejects at 5%%: %s\n", pj, mc(pj), pj < 0.05))
cat(sprintf("PC3 Bonferroni (x3) = %.4f            rejects at 5%%: %s\n",
            min(1, 3 * p3), min(1, 3 * p3) < 0.05))
