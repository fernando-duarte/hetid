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

# Null-imposed circular MBB: rebuild each news PC as its own mean plus a
# block-resampled residual-from-the-mean, so beta2R = 0 holds by construction in
# every replicate, then recompute the same statistics.
cat("\n=== null-imposed circular MBB (block 10, same protocol as the paper) ===\n")
B <- 20000L
idx <- paper_mbb_index_family(B, n, 10L, boot_seed, "primary")$indices
Y_c <- scale(Y, center = TRUE, scale = FALSE)
mu <- attr(Y_c, "scaled:center")

boot_stats <- vapply(seq_len(B), function(b) {
  Yb <- sweep(Y_c[idx[[b]], , drop = FALSE], 2, mu, "+")
  ws <- vapply(seq_along(yc), function(j) {
    fit <- stats::lm.fit(X, Yb[, j])
    bb <- fit$coefficients
    v <- nw_vcov(X, fit$residuals, lag_nw)
    sel <- 2:k
    drop(t(bb[sel]) %*% solve(v[sel, sel]) %*% bb[sel])
  }, numeric(1))
  c(ws, sum(ws))
}, numeric(length(yc) + 1L))

pb <- function(obs, draws) (1 + sum(draws >= obs)) / (length(draws) + 1)
for (j in seq_along(yc)) {
  cat(sprintf("%-14s  bootstrap p = %.4f\n", yc[j], pb(w_obs[j], boot_stats[j, ])))
}
cat(sprintf("%-14s  bootstrap p = %.4f\n", "JOINT", pb(joint_obs, boot_stats[4, ])))

cat("\n=== verdict ===\n")
p3 <- pb(w_obs[3], boot_stats[3, ])
pj <- pb(joint_obs, boot_stats[4, ])
cat(sprintf("news PC3 rejects at 5%%: %s\n", p3 < 0.05))
cat(sprintf("joint restriction rejects at 5%%: %s\n", pj < 0.05))
cat(sprintf("Bonferroni-adjusted PC3 p (x3): %.4f -> rejects at 5%%: %s\n",
            min(1, 3 * p3), min(1, 3 * p3) < 0.05))

# Wild BLOCK bootstrap: imposes only E[u|X] = 0 (the actual null) rather than
# full X-Y independence, and keeps every residual paired with its own X_t so the
# design alignment and conditional heteroskedasticity survive. Rademacher
# multiplier held constant within blocks of 10 to retain serial dependence.
cat("\n=== wild block bootstrap (imposes the zero-projection null only) ===\n")
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
pw <- pb(w_obs[3], wild[3, ])
cat(sprintf("\nPC3 Bonferroni (x3) = %.4f -> rejects at 5%%: %s\n",
            min(1, 3 * pw), min(1, 3 * pw) < 0.05))
