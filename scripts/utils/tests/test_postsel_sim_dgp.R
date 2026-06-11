# Post-selection simulation DGP tests. Run from package root:
#   Rscript scripts/utils/tests/test_postsel_sim_dgp.R
suppressMessages(library(hetid))
source("scripts/utils/postsel_sim_dgp.R")

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

params <- postsel_dgp_params(4L, phi = 0.5)
check(
  "sigma_nu solver hits the rho target from above",
  max(params$rho) <= params$rho_target + 1e-8 &&
    abs(max(params$rho) - params$rho_target) < 1e-6
)

# Large iid Monte Carlo of the epsilon-level moments (marginal
# moments do not depend on phi, so phi = 0 draws are a clean check).
# The eps1*eps2 products are heavy-tailed (lognormal scale, large
# sigma_nu), so the draw is sized for the correlation noise to sit
# well inside the constancy tolerance below.
params0 <- postsel_dgp_params(4L, phi = 0)
mc <- draw_postsel_data(params0, 1000000L, seed = 4)
x_mc <- cbind(1, mc$z)
eps2 <- mc$y2 - x_mc %*% t(params0$beta20)
eps1 <- drop(mc$y1) - drop(x_mc %*% params0$beta10) -
  drop(mc$y2 %*% params0$theta0)
ratio_at <- function(l) {
  zl <- drop(mc$z %*% l)
  abs(cor(zl, eps1 * eps2[, 1])) / abs(cor(zl, eps2[, 1]^2))
}
denom_at <- function(l) {
  abs(cor(drop(mc$z %*% l), eps2[, 1]^2))
}
set.seed(11)
dirs <- matrix(rnorm(4 * 12), 4)
keep <- apply(dirs, 2, denom_at) > 0.05
ratios <- apply(dirs[, keep, drop = FALSE], 2, ratio_at)
check(
  "relative correlation ratio is constant across directions",
  sum(keep) >= 6L && max(abs(ratios - mean(ratios))) < 0.02
)
check(
  "closed-form rho matches the Monte Carlo ratio",
  abs(mean(ratios) - params0$rho[1]) < 0.02
)

d1 <- draw_postsel_data(params, 100L, seed = 7)
d2 <- draw_postsel_data(params, 100L, seed = 7)
d3 <- draw_postsel_data(params, 100L, seed = 8)
check(
  "draws are seed-deterministic and seed-sensitive",
  identical(d1, d2) && !identical(d1, d3)
)

big <- draw_postsel_data(params, 20000L, seed = 5)
lag1 <- cor(big$z[-1, 1], big$z[-20000, 1])
check(
  "instruments keep unit marginal variance and target persistence",
  abs(sd(big$z[, 1]) - 1) < 0.05 && abs(lag1 - params$phi) < 0.05
)

# Population-scale membership: with the uniform bound holding at
# rho_target < tau, theta0 must sit inside the equal-weight set
m_pop <- sim_window_moments(mc, seq_len(100000L))
lam0 <- equal_weight_lambda(4L, 2L)
check(
  "population-scale sample puts theta0 inside the equal-weight set",
  covers_theta0(lam0, rep(0.2, 2), m_pop, params0$theta0)
)

# Window isolation for the simulation's per-window refit
datw <- draw_postsel_data(params, 80L, seed = 21)
rows_a <- 1:38
datw_bad <- datw
datw_bad$y1[41:80] <- 99
datw_bad$y2[41:80, ] <- -99
datw_bad$z[41:80, ] <- 3.3
check(
  "window moments use only their window",
  identical(
    sim_window_moments(datw, rows_a),
    sim_window_moments(datw_bad, rows_a)
  )
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
