# Gamma-optimization selection tests: the reported objectives must be HONEST
# (Inf or a genuine bounded+valid width), never the inner steering penalty.
# Run from package root:
#   Rscript scripts/utils/tests/test_optimization_utils.R
suppressMessages({
  library(nloptr)
  library(hetid)
})
source("scripts/utils/profile_bounds.R")
source("scripts/utils/optimization_utils.R")

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

# Real moments from small synthetic data via the package container.
set.seed(42)
t_obs <- 80L
w1 <- rnorm(t_obs)
w2 <- matrix(rnorm(t_obs * 2), ncol = 2)
pcs <- matrix(rnorm(t_obs * 4), ncol = 4)
moments <- hetid::compute_identification_moments(w1, w2, pcs)
tau <- rep(0.5, 2)
gamma_start <- matrix(rnorm(4 * 2), nrow = 4, ncol = 2)

res <- run_gamma_optimization(
  gamma_start, moments, tau,
  n_starts = 2, seed = 1, maxeval = 25L
)

# The report is either Inf (no bounded+valid gamma found) or exactly the
# honest width of the returned gamma -- by construction it can never read the
# steering penalty.
check(
  "objective_final equals honest_width(gamma_optimized) (or both Inf)",
  isTRUE(all.equal(
    res$objective_final,
    honest_width(res$gamma_optimized, tau, moments)
  ))
)
check(
  "objective_final is Inf or finite -- never the steering penalty",
  (identical(res$objective_final, Inf) || is.finite(res$objective_final)) &&
    !isTRUE(all.equal(res$objective_final, UNBOUNDED_PENALTY))
)
check(
  "objective_start equals honest_width of the start gamma",
  isTRUE(all.equal(
    res$objective_start,
    honest_width(gamma_start, tau, moments)
  ))
)
check(
  "honest_width returns Inf or a finite nonnegative width",
  identical(honest_width(gamma_start, tau, moments), Inf) ||
    (is.finite(honest_width(gamma_start, tau, moments)) &&
      honest_width(gamma_start, tau, moments) >= 0)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
