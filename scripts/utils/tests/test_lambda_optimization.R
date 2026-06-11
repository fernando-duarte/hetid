# Lambda optimization tests. Run from package root:
#   Rscript scripts/utils/tests/test_lambda_optimization.R
suppressMessages({
  library(hetid)
  library(nloptr)
})
source("scripts/utils/optimization_utils.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/lambda_optimization.R")

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

# Single-combination equivalence: with a K = 1 start and the same
# seed, the general optimizer must reproduce the legacy gamma
# optimizer exactly (same pack order, same rnorm draw count, same
# objective surface via the builder equivalence).
set.seed(33)
t_obs <- 80L
w1 <- rnorm(t_obs)
w2 <- matrix(rnorm(t_obs * 2), nrow = t_obs)
z <- matrix(rnorm(t_obs * 3), nrow = t_obs)
moments <- suppressMessages(compute_identification_moments(w1, w2, z))
gamma_start <- matrix(rnorm(6), nrow = 3)
tau <- rep(0.2, 2)

legacy <- run_gamma_optimization(
  gamma_start, moments, tau,
  n_starts = 3, seed = 99, maxeval = 50L
)
general <- run_lambda_optimization(
  gamma_start, moments, tau,
  n_starts = 3, seed = 99, maxeval = 50L
)
check(
  "K=1 lambda optimization equals the legacy gamma optimizer",
  identical(
    do.call(cbind, general$lambda_optimized),
    unname(legacy$gamma_optimized)
  ) &&
    identical(general$objective_final, legacy$objective_final) &&
    identical(general$best_index, legacy$best_index) &&
    identical(general$objective_start, legacy$objective_start)
)

# Ragged multi-combination run: shapes survive the pack/unpack round
# trip and optimized columns come back unit-norm (exact-zero terminal
# columns would error inside the builder and land at the honest Inf,
# so the norm check applies to the returned bounded solution).
lambda_start <- list(
  matrix(rnorm(6), nrow = 3),
  matrix(rnorm(3), nrow = 3)
)
out <- run_lambda_optimization(
  lambda_start, moments, rep(0.2, 2),
  n_starts = 2, seed = 7, maxeval = 30L
)
check(
  "multi-combination optimization returns the ragged shapes",
  identical(dim(out$lambda_optimized[[1]]), c(3L, 2L)) &&
    identical(dim(out$lambda_optimized[[2]]), c(3L, 1L)) &&
    length(out$duplicate_directions) == 2L
)
check(
  "optimized combination columns are unit-norm",
  all(abs(colSums(out$lambda_optimized[[1]]^2) - 1) < 1e-12) &&
    all(abs(colSums(out$lambda_optimized[[2]]^2) - 1) < 1e-12)
)

# Subset-maturity containers: a full-size matrix start must be
# accepted with its unconstrained columns dropped. Note the seeded
# K=1 trajectory equivalence holds only for FULL-system containers
# (the general optimizer packs fewer parameters on subsets -- fewer
# rnorm draws -- so values legitimately differ from the legacy
# optimizer there; honesty, not equality, is the contract).
moments_sub <- suppressMessages(
  compute_identification_moments(w1, w2, z, maturities = 2)
)
general_sub <- run_lambda_optimization(
  gamma_start, moments_sub, tau,
  n_starts = 2, seed = 11, maxeval = 30L
)
check(
  "subset-maturity matrix start is accepted and honestly evaluated",
  is.infinite(general_sub$objective_final) ||
    general_sub$objective_final < UNBOUNDED_PENALTY
)

# Honest reporting: the final objective is the true width or Inf --
# NEVER the finite steering penalty. (A plain >= 0 check would pass
# even if the penalty leaked through; compare against the penalty.)
set.seed(34)
w1_s <- rnorm(40)
w2_s <- matrix(rnorm(40), nrow = 40)
z_s <- matrix(rnorm(40), nrow = 40)
moments_s <- suppressMessages(
  compute_identification_moments(w1_s, w2_s, z_s)
)
out_s <- run_lambda_optimization(
  list(matrix(1, 1, 1)), moments_s, 0.99,
  n_starts = 1, seed = 1, maxeval = 10L
)
check(
  "honest objective is never the steering penalty",
  is.infinite(out_s$objective_final) ||
    out_s$objective_final < UNBOUNDED_PENALTY
)
check(
  "honest start objective is never the steering penalty",
  is.infinite(out_s$objective_start) ||
    out_s$objective_start < UNBOUNDED_PENALTY
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
