# Capture the frozen whiten = NULL reference for
# test_lambda_whitening.R: one unmasked ragged run and one masked
# run, stored without the support/whitening metadata fields so the
# comparison targets exactly the legacy surface. Run from the
# package root at the pre-whitening tree state (or on a new platform
# to re-pin the machine-local reference):
#   Rscript scripts/utils/tests/fixtures/capture_whiten_null_fixture.R
suppressMessages({
  library(hetid)
  library(nloptr)
})
source("scripts/utils/optimization_utils.R")
source("scripts/utils/profile_bounds_core.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/lambda_mask.R")
source("scripts/utils/lambda_optimization.R")

set.seed(33)
t_obs <- 80L
w1 <- rnorm(t_obs)
w2 <- matrix(rnorm(t_obs * 2), nrow = t_obs)
z <- matrix(rnorm(t_obs * 3), nrow = t_obs)
moments <- suppressMessages(compute_identification_moments(w1, w2, z))
lambda_start <- list(
  matrix(rnorm(6), nrow = 3),
  matrix(rnorm(3), nrow = 3)
)
tau <- rep(0.2, 2)

unmasked <- run_lambda_optimization(
  lambda_start, moments, tau,
  n_starts = 3, seed = 7, maxeval = 40L
)
support <- list(c(1L, 3L), 1:3)
masked_start <- lambda_from_support(
  support,
  list(matrix(c(0.6, 0.8), 2, 1), matrix(c(1, 0.5, -0.5), 3, 1)),
  j_total = 3
)
masked <- run_lambda_optimization(
  masked_start, moments, tau,
  n_starts = 3, seed = 11, maxeval = 40L, support = support
)

drop_meta <- function(out) {
  out[setdiff(names(out), c("support", "whitening"))]
}
saveRDS(
  list(unmasked = drop_meta(unmasked), masked = drop_meta(masked)),
  "scripts/utils/tests/fixtures/whiten_null_path_fixture.rds"
)
cat("fixture written\n")
