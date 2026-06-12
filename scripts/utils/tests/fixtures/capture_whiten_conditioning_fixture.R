# Capture the pinned outputs for the whitening conditioning fixture
# (test_lambda_whitening_conditioning.R). Run from the package root
# AFTER the whitening feature is implemented (the capture exercises
# whiten = list(z = ...)); re-run on a new platform to re-pin the
# machine-local reference:
#   Rscript scripts/utils/tests/fixtures/capture_whiten_conditioning_fixture.R
suppressMessages({
  library(hetid)
  library(nloptr)
})
source("scripts/utils/optimization_utils.R")
source("scripts/utils/profile_bounds_core.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/lambda_mask.R")
source("scripts/utils/lambda_whitening.R")
source("scripts/utils/lambda_optimization.R")
source("scripts/utils/tests/fixtures/whiten_conditioning_dgp.R")

dat <- make_whiten_conditioning_data()
moments_corr <- suppressMessages(
  compute_identification_moments(dat$w1, dat$w2, dat$z)
)
tau <- rep(0.5, 2)
plain <- run_lambda_optimization(
  dat$start, moments_corr, tau,
  n_starts = 6, seed = 42, maxeval = 200L
)
white <- run_lambda_optimization(
  dat$start, moments_corr, tau,
  n_starts = 6, seed = 42, maxeval = 200L,
  whiten = list(z = dat$z)
)
if (!is.finite(plain$objective_final) ||
  !is.finite(white$objective_final)) {
  stop(
    "conditioning fixture produced an unbounded width; report this ",
    "to the user -- do NOT tune the DGP to force a result"
  )
}
saveRDS(
  list(
    plain_final = plain$objective_final,
    white_final = white$objective_final,
    plain_best_index = plain$best_index,
    white_best_index = white$best_index
  ),
  "scripts/utils/tests/fixtures/whiten_conditioning_fixture.rds"
)
cat(sprintf(
  "pinned: unwhitened %.6g | whitened %.6g | whitened <= unwhitened: %s\n",
  plain$objective_final, white$objective_final,
  white$objective_final <= plain$objective_final
))
