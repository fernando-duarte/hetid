# Whitening plumbing tests for run_lambda_optimization: the frozen
# whiten = NULL reference, identity-covariance bitwise equivalence,
# the transform echo, codec round trips, and lambda-space output
# conventions. The fixture pin is machine-local (identical() over
# slsqp trajectories is BLAS-specific); re-pin on a new platform:
#   Rscript scripts/utils/tests/fixtures/capture_whiten_null_fixture.R
# Run from package root:
#   Rscript scripts/utils/tests/test_lambda_whitening.R
suppressMessages({
  library(hetid)
  library(nloptr)
})
source("scripts/utils/optimization_utils.R")
source("scripts/utils/profile_bounds_core.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/lambda_mask.R")
source("scripts/utils/lambda_whitening.R")
source("scripts/utils/lambda_varnorm.R")
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
drop_meta <- function(out) {
  out[setdiff(names(out), c("support", "whitening", "lambda_variance"))]
}

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

# Frozen pre-feature reference: whiten = NULL must reproduce it
# field-for-field under strict identical().
fixture <- readRDS(
  "scripts/utils/tests/fixtures/whiten_null_path_fixture.rds"
)
out_unmasked <- run_lambda_optimization(
  lambda_start, moments, tau,
  whiten = NULL,
  n_starts = 3, seed = 7, maxeval = 40L
)
check(
  "whiten = NULL reproduces the frozen pre-feature run",
  identical(drop_meta(out_unmasked), fixture$unmasked)
)
support <- list(c(1L, 3L), 1:3)
masked_start <- lambda_from_support(
  support,
  list(matrix(c(0.6, 0.8), 2, 1), matrix(c(1, 0.5, -0.5), 3, 1)),
  j_total = 3
)
out_masked <- run_lambda_optimization(
  masked_start, moments, tau,
  whiten = NULL,
  n_starts = 3, seed = 11, maxeval = 40L, support = support
)
check(
  "masked whiten = NULL reproduces the frozen pre-feature run",
  identical(drop_meta(out_masked), fixture$masked)
)

# Identity covariance: chol(I) = I, so encode/decode are exact
# identities and the whitened run must be bitwise equal to the
# unwhitened one -- the whitened analog of the mask's full-support
# pin.
out_id <- run_lambda_optimization(
  lambda_start, moments, tau,
  n_starts = 3, seed = 7, maxeval = 40L,
  whiten = list(vcov = diag(3))
)
check(
  "identity-covariance whitening reproduces the unwhitened run bitwise",
  identical(drop_meta(out_id), drop_meta(out_unmasked))
)
check(
  "the applied transform is echoed and absent when off",
  identical(out_id$whitening$source, "vcov") &&
    identical(out_id$whitening$ridge, 0) &&
    identical(out_id$whitening$jitter, 0) &&
    identical(out_id$whitening$vcov, diag(3)) &&
    is.null(out_unmasked$whitening)
)

# A genuinely correlated covariance: outputs stay in lambda space
# with unit-Euclidean columns, and honest objectives never leak the
# steering penalty.
v_corr <- crossprod(matrix(rnorm(12), 4, 3)) / 4 + diag(0.1, 3)
out_w <- run_lambda_optimization(
  lambda_start, moments, tau,
  n_starts = 3, seed = 7, maxeval = 40L,
  whiten = list(vcov = v_corr)
)
v_q <- function(el) diag(crossprod(el, v_corr %*% el))
check(
  "whitened optimized columns are variance-normalized in lambda space",
  is.infinite(out_w$objective_final) ||
    (all(abs(v_q(out_w$lambda_optimized[[1]]) - 1) < 1e-12) &&
      all(abs(v_q(out_w$lambda_optimized[[2]]) - 1) < 1e-12))
)
check(
  "whitened honest objectives are never the steering penalty",
  (is.infinite(out_w$objective_final) ||
    out_w$objective_final < UNBOUNDED_PENALTY) &&
    (is.infinite(out_w$objective_start) ||
      out_w$objective_start < UNBOUNDED_PENALTY)
)

# Codec round trip: decode(pack(encode(x))) recovers x to floating
# point (one triangular multiply plus one backsolve on a
# well-conditioned factor).
wctx <- whiten_context(list(vcov = v_corr), NULL, moments)
dims <- lambda_dims(lambda_start)
par_mu <- pack_active(encode_lambda(lambda_start, wctx), NULL)
back <- decode_lambda(par_mu, dims, NULL, wctx)
round_trip_err <- max(abs(unlist(back) - unlist(lambda_start)))
check(
  "encode/decode round trip recovers the start to 1e-12",
  round_trip_err <= 1e-12 * max(1, max(abs(unlist(lambda_start))))
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
