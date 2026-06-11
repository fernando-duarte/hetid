# Support-mask validation tests for run_lambda_optimization:
# structured rejections raised before any RNG use or optimization,
# and the matrix-start-plus-mask path on subset-maturity containers.
# Run from package root:
#   Rscript scripts/utils/tests/test_lambda_mask_validation.R
suppressMessages({
  library(hetid)
  library(nloptr)
})
source("scripts/utils/optimization_utils.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/lambda_mask.R")
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
# Sign-sensitive zero test: 1 / +0.0 is Inf, 1 / -0.0 is -Inf, and
# identical() with its default num.eq = TRUE treats 0 and -0 as equal
all_pos_zero <- function(x) all(1 / x == Inf)

set.seed(33)
t_obs <- 80L
w1 <- rnorm(t_obs)
w2 <- matrix(rnorm(t_obs * 2), nrow = t_obs)
z <- matrix(rnorm(t_obs * 3), nrow = t_obs)
moments <- suppressMessages(compute_identification_moments(w1, w2, z))
gamma_start <- matrix(rnorm(6), nrow = 3)
tau <- rep(0.2, 2)

# Rejections are structured errors raised BEFORE set.seed and any
# optimization: a failed call must leave the global RNG state
# untouched.
expect_mask_error <- function(label, bad_support,
                              moments_in = moments,
                              start = gamma_start) {
  err <- tryCatch(
    {
      run_lambda_optimization(
        start, moments_in, tau,
        n_starts = 2, seed = 99, maxeval = 10L,
        support = bad_support
      )
      NULL
    },
    error = function(e) e
  )
  check(label, inherits(err, "hetid_error_bad_argument"))
}

set.seed(2024)
rng_reference <- rnorm(1)
set.seed(2024)
expect_mask_error(
  "fully masked column is rejected with a structured error",
  list(integer(0), 1:3)
)
rng_after <- rnorm(1)
check(
  "mask validation precedes set.seed and all optimization",
  identical(rng_reference, rng_after)
)
expect_mask_error(
  "nonzero off-support start entries are rejected, not zeroed",
  list(c(1, 3), 2)
)
expect_mask_error(
  "out-of-range support indices are rejected",
  list(c(1, 4), 1:3)
)
expect_mask_error(
  "duplicate support indices are rejected",
  list(c(1, 1), 1:3)
)
expect_mask_error(
  "fractional support indices are rejected",
  list(1.5, 1:3)
)
expect_mask_error(
  "wrong-length support lists are rejected",
  list(1:3)
)

# Subset-maturity containers: support NULL pattern must match the
# container's constrained columns; a matrix start plus mask works.
moments_sub <- suppressMessages(
  compute_identification_moments(w1, w2, z, maturities = 2)
)
expect_mask_error(
  "support at an unconstrained column is rejected",
  list(1:3, 1:3),
  moments_in = moments_sub
)
sub_start <- matrix(0, nrow = 3, ncol = 2)
sub_start[c(1, 3), 2] <- c(0.6, 0.8)
out_sub <- run_lambda_optimization(
  sub_start, moments_sub, tau,
  n_starts = 2, seed = 11, maxeval = 30L,
  support = list(NULL, c(1, 3))
)
check(
  "matrix start plus mask works on subset-maturity containers",
  is.null(out_sub$lambda_optimized[[1]]) &&
    all_pos_zero(out_sub$lambda_optimized[[2]][2, ]) &&
    identical(out_sub$support, list(NULL, c(1L, 3L)))
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
