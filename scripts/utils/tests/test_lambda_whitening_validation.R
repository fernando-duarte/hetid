# Whitening input-form validation tests for run_lambda_optimization:
# structured rejections of malformed whiten inputs, raised before
# any RNG use or optimization. Run from package root:
#   Rscript scripts/utils/tests/test_lambda_whitening_validation.R
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

set.seed(33)
t_obs <- 80L
w1 <- rnorm(t_obs)
w2 <- matrix(rnorm(t_obs * 2), nrow = t_obs)
z <- matrix(rnorm(t_obs * 3), nrow = t_obs)
moments <- suppressMessages(compute_identification_moments(w1, w2, z))
gamma_start <- matrix(rnorm(6), nrow = 3)
tau <- rep(0.2, 2)

expect_whiten_error <- function(label, whiten_arg,
                                class = "hetid_error",
                                support_arg = NULL,
                                moments_in = moments,
                                start = gamma_start) {
  err <- tryCatch(
    {
      run_lambda_optimization(
        start, moments_in, tau,
        n_starts = 2, seed = 99, maxeval = 10L,
        support = support_arg, whiten = whiten_arg
      )
      NULL
    },
    error = function(e) e
  )
  check(label, inherits(err, class))
}

# Rejections fire BEFORE set.seed and any optimization: a failed
# call must leave the global RNG state untouched.
set.seed(2024)
rng_reference <- rnorm(1)
set.seed(2024)
expect_whiten_error(
  "a bare matrix whiten input is rejected with guidance",
  diag(3),
  class = "hetid_error_bad_argument"
)
rng_after <- rnorm(1)
check(
  "whiten validation precedes set.seed and all optimization",
  identical(rng_reference, rng_after)
)
expect_whiten_error(
  "both z and vcov together are rejected",
  list(z = z, vcov = diag(3)),
  class = "hetid_error_bad_argument"
)
expect_whiten_error(
  "a whiten list without z or vcov is rejected",
  list(ridge = 0.1),
  class = "hetid_error_bad_argument"
)
expect_whiten_error(
  "unknown whiten fields are rejected (no Cholesky-factor form)",
  list(chol = diag(3)),
  class = "hetid_error_bad_argument"
)
expect_whiten_error(
  "z with the wrong column count is rejected",
  list(z = z[, 1:2])
)
expect_whiten_error(
  "a single-row z is rejected",
  list(z = z[1, , drop = FALSE]),
  class = "hetid_error_bad_argument"
)
expect_whiten_error(
  "a wrong-sized vcov is rejected",
  list(vcov = diag(2))
)
expect_whiten_error(
  "an asymmetric vcov is rejected",
  list(vcov = matrix(c(1, 0.5, 0, 0, 1, 0, 0, 0, 1), 3, 3)),
  class = "hetid_error_bad_argument"
)
expect_whiten_error(
  "a negative ridge is rejected",
  list(vcov = diag(3), ridge = -1e-6),
  class = "hetid_error_bad_argument"
)
expect_whiten_error(
  "a vector ridge is rejected",
  list(vcov = diag(3), ridge = c(1e-6, 1e-6)),
  class = "hetid_error_bad_argument"
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
