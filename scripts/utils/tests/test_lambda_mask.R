# Support-mask mechanics tests for run_lambda_optimization: bitwise
# NULL-path equivalence, free-element packing, exact +0.0
# preservation, and the normalization claim. Run from package root:
#   Rscript scripts/utils/tests/test_lambda_mask.R
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
drop_support <- function(out) out[setdiff(names(out), "support")]
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

# A vacuous (full) support must reproduce the support = NULL run
# bit-for-bit: same pack length, same rnorm draw count, same slsqp
# trajectories. This pins the masked plumbing against perturbing the
# unmasked arithmetic.
out_null <- run_lambda_optimization(
  gamma_start, moments, tau,
  n_starts = 3, seed = 99, maxeval = 50L
)
out_full <- run_lambda_optimization(
  gamma_start, moments, tau,
  n_starts = 3, seed = 99, maxeval = 50L,
  support = list(1:3, 1:3)
)
check(
  "full support reproduces the support = NULL run bitwise",
  identical(drop_support(out_null), drop_support(out_full))
)
check(
  "support echo is NULL unmasked and canonical when masked",
  is.null(out_null$support) &&
    identical(out_full$support, list(1:3, 1:3))
)

# Masked optimization: off-support entries are exact +0.0 in the
# optimum and in every per-start terminal parameter vector, and only
# free elements are packed.
support <- list(c(1, 3), 2)
masked_start <- lambda_from_support(
  support, list(matrix(c(0.6, 0.8), 2, 1), matrix(1, 1, 1)),
  j_total = 3
)
out_masked <- run_lambda_optimization(
  masked_start, moments, tau,
  n_starts = 3, seed = 7, maxeval = 30L, support = support
)
dims <- lambda_dims(out_masked$lambda_optimized)
free <- support_free_mask(dims, out_masked$support)
check(
  "only free elements are packed under a mask",
  sum(free) == 3L &&
    all(lengths(lapply(out_masked$all_results, `[[`, "par")) == 3L)
)
check(
  "off-support entries of the optimum are exact +0.0",
  all_pos_zero(out_masked$lambda_optimized[[1]][2, ]) &&
    all_pos_zero(out_masked$lambda_optimized[[2]][c(1, 3), ])
)
per_start_zero <- vapply(out_masked$all_results, function(r) {
  ll <- unpack_active(r$par, dims, free)
  all_pos_zero(ll[[1]][2, ]) && all_pos_zero(ll[[2]][c(1, 3), ])
}, logical(1))
check(
  "every per-start terminal par unpacks with exact +0.0 off-support",
  all(per_start_zero)
)
check(
  "masked support echo is integer-coerced",
  identical(out_masked$support, list(c(1L, 3L), 2L))
)
check(
  "masked honest objectives are never the steering penalty",
  (is.infinite(out_masked$objective_final) ||
    out_masked$objective_final < UNBOUNDED_PENALTY) &&
    (is.infinite(out_masked$objective_start) ||
      out_masked$objective_start < UNBOUNDED_PENALTY)
)
check(
  "masked optimized columns are unit-norm when bounded",
  is.infinite(out_masked$objective_final) ||
    (abs(sum(out_masked$lambda_optimized[[1]]^2) - 1) < 1e-12 &&
      abs(sum(out_masked$lambda_optimized[[2]]^2) - 1) < 1e-12)
)

# Normalizing a padded matrix is bitwise identical to normalizing
# the compact free rows: zeros add exactly 0 to the column norms and
# 0 / c is exactly +0.0. This is the verified claim that lets the
# mask reuse normalize_lambda_columns unchanged.
padded <- matrix(c(0, 1, 2, 0, 3, 4), nrow = 3)
norm_padded <- normalize_lambda_columns(list(padded))[[1]]
norm_compact <- normalize_gamma_columns(padded[c(2, 3), , drop = FALSE])
check(
  "padded-column normalization preserves exact +0.0 bitwise",
  all_pos_zero(norm_padded[1, ]) &&
    identical(norm_padded[c(2, 3), ], norm_compact)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
