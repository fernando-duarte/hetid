# Joint support-mask x whitening tests: per-component sub-block
# factors over the supported instruments, exact +0.0 preservation
# off-support through whitened optimization, packed-length
# invariance, the all_results lambda-coordinate convention, and the
# full-support pin. Run from package root:
#   Rscript scripts/utils/tests/test_lambda_whitening_mask.R
suppressMessages({
  library(hetid)
  library(nloptr)
})
source("scripts/utils/optimization_utils.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/lambda_mask.R")
source("scripts/utils/lambda_whitening.R")
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
# Sign-sensitive zero test: 1 / +0.0 is Inf, 1 / -0.0 is -Inf
all_pos_zero <- function(x) all(1 / x == Inf)
drop_support <- function(out) out[setdiff(names(out), "support")]

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
v_corr <- crossprod(matrix(rnorm(12), 4, 3)) / 4 + diag(0.1, 3)

support <- list(c(1L, 3L), 1:3)
masked_start <- lambda_from_support(
  support,
  list(matrix(c(0.6, 0.8), 2, 1), matrix(c(1, 0.5, -0.5), 3, 1)),
  j_total = 3
)
out_mw <- run_lambda_optimization(
  masked_start, moments, tau,
  n_starts = 3, seed = 11, maxeval = 40L,
  support = support, whiten = list(vcov = v_corr)
)
dims_m <- lambda_dims(masked_start)
free_m <- support_free_mask(dims_m, out_mw$support)
check(
  "masked whitened runs pack only free elements",
  sum(free_m) == 5L &&
    all(lengths(lapply(out_mw$all_results, `[[`, "par")) == 5L)
)
# all_results pars are packed LAMBDA after the return-time decode,
# so plain unpack_active (no codec) must reproduce the optimum.
best_par <- out_mw$all_results[[out_mw$best_index]]$par
check(
  "all_results pars are packed lambda under whitening",
  identical(
    normalize_lambda_columns(unpack_active(best_par, dims_m, free_m)),
    out_mw$lambda_optimized
  )
)
per_start_zero <- vapply(out_mw$all_results, function(r) {
  ll <- unpack_active(r$par, dims_m, free_m)
  all_pos_zero(ll[[1]][2, ])
}, logical(1))
check(
  "off-support entries stay exact +0.0 through whitened optimization",
  all_pos_zero(out_mw$lambda_optimized[[1]][2, ]) &&
    all(per_start_zero)
)
wctx_m <- whiten_context(
  list(vcov = v_corr), out_mw$support, moments
)
check(
  "the whitening factor is the sub-block of the supported instruments",
  identical(dim(wctx_m$chol[[1]]), c(2L, 2L)) &&
    identical(dim(wctx_m$chol[[2]]), c(3L, 3L)) &&
    identical(wctx_m$chol[[1]], chol(v_corr[c(1, 3), c(1, 3)]))
)
par_mu <- pack_active(encode_lambda(masked_start, wctx_m), free_m)
back <- decode_lambda(par_mu, dims_m, free_m, wctx_m)
check(
  "masked codec round trip is exact off-support and tight on-support",
  all_pos_zero(back[[1]][2, ]) &&
    max(abs(unlist(back) - unlist(masked_start))) <=
      1e-12 * max(1, max(abs(unlist(masked_start))))
)

# Full support under whitening is bitwise the NULL-support whitened
# run (the whitened analog of the mask's vacuous-support pin).
out_w_null <- run_lambda_optimization(
  lambda_start, moments, tau,
  n_starts = 3, seed = 7, maxeval = 40L,
  whiten = list(vcov = v_corr)
)
out_w_full <- run_lambda_optimization(
  lambda_start, moments, tau,
  n_starts = 3, seed = 7, maxeval = 40L,
  support = list(1:3, 1:3), whiten = list(vcov = v_corr)
)
check(
  "full support under whitening reproduces the unmasked whitened run",
  identical(drop_support(out_w_full), drop_support(out_w_null))
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
