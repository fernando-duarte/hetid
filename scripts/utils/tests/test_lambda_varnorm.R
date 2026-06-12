# Variance-normalization pins for run_lambda_optimization: the repo
# default convention lambda' Vhat lambda = 1 on every returned
# column, set invariance across reporting representatives, seeded
# determinism, the required-whiten contract, the structured
# zero-column rejection BEFORE any RNG use, the identification-
# strength diagnostic echo, and the Z/moments consistency guard.
# Run from package root: Rscript scripts/utils/tests/test_lambda_varnorm.R
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
source("scripts/utils/z_source.R")
source("scripts/utils/tests/fixtures/whiten_conditioning_dgp.R")

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
all_pos_zero <- function(x) all(1 / x == Inf)

dat <- make_whiten_conditioning_data()
moments <- suppressMessages(
  compute_identification_moments(dat$w1, dat$w2, dat$z)
)
tau <- rep(0.5, 2)
wctx_chk <- whiten_context(list(z = dat$z), NULL, moments)
qf <- function(el, rows) {
  block <- wctx_chk$vcov[rows, rows, drop = FALSE]
  sub <- el[rows, , drop = FALSE]
  diag(crossprod(sub, block %*% sub))
}

# whiten is REQUIRED: omitting it must error instructively, not fall
# back to a silent Euclidean run.
miss_err <- tryCatch(
  run_lambda_optimization(dat$start, moments, tau, n_starts = 2),
  error = function(e) conditionMessage(e)
)
check(
  "omitting whiten errors instructively",
  is.character(miss_err) && grepl("whiten is required", miss_err)
)

out <- run_lambda_optimization(
  dat$start, moments, tau,
  whiten = list(z = dat$z),
  n_starts = 4, seed = 42, maxeval = 100L
)
check(
  "whitened run is bounded on the conditioning DGP",
  is.finite(out$objective_final)
)
check(
  "every returned column satisfies lambda' V lambda = 1 to 1e-12",
  all(vapply(seq_along(out$lambda_optimized), function(i) {
    el <- out$lambda_optimized[[i]]
    is.null(el) || all(abs(qf(el, seq_len(nrow(el))) - 1) < 1e-12)
  }, logical(1)))
)

# Set invariance: bounds from the variance-normalized optimum equal
# bounds from the Euclidean-normalized SAME directions. The c^2
# constraint-level law is pinned exactly in
# test_lambda_whitening_conditioning.R; solved bounds add inner-slsqp
# wobble, so compare at 1e-6 relative (the profile solver's xtol_rel
# scale) -- a convention error would show at O(1).
qs_var <- build_general_quadratic_system(out$lambda_optimized, tau, moments)
qs_eu <- build_general_quadratic_system(
  normalize_lambda_columns(out$lambda_optimized), tau, moments
)
b_var <- solve_all_profile_bounds(qs_var$quadratic)
b_eu <- solve_all_profile_bounds(qs_eu$quadratic)
rel_ok <- function(x, y) {
  all(abs(x - y) <= 1e-6 * pmax(1, abs(y)))
}
check(
  "identified-set bounds are invariant to the reporting representative",
  rel_ok(b_var$lower, b_eu$lower) && rel_ok(b_var$upper, b_eu$upper)
)

out_again <- run_lambda_optimization(
  dat$start, moments, tau,
  whiten = list(z = dat$z),
  n_starts = 4, seed = 42, maxeval = 100L
)
check(
  "seeded whitened runs are deterministic",
  identical(out, out_again)
)

# Diagnostic echo: per-column lambda' V lambda of the unit-Euclidean
# representative, for the start and the optimum; absent (NULL) in the
# explicit whiten = NULL plumbing mode.
diag_indep <- lapply(out$lambda_optimized, function(el) {
  if (is.null(el)) NULL else qf(normalize_gamma_columns(el), seq_len(nrow(el)))
})
check(
  "lambda_variance echoes the unit-direction instrument variances",
  !is.null(out$lambda_variance) &&
    all(unlist(out$lambda_variance$start) > 0) &&
    max(abs(
      unlist(out$lambda_variance$optimized) - unlist(diag_indep)
    )) < 1e-12
)
out_plain <- run_lambda_optimization(
  dat$start, moments, tau,
  whiten = NULL,
  n_starts = 2, seed = 42, maxeval = 30L
)
check(
  "lambda_variance is NULL in the explicit whiten = NULL mode",
  is.null(out_plain$lambda_variance)
)

# Zero-column rejection: structured error, raised BEFORE the
# optimizer's set.seed -- the ambient RNG stream must be untouched.
bad_start <- list(matrix(0, 3, 1), matrix(c(1, 0.5, -0.5), 3, 1))
set.seed(777)
ref_draw <- rnorm(1)
set.seed(777)
err <- tryCatch(
  run_lambda_optimization(
    bad_start, moments, tau,
    whiten = list(z = dat$z),
    n_starts = 2, seed = 1, maxeval = 10L
  ),
  error = function(e) e
)
post_draw <- rnorm(1)
check(
  "zero start column raises a structured hetid_error",
  inherits(err, "hetid_error") &&
    grepl("instrument variance", conditionMessage(err))
)
check(
  "the rejection happens before any RNG use",
  identical(ref_draw, post_draw)
)

# Masked whitened run: off-support entries stay exact +0.0 and the
# on-support block is variance-normalized against the V sub-block.
support <- list(c(1L, 3L), 1:3)
masked_start <- lambda_from_support(
  support,
  list(matrix(c(0.6, 0.8), 2, 1), matrix(c(1, 0.5, -0.5), 3, 1)),
  j_total = 3
)
out_m <- run_lambda_optimization(
  masked_start, moments, tau,
  whiten = list(z = dat$z),
  n_starts = 3, seed = 11, maxeval = 60L, support = support
)
check(
  "masked whitened optimum keeps exact +0.0 off-support",
  is.infinite(out_m$objective_final) ||
    all_pos_zero(out_m$lambda_optimized[[1]][2, ])
)
check(
  "masked columns are variance-normalized on the support sub-block",
  is.infinite(out_m$objective_final) ||
    (all(abs(qf(out_m$lambda_optimized[[1]], c(1L, 3L)) - 1) < 1e-12) &&
      all(abs(qf(out_m$lambda_optimized[[2]], 1:3) - 1) < 1e-12))
)

# Z/moments consistency guard (used by the stage-05 rebuild path):
# passes on the true matrix, fires on same-shape different values.
check(
  "z/moments consistency guard passes on the true matrix",
  identical(
    assert_z_matches_moments(dat$z, dat$w1, dat$w2, moments), dat$z
  )
)
z_alt <- dat$z
z_alt[, 1] <- z_alt[, 1] * 2
guard_err <- tryCatch(
  assert_z_matches_moments(z_alt, dat$w1, dat$w2, moments),
  error = function(e) conditionMessage(e)
)
check(
  "z/moments consistency guard fires on same-shape different values",
  is.character(guard_err) && grepl("HETID_Z_SOURCE", guard_err)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
