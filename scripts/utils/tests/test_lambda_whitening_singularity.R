# Whitening name-alignment and singularity tests for
# run_lambda_optimization: the instrument-name cross-check, the
# structured singularity rejection with its explicit opt-in ridge
# rescue (echoed, never silent), and sub-block whitening on masked
# supports. Run from package root:
#   Rscript scripts/utils/tests/test_lambda_whitening_singularity.R
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

set.seed(33)
t_obs <- 80L
w1 <- rnorm(t_obs)
w2 <- matrix(rnorm(t_obs * 2), nrow = t_obs)
z <- matrix(rnorm(t_obs * 3), nrow = t_obs)
moments <- suppressMessages(compute_identification_moments(w1, w2, z))
gamma_start <- matrix(rnorm(6), nrow = 3)
tau <- rep(0.2, 2)

# Instrument-name cross-check: enforced only when both sides carry
# names; positional inputs stay accepted.
z_named <- z
colnames(z_named) <- c("pc_a", "pc_b", "pc_c")
moments_named <- suppressMessages(
  compute_identification_moments(w1, w2, z_named)
)
z_bad_names <- z_named
colnames(z_bad_names) <- c("pc_a", "pc_b", "pc_x")
check(
  "the moments container carries instrument names",
  identical(rownames(moments_named$r_i_0), c("pc_a", "pc_b", "pc_c"))
)
err_names <- tryCatch(
  run_lambda_optimization(
    gamma_start, moments_named, tau,
    n_starts = 2, seed = 99, maxeval = 10L,
    whiten = list(z = z_bad_names)
  ),
  error = function(e) e
)
check(
  "mismatched instrument names are rejected",
  inherits(err_names, "hetid_error_bad_argument")
)
out_pos <- run_lambda_optimization(
  gamma_start, moments_named, tau,
  n_starts = 2, seed = 5, maxeval = 10L,
  whiten = list(z = z)
)
check(
  "an unnamed z against named moments is accepted positionally",
  is.list(out_pos$whitening)
)

# Singularity: a rank-deficient vcov errors with guidance; an
# explicit ridge rescues it and is echoed (relative jitter applied
# once to the full matrix).
v_singular <- tcrossprod(matrix(rnorm(6), 3, 2))
err_sing <- tryCatch(
  run_lambda_optimization(
    gamma_start, moments, tau,
    n_starts = 2, seed = 5, maxeval = 10L,
    whiten = list(vcov = v_singular)
  ),
  error = function(e) e
)
check(
  "a singular vcov is rejected with ridge guidance",
  inherits(err_sing, "hetid_error_bad_argument") &&
    grepl("ridge", conditionMessage(err_sing))
)
out_ridge <- run_lambda_optimization(
  gamma_start, moments, tau,
  n_starts = 2, seed = 5, maxeval = 10L,
  whiten = list(vcov = v_singular, ridge = 1e-6)
)
check(
  "an explicit ridge rescues a singular vcov and is echoed",
  identical(out_ridge$whitening$ridge, 1e-6) &&
    out_ridge$whitening$jitter > 0 &&
    isTRUE(all.equal(
      out_ridge$whitening$vcov,
      (v_singular + t(v_singular)) / 2 +
        diag(1e-6 * mean(diag(v_singular)), 3)
    ))
)

# Sub-block semantics: a covariance singular on the FULL instrument
# set whitens cleanly when the mask drops the collinear instrument,
# and still errors when the mask keeps it.
z_dup <- cbind(z[, 1], z[, 1], z[, 3])
moments_dup <- suppressMessages(
  compute_identification_moments(w1, w2, z_dup)
)
support_sub <- list(c(1L, 3L), c(1L, 3L))
start_sub <- lambda_from_support(
  support_sub,
  list(matrix(c(0.6, 0.8), 2, 1), matrix(c(1, -1), 2, 1)),
  j_total = 3
)
out_sub <- run_lambda_optimization(
  start_sub, moments_dup, tau,
  n_starts = 2, seed = 5, maxeval = 10L,
  support = support_sub, whiten = list(z = z_dup)
)
check(
  "sub-block whitening succeeds when the mask drops the collinear instrument",
  is.list(out_sub$whitening) &&
    (is.infinite(out_sub$objective_final) ||
      out_sub$objective_final < UNBOUNDED_PENALTY)
)
err_full <- tryCatch(
  run_lambda_optimization(
    gamma_start, moments_dup, tau,
    n_starts = 2, seed = 5, maxeval = 10L,
    whiten = list(z = z_dup)
  ),
  error = function(e) e
)
check(
  "full-support whitening on collinear instruments is rejected",
  inherits(err_full, "hetid_error_bad_argument")
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
