# Scale-law and conditioning tests: the spec's c^2 direction-scaling
# law (bitwise for power-of-two scalings; 1e-12 relative for
# arbitrary ones), raw vs whitened-then-mapped-back constraint
# equality up to that law, and a correlated-instrument fixture whose
# whitened/unwhitened comparison is PRINTED (informational) while
# the GATING check is a deterministic pin against the captured
# fixture outputs. The pin is machine-local; re-pin on a new
# platform with
#   Rscript scripts/utils/tests/fixtures/capture_whiten_conditioning_fixture.R
# Run from package root:
#   Rscript scripts/utils/tests/test_lambda_whitening_conditioning.R
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

dat <- make_whiten_conditioning_data()
moments_corr <- suppressMessages(
  compute_identification_moments(dat$w1, dat$w2, dat$z)
)
tau <- rep(0.5, 2)

# Scale law on the quadratic coefficients. Power-of-two scalings are
# bitwise: multiplying every addend by 4 shifts binary exponents
# only, and that scaling distributes exactly over the sums, products,
# differences, and same-denominator divisions in the kernel.
lambda_fix <- list(
  matrix(c(0.6, -0.2, 0.8, 0.1, 0.5, -0.7), nrow = 3),
  matrix(c(0.3, 0.4, -0.5), nrow = 3)
)
scale_cols <- function(ll, c_val) {
  lapply(ll, function(el) if (is.null(el)) NULL else c_val * el)
}
qs_base <- build_general_quadratic_system(
  lambda_fix, tau, moments_corr
)
qs_two <- build_general_quadratic_system(
  scale_cols(lambda_fix, 2), tau, moments_corr
)
n_con <- length(qs_base$quadratic$A_i)
bitwise_ok <- all(vapply(seq_len(n_con), function(k) {
  identical(qs_two$quadratic$A_i[[k]], 4 * qs_base$quadratic$A_i[[k]]) &&
    identical(qs_two$quadratic$b_i[[k]], 4 * qs_base$quadratic$b_i[[k]]) &&
    identical(qs_two$quadratic$c_i[[k]], 4 * qs_base$quadratic$c_i[[k]])
}, logical(1)))
check(
  "power-of-two column scaling rescales every constraint bitwise by c^2",
  bitwise_ok
)

# Arbitrary scalings hold to 1e-12 relative: each coefficient gains
# at most a few extra roundings (one per multiply), a handful of
# ulps; 1e-12 leaves orders of margin yet catches any convention
# error.
c_arb <- 0.3
qs_arb <- build_general_quadratic_system(
  scale_cols(lambda_fix, c_arb), tau, moments_corr
)
rel_close <- function(x, ref, tol) {
  max(abs(x - ref)) <= tol * max(1, max(abs(ref)))
}
arb_ok <- all(vapply(seq_len(n_con), function(k) {
  rel_close(
    qs_arb$quadratic$A_i[[k]],
    c_arb^2 * qs_base$quadratic$A_i[[k]], 1e-12
  ) &&
    rel_close(
      qs_arb$quadratic$b_i[[k]],
      c_arb^2 * qs_base$quadratic$b_i[[k]], 1e-12
    ) &&
    rel_close(
      qs_arb$quadratic$c_i[[k]],
      c_arb^2 * qs_base$quadratic$c_i[[k]], 1e-12
    )
}, logical(1)))
check(
  "arbitrary column scaling rescales constraints by c^2 within 1e-12",
  arb_ok
)

# Raw vs whitened-then-mapped-back: the mu-unit representative of a
# column is the raw column divided by ||R lambda||, so each
# constraint rescales by exactly 1 / ||R lambda||^2. Tolerance
# 1e-10: the mapped representative adds one triangular solve and one
# normalization (ulp-level error scaled by this fixture's modest
# sub-block condition number).
wctx <- whiten_context(list(z = dat$z), NULL, moments_corr)
mu_fix <- encode_lambda(lambda_fix, wctx)
col_scales <- unlist(lapply(mu_fix, function(el) {
  if (is.null(el)) numeric(0) else 1 / colSums(el^2)
}))
lam_white <- decode_lambda(
  pack_lambda(normalize_lambda_columns(mu_fix)),
  lambda_dims(mu_fix), NULL, wctx
)
qs_white <- build_general_quadratic_system(
  lam_white, tau, moments_corr
)
white_ok <- all(vapply(seq_len(n_con), function(k) {
  rel_close(
    qs_white$quadratic$A_i[[k]],
    col_scales[k] * qs_base$quadratic$A_i[[k]], 1e-10
  ) &&
    rel_close(
      qs_white$quadratic$b_i[[k]],
      col_scales[k] * qs_base$quadratic$b_i[[k]], 1e-10
    ) &&
    rel_close(
      qs_white$quadratic$c_i[[k]],
      col_scales[k] * qs_base$quadratic$c_i[[k]], 1e-10
    )
}, logical(1)))
check(
  "whitened-then-mapped-back weights rescale constraints by the direction law",
  white_ok
)
check(
  "mu-unit representatives are variance-normalized in lambda space",
  all(vapply(seq_along(lam_white), function(i) {
    if (is.null(lam_white[[i]])) {
      return(TRUE)
    }
    v_quad <- diag(
      crossprod(lam_white[[i]], wctx$vcov %*% lam_white[[i]])
    )
    all(abs(v_quad - 1) < 1e-10)
  }, logical(1)))
)

# Conditioning comparison, demonstrated not asserted: the printed
# line reports whether the whitened search did at least as well as
# the unwhitened one across the same seeds on this fixture
# (INFORMATIONAL -- a local heuristic carries no general-superiority
# guarantee, so no inequality is gated). The gating check is the
# deterministic pin against the captured fixture.
plain <- run_lambda_optimization(
  dat$start, moments_corr, tau,
  n_starts = 6, seed = 42, maxeval = 200L
)
white <- run_lambda_optimization(
  dat$start, moments_corr, tau,
  n_starts = 6, seed = 42, maxeval = 200L,
  whiten = list(z = dat$z)
)
cat(sprintf(
  "conditioning fixture: unwhitened %.6g | whitened %.6g | whitened <= unwhitened: %s\n",
  plain$objective_final, white$objective_final,
  white$objective_final <= plain$objective_final
))
fix <- readRDS(
  "scripts/utils/tests/fixtures/whiten_conditioning_fixture.rds"
)
check(
  "conditioning fixture outputs match the pinned capture",
  identical(plain$objective_final, fix$plain_final) &&
    identical(white$objective_final, fix$white_final) &&
    identical(plain$best_index, fix$plain_best_index) &&
    identical(white$best_index, fix$white_best_index)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
