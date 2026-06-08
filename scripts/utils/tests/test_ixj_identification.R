# I x J identified-set constructor tests. Run from package root:
#   Rscript scripts/utils/tests/test_ixj_identification.R
suppressMessages({
  library(hetid)
  library(nloptr)
})
source("scripts/utils/identification_utils.R")
source("scripts/utils/profile_bounds.R")
source("scripts/utils/ixj_identification.R")

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
mats_equal <- function(xs, ys, tol = 1e-10) {
  length(xs) == length(ys) &&
    all(mapply(function(a, b) max(abs(a - b)) < tol, xs, ys))
}

# Synthetic reduced-form residuals + instruments.
set.seed(7)
t_obs <- 200L
n_comp <- 3L
n_pcs <- 4L
w1 <- rnorm(t_obs)
w2 <- matrix(rnorm(t_obs * n_comp), nrow = t_obs, ncol = n_comp)
pcs <- matrix(rnorm(t_obs * n_pcs), nrow = t_obs, ncol = n_pcs)
moments <- suppressMessages(compute_identification_moments(w1, w2, pcs))

# Single-instrument reproduction: with J = 1, build_ixj must reproduce exactly
# build_quadratic_system with gamma = e_1 (a 1 x I all-ones matrix).
moments1 <- suppressMessages(
  compute_identification_moments(w1, w2, pcs[, 1, drop = FALSE])
)
ixj1 <- build_ixj_quadratic_system(moments1, matrix(0.2, nrow = 1, ncol = n_comp))
ref1 <- suppressMessages(
  build_quadratic_system(make_basis_gamma(1, 1, n_comp), rep(0.2, n_comp), moments1)
)$quadratic
check(
  "J=1 reproduces build_quadratic_system(gamma = e_1)",
  length(ixj1$quadratic$A_i) == n_comp &&
    mats_equal(ixj1$quadratic$A_i, ref1$A_i) &&
    mats_equal(ixj1$quadratic$b_i, ref1$b_i) &&
    max(abs(ixj1$quadratic$c_i - ref1$c_i)) < 1e-10 &&
    max(abs(ixj1$quadratic$d_i - ref1$d_i)) < 1e-10
)

# Constraint count is exactly I*J; labels map each constraint to (component, instrument).
ixj <- build_ixj_quadratic_system(moments, matrix(0.2, nrow = n_pcs, ncol = n_comp))
check(
  "J>1 constraint count = I*J",
  length(ixj$quadratic$A_i) == n_comp * n_pcs &&
    nrow(ixj$labels) == n_comp * n_pcs &&
    setequal(ixj$labels$instrument, seq_len(n_pcs)) &&
    setequal(ixj$labels$component, seq_len(n_comp))
)

# Every A_ij acts on theta in R^I, so profiling yields exactly I intervals.
bnds <- solve_all_profile_bounds(ixj$quadratic)
check("solve_all_profile_bounds returns I rows", nrow(bnds) == n_comp)

# Per-(i,j) tolerances: tau_matrix entry (j, i) drives d for instrument j, component i.
tau_mat <- matrix(seq_len(n_pcs * n_comp) / 100, nrow = n_pcs, ncol = n_comp)
ixj_tau <- build_ixj_quadratic_system(moments, tau_mat)
# d_ij scales with tau_ij^2; reconstruct expected d for the (component, instrument) map.
expected_d <- mapply(function(i, j) {
  suppressMessages(
    build_quadratic_system(make_basis_gamma(j, n_pcs, n_comp), tau_mat[j, ], moments)
  )$quadratic$d_i[i]
}, ixj_tau$labels$component, ixj_tau$labels$instrument)
check(
  "per-(i,j) tau_matrix wired through to d_ij",
  max(abs(ixj_tau$quadratic$d_i - expected_d)) < 1e-12
)

# Position-indexing guard: maturity-value-named moments are rejected.
moments_bad <- moments
colnames(moments_bad$r_i_0) <- c("maturity_2", "maturity_5", "maturity_9")
err <- tryCatch(
  build_ixj_quadratic_system(moments_bad, matrix(0.2, nrow = n_pcs, ncol = n_comp)),
  error = function(e) e
)
check("rejects maturity-value-named moments", inherits(err, "error"))

# tau_matrix shape guard.
err2 <- tryCatch(
  build_ixj_quadratic_system(moments, matrix(0.2, nrow = n_comp, ncol = n_pcs)),
  error = function(e) e
)
check("rejects wrong-shape tau_matrix", inherits(err2, "error"))

# Monotonicity: intersecting more constraints gives weakly TIGHTER intervals.
# Unit disk + radius-0.5 disk in R^2 -> [-0.5, 0.5]; dropping the tighter disk
# widens to the unit disk [-1, 1].
q_full <- list(
  A_i = list(diag(2), diag(2)), b_i = list(c(0, 0), c(0, 0)), c_i = c(-1, -0.25)
)
q_sub <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = c(-1))
w_full <- solve_all_profile_bounds(q_full)$width
w_sub <- solve_all_profile_bounds(q_sub)$width
check(
  "monotonicity: fewer constraints weakly wider",
  all(w_sub >= w_full - 1e-6) && any(w_sub > w_full + 1e-3)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
