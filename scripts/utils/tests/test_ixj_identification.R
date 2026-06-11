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

# Full-system guard: a subset container (constraint axis smaller than the
# system) cannot supply one constraint per gamma column and is rejected.
moments_subset <- suppressMessages(
  compute_identification_moments(w1, w2, pcs, maturities = c(1, 3))
)
err <- tryCatch(
  build_ixj_quadratic_system(
    moments_subset, matrix(0.2, nrow = n_pcs, ncol = n_comp)
  ),
  error = function(e) e
)
check("rejects subset (non-full-system) moments", inherits(err, "error"))

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

# Frozen pre-rewrite implementation: the oracle the wrapper must
# reproduce bit for bit (guards omitted; inputs here are valid).
legacy_ixj_oracle <- function(moments, tau_matrix) {
  n_pcs <- nrow(moments$r_i_0)
  n_components <- attr(moments, "n_components")
  total <- n_pcs * n_components
  a_list <- vector("list", total)
  b_list <- vector("list", total)
  c_vec <- numeric(total)
  d_vec <- numeric(total)
  comp_idx <- integer(total)
  inst_idx <- integer(total)
  pos <- 0L
  for (j in seq_len(n_pcs)) {
    gamma_j <- make_basis_gamma(j, n_pcs, n_components)
    qs_j <- suppressMessages(
      build_quadratic_system(gamma_j, tau_matrix[j, ], moments)
    )$quadratic
    for (i in seq_len(n_components)) {
      pos <- pos + 1L
      a_list[[pos]] <- qs_j$A_i[[i]]
      b_list[[pos]] <- qs_j$b_i[[i]]
      c_vec[pos] <- qs_j$c_i[i]
      d_vec[pos] <- qs_j$d_i[i]
      comp_idx[pos] <- i
      inst_idx[pos] <- j
    }
  }
  list(
    quadratic = list(
      A_i = a_list, b_i = b_list, c_i = c_vec, d_i = d_vec
    ),
    labels = data.frame(
      constraint = seq_len(total),
      component = comp_idx,
      instrument = inst_idx,
      stringsAsFactors = FALSE
    )
  )
}

set.seed(21)
w1_oracle <- rnorm(60)
w2_oracle <- matrix(rnorm(60 * 3), nrow = 60)
z_oracle <- matrix(rnorm(60 * 4), nrow = 60)
moments_oracle <- suppressMessages(
  compute_identification_moments(w1_oracle, w2_oracle, z_oracle)
)
tau_oracle <- matrix(runif(12, 0.05, 0.5), nrow = 4)
check(
  "wrapper reproduces the frozen legacy ixj implementation exactly",
  identical(
    build_ixj_quadratic_system(moments_oracle, tau_oracle),
    legacy_ixj_oracle(moments_oracle, tau_oracle)
  )
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0) quit(status = 1)
