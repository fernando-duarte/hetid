# Freeze the pre-refactor quadratic outputs on synthetic seeded data.
# Run ONCE from the package root BEFORE the kernel extraction lands:
#   Rscript tests/testthat/fixtures/make-quadratic-kernel-fixture.R
# The committed RDS is the bitwise oracle for test-constraint_kernel.R.
# Generated at pre-refactor commit f0381fc on aarch64-apple-darwin
# (long.double FALSE); regenerating from any later commit makes the
# oracle tautological -- do not rerun this script.
set.seed(42)
n_obs <- 100
i_dim <- 3
j_dim <- 4
w1 <- rnorm(n_obs)
w2 <- matrix(rnorm(n_obs * i_dim), nrow = n_obs)
z <- matrix(
  rnorm(n_obs * j_dim),
  nrow = n_obs,
  dimnames = list(NULL, paste0("z", seq_len(j_dim)))
)
gamma <- matrix(rnorm(j_dim * i_dim), nrow = j_dim)
tau <- c(0, 0.1, 0.3)

moments_full <- hetid::compute_identification_moments(w1, w2, z)
moments_sub <- hetid::compute_identification_moments(
  w1, w2, z,
  maturities = c(1, 3)
)
saveRDS(
  list(
    w1 = w1, w2 = w2, z = z, gamma = gamma, tau = tau,
    qs_full = hetid::build_quadratic_system(gamma, tau, moments_full),
    qs_sub = hetid::build_quadratic_system(gamma, tau, moments_sub)
  ),
  file.path("tests", "testthat", "fixtures", "quadratic_kernel_fixture.rds"),
  version = 3
)
