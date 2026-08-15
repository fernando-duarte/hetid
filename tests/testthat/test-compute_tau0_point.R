# Tests for compute_tau0_point, the closed-form point solver at tau = 0

make_point_components <- function(qmat, lvec) {
  # build through the real internal constructor so the fixture cannot drift
  # from the container's invariants: V_i is a named numeric vector (not a
  # list), and L_i/V_i/Q_i all carry maturity_N names
  nms <- paste0("maturity_", seq_len(nrow(qmat)))
  hetid:::new_hetid_components(
    L_i = stats::setNames(lvec, nms),
    V_i = stats::setNames(rep(1, nrow(qmat)), nms),
    Q_i = stats::setNames(
      lapply(seq_len(nrow(qmat)), function(i) qmat[i, ]),
      nms
    ),
    maturities = seq_len(nrow(qmat)), n_components = ncol(qmat)
  )
}

test_that("full-rank consistent system returns the solve() solution", {
  set.seed(42)
  qmat <- matrix(rnorm(9), 3, 3)
  theta_true <- c(0.5, -1, 2)
  comp <- make_point_components(qmat, drop(qmat %*% theta_true))
  pt <- compute_tau0_point(comp)
  expect_equal(pt$theta, theta_true, tolerance = 1e-10)
  expect_equal(pt$cond, kappa(qmat), tolerance = 1e-10)
})

test_that("rank-deficient and under-determined systems give NULL", {
  set.seed(1)
  qmat <- matrix(rnorm(9), 3, 3)
  rank_def <- qmat
  rank_def[3, ] <- rank_def[1, ]
  expect_null(compute_tau0_point(make_point_components(rank_def, rnorm(3))))
  expect_null(compute_tau0_point(make_point_components(qmat[1:2, ], rnorm(2))))
})

test_that("non-finite systems are misuse, not a no-point outcome", {
  # verified: the container validator does not check finiteness, and without
  # the solver's own guard an Inf in L slips the residual gate as a zero point
  set.seed(1)
  qmat <- matrix(rnorm(9), 3, 3)
  expect_error(compute_tau0_point(make_point_components(qmat, c(Inf, 1, 1))),
    class = "hetid_error_bad_argument"
  )
  q_inf <- qmat
  q_inf[2, 2] <- Inf
  expect_error(compute_tau0_point(make_point_components(q_inf, rnorm(3))),
    class = "hetid_error_bad_argument"
  )
})

# The maturities axis satisfies M <= n_components in every valid container
# (maturities index w2 columns), so a full-rank stacked system is square and
# necessarily consistent: the paper's residual-consistency gate is retained
# as ported defensive depth but is unreachable through validated containers
# and carries no dedicated test here.

test_that("non-components input raises hetid_error_bad_argument", {
  expect_error(compute_tau0_point(list(Q_i = list(), L_i = numeric())),
    class = "hetid_error_bad_argument"
  )
})
