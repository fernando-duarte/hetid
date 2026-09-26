evidence_system <- function(a, b = rep(0, nrow(a)), constant = -1) {
  list(A_i = list(a), b_i = list(b), c_i = constant)
}

test_that("boundedness evidence is checked independently of candidate optimizers", {
  ball <- evidence_system(diag(2))
  out <- compute_quadratic_set_evidence(ball, diag(2), n_dir = 0)
  expect_true(out$nonempty)
  expect_equal(out$summary$lower_state, rep("bounded", 2))
  expect_equal(out$summary$upper_state, rep("bounded", 2))
  expect_true(out$check_point(c(0, 0)))
  expect_false(out$check_point(c(2, 0)))
  expect_false(out$check_point(c(NA_real_, 0)))
  certificate <- out$boundedness
  expect_true(all(certificate$weights >= 0))
  combined <- Reduce(`+`, Map(
    function(a, weight, scale) a * weight / scale,
    ball$A_i, certificate$weights, certificate$scales
  ))
  expect_true(min(eigen(combined, symmetric = TRUE)$values) > certificate$error)
})

test_that("strict curvature settles orthogonal and tiny nonzero objectives", {
  outside <- evidence_system(-diag(2), constant = 1)
  objectives <- cbind(c(1, 0), c(0, 1e-200), c(0, 0))
  out <- compute_quadratic_set_evidence(outside, objectives,
    directions = matrix(c(1, 0), 1), n_dir = 0, maxit = 0
  )
  expect_true(out$nonempty)
  expect_equal(out$summary$lower_state, c("unbounded", "unbounded", "bounded"))
  expect_equal(out$summary$upper_state, out$summary$lower_state)
  expect_equal(out$summary$constant, c(FALSE, FALSE, TRUE))
})

test_that("line tails and directional finite certificates preserve side identity", {
  halfspace <- evidence_system(matrix(0, 2, 2), c(1, 0))
  out <- compute_quadratic_set_evidence(halfspace, diag(2), n_dir = 0)
  expect_equal(out$summary$lower_state, c("unbounded", "unbounded"))
  expect_equal(out$summary$upper_state, c("bounded", "unbounded"))
  cylinder <- evidence_system(diag(c(1, 0)))
  out <- compute_quadratic_set_evidence(cylinder, diag(2), n_dir = 0)
  expect_equal(out$summary$lower_state, c("bounded", "unbounded"))
  expect_equal(out$summary$upper_state, out$summary$lower_state)
})

test_that("zero objectives need a nonemptiness witness", {
  empty <- evidence_system(diag(2), constant = 1)
  out <- compute_quadratic_set_evidence(empty, matrix(0, 2, 1), n_dir = 0)
  expect_false(out$nonempty)
  expect_equal(out$summary$lower_state, "unresolved")
  expect_equal(out$summary$upper_state, "unresolved")
  allspace <- evidence_system(matrix(0, 2, 2), constant = 0)
  out <- compute_quadratic_set_evidence(allspace, cbind(diag(2), zero = 0), n_dir = 0)
  expect_equal(out$summary$lower_state, c("unbounded", "unbounded", "bounded"))
  expect_true(out$nonempty)
})

test_that("scale and near-flat curvature cannot create false infinity", {
  for (scale in c(1e-200, 1, 1e200)) {
    ball <- evidence_system(scale * diag(2), constant = -scale)
    out <- compute_quadratic_set_evidence(ball, diag(2), n_dir = 0)
    expect_equal(out$summary$lower_state, rep("bounded", 2))
    expect_equal(out$summary$upper_state, rep("bounded", 2))
  }
  thin <- evidence_system(diag(c(1, 1e-200)))
  out <- compute_quadratic_set_evidence(thin, diag(2), n_dir = 0)
  expect_false(any(out$summary$lower_state == "unbounded"))
  expect_false(any(out$summary$upper_state == "unbounded"))
})

test_that("search exhaustion is inconclusive and does not mutate RNG state", {
  rotated <- matrix(c(1, 1, 1, 1), 2)
  slab <- evidence_system(rotated)
  set.seed(9401)
  before <- .Random.seed
  out <- compute_quadratic_set_evidence(slab, diag(2), n_dir = 0, maxit = 0)
  expect_identical(.Random.seed, before)
  expect_false(any(out$summary$lower_state == "bounded"))
  expect_false(any(out$summary$upper_state == "bounded"))
})

test_that("raw quadratic validation uses structured errors", {
  ball <- evidence_system(diag(2))
  expect_error(compute_quadratic_set_evidence(ball, c(1, 0)), class = "hetid_error")
  bad <- ball
  bad$A_i[[1]][1, 2] <- 1
  expect_error(compute_quadratic_set_evidence(bad, diag(2)), class = "hetid_error")
  bad <- ball
  bad$b_i <- matrix(c(0, 0), 2, 1)
  expect_error(compute_quadratic_set_evidence(bad, diag(2)), class = "hetid_error")
  expect_error(compute_quadratic_set_evidence(ball, diag(2) + 1i), class = "hetid_error")
  bad <- ball
  bad$c_i <- Inf
  expect_error(compute_quadratic_set_evidence(bad, diag(2)), class = "hetid_error")
  expect_error(compute_quadratic_set_evidence(ball, diag(2), points = c(0, 0)),
    class = "hetid_error"
  )
})


test_that("a verified combination can bound individually indefinite constraints", {
  qs <- list(
    A_i = list(diag(c(1, -3)), diag(c(-.2, 1))),
    b_i = list(c(0, 0), c(0, 0)), c_i = c(-1, -1)
  )
  out <- compute_quadratic_set_evidence(qs, diag(2), n_dir = 0)
  expect_equal(out$summary$lower_state, rep("bounded", 2))
  expect_equal(out$summary$upper_state, rep("bounded", 2))
  weights <- out$boundedness$weights / out$boundedness$scales
  combined <- Reduce(`+`, Map(`*`, qs$A_i, weights))
  expect_true(all(diag(combined) > 0))
  # Independent containment: the weighted inequality bounds every feasible x.
  corners <- rbind(c(0, 0), c(.3, .3), c(-.3, .3))
  for (i in seq_len(nrow(corners))) {
    expect_true(out$check_point(corners[i, ]))
    expect_lte(sum(corners[i, ]^2 * diag(combined)), sum(weights))
  }
})
