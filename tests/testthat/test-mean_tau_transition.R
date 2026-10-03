test_that("the actual classifier brackets the donor synthetic transition", {
  fit <- mean_profile_fixture(26L)
  control <- MEAN_TAU_CONTROL
  control$cap <- 0.9
  control$sweep_step <- 0.1
  control$bisection_iterations <- 4L
  got <- find_mean_tau_star(fit, control)
  expect_identical(got$bracket$status, "bracketed")
  expect_equal(got$bracket$lower, 0.55625, tolerance = 1e-15)
  expect_identical(got$bracket$upper, 0.5625)
  expect_identical(got$tau_star, got$bracket$lower)
  expect_identical(
    got$sweep$status,
    c(
      rep("bounded", 6), rep("unbounded", 4), "bounded", "unbounded",
      "unbounded", "bounded"
    )
  )
  dimension <- ncol(fit$w2)
  classify <- function(tau) {
    quadratic <- build_quadratic_system(
      fit$gamma, rep(tau, dimension),
      fit$moments
    )$quadratic
    evidence <- compute_quadratic_set_evidence(quadratic, diag(dimension),
      points = matrix(fit$point$theta, nrow = 1L)
    )
    c(evidence$summary$lower_state, evidence$summary$upper_state)
  }
  expect_true(all(classify(got$bracket$lower) == "bounded"))
  expect_true(any(classify(got$bracket$upper) == "unbounded"))
})

test_that("a nondivisible cap retains the last inspected donor progression value", {
  fit <- mean_profile_fixture()
  control <- MEAN_TAU_CONTROL
  control$cap <- 0.41
  control$sweep_step <- 0.1
  testthat::local_mocked_bindings(profile_mean_tau_status = function(fit, tau) {
    "bounded"
  }, .package = "hetid")
  got <- find_mean_tau_star(fit, control)
  expect_identical(got$sweep$tau, seq(0, 0.41, by = 0.1))
  expect_identical(got$bracket$status, "capped")
  expect_identical(got$tau_star, 0.4)
  expect_identical(got$bracket$sweep_max, 0.4)
})
