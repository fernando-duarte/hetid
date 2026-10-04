test_that("tau brackets return the bounded end and retain unresolved states", {
  fit <- mean_profile_fixture()
  control <- MEAN_TAU_CONTROL
  control$CAP <- 0.4
  control$SWEEP_STEP <- 0.1
  control$BISECTION_ITERATIONS <- 2L
  run <- function(classify) {
    testthat::local_mocked_bindings(profile_mean_tau_status = classify, .package = "hetid")
    find_mean_tau_star(fit, control)
  }
  bracket <- run(function(fit, tau) if (tau <= 0.28) "bounded" else "unbounded")
  expect_identical(bracket$bracket$status, "bracketed")
  expect_equal(bracket$tau_star, 0.275)
  expect_identical(bracket$tau_star, bracket$bracket$lower)
  capped <- run(function(fit, tau) "bounded")
  expect_identical(capped$bracket$status, "capped")
  expect_equal(capped$tau_star, 0.4)
  expect_true(is.na(capped$bracket$upper))
  above <- run(function(fit, tau) if (tau <= 0.2) "bounded" else "unreliable")
  expect_identical(above$bracket$status, "unresolved_above")
  below <- run(function(fit, tau) "unbounded")
  expect_identical(below$bracket$status, "unresolved_below")
  expect_identical(below$tau_star, 0)
  band <- run(function(fit, tau) {
    if (tau <= 0.2) "bounded" else if (tau >= 0.4) "unbounded" else "unreliable"
  })
  expect_identical(band$bracket$status, "unresolved_band")
  expect_equal(band$tau_star, 0.2)
  expect_true(length(band$bracket$inconclusive) >= 1L)
  expect_identical(tail(band$sweep$status, 1), "unreliable")
  expect_error(run(function(fit, tau) if (tau < 0.3) "unbounded" else "bounded"),
    "Inconsistent",
    class = "hetid_error"
  )
})
