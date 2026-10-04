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

test_that("the tau grid preserves exact donor keys and the backbone maximum", {
  got <- mean_tau_grid(0.615)
  keys <- c(
    "0.025624999999999998", "0.051249999999999997", "0.076874999999999999",
    "0.10249999999999999", "0.12812499999999999", "0.15375", "0.17937499999999998",
    "0.20499999999999999", "0.230625", "0.25624999999999998", "0.28187499999999999",
    "0.3075", "0.333125", "0.35874999999999996", "0.38437499999999997",
    "0.40999999999999998", "0.43562499999999998", "0.46124999999999999",
    "0.48687499999999995", "0.51249999999999996", "0.53812499999999996",
    "0.54453124999999991", "0.55093749999999997", "0.55734375000000003",
    "0.56374999999999997", "0.57015624999999992", "0.57656249999999998",
    "0.58296875000000004", "0.58937499999999998"
  )
  expect_identical(sprintf("%.17g", got), keys)
  expect_identical(max(got), seq(0, 0.615, length.out = 25L)[24L])
  expect_true(all(diff(got) > 0 & got[-1L] < 0.615))
  invalid <- MEAN_TAU_CONTROL
  invalid$GRID_TAIL_FRACTION <- 0.999
  expect_error(mean_tau_grid(0.615, invalid), class = "hetid_error_bad_argument")
  expect_error(mean_tau_grid(NA_real_), class = "hetid_error")
  expect_error(mean_tau_grid(0), class = "hetid_error")
})
