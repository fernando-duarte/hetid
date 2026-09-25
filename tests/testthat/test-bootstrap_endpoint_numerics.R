test_that("pointwise budgets retain a conservative bracket without false convergence", {
  lo <- c(rep(0, 84), rep(0.25, 8), rep(0, 8))
  up <- c(rep(0, 84), rep(0, 8), rep(1, 8))
  p <- bootstrap_pointwise_critical(lo, up, rep(TRUE, 100), 1, 1, 0.1, 1e-4, 0.25, 2L)
  expect_identical(p$search_stop, "max_evals")
  expect_equal(p$c_p_lower, 0)
  expect_gte(p$c_p_upper, 0.125)
  expect_lte(p$c_p_upper, 0.25)
  expect_equal(p$evals, 2)
  x <- bootstrap_fixture()
  out <- bootstrap_set_interval(x$full, x$draws, "containment", 0.1, 50, 0.85)
  expect_equal(out$summary$c_p_evals, 0)
  expect_true(is.na(out$summary$c_p_upper))
  expect_identical(out$summary$search_stop, "not_requested")
  expect_equal(out$summary$ci_lower, -2 - out$summary$c_s * out$summary$se_lower)
})

test_that("numerical overflow is not a geometric infinity", {
  x <- bootstrap_fixture()
  x$full$lower <- -1e308
  x$full$upper <- 1e308
  expect_error(bootstrap_fixture_fit(x), class = "hetid_error")
  expect_error(bootstrap_pointwise_critical(
    c(1, 2), c(2, 1), c(TRUE, TRUE),
    Inf, 1, 0.1, 1e-4, 2
  ), class = "hetid_error")
  values <- c(-1e308 - 1e293, -1e308, -1e308 + 1e293)
  side <- bootstrap_endpoint_side(values, rep("bounded", 3), 1e308, 1, 1, 0)
  expect_error(bootstrap_containment_critical(side$ok, 0.1, side$z), class = "hetid_error")
})

test_that("controls are strict and small pools expose their rank resolution", {
  x <- bootstrap_fixture(4)
  out <- bootstrap_set_interval(x$full, x$draws, "containment", 0.01, 2, 0.5)
  expect_equal(out$summary$root_rank, 4)
  expect_equal(out$summary$tail_resolution, 1 / 5)
  for (control in list(
    list(unknown = 1), list(tolerance = 0), list(max_evals = 1),
    list(max_evals = 2.5), list(tolerance = NA_real_)
  )) {
    expect_error(bootstrap_fixture_fit(x, control = control), class = "hetid_error")
  }
  expect_error(bootstrap_set_interval(x$full, x$draws, "p", 0.1, 2, 0.5),
    class = "hetid_error"
  )
  expect_error(bootstrap_set_interval(x$full, x$draws, "pointwise", 0, 2, 0.5),
    class = "hetid_error"
  )
})


test_that("diagnostic-only overflow does not abort other coefficient intervals", {
  full <- data.frame(
    coef = c("extreme", "regular"), lower = c(0, -2), upper = c(NA, -1),
    lower_status = c("bounded", "bounded"), upper_status = c("unreliable", "bounded")
  )
  v <- c(-2e-300, -1e-300, 0, 1e-300, 2e-300, 1e308)
  lo <- cbind(extreme = v, regular = seq(-2.2, -1.8, length.out = 6))
  up <- cbind(extreme = NA_real_, regular = lo[, 2] + 1)
  ls <- matrix("bounded", 6, 2, dimnames = dimnames(lo))
  us <- ls
  us[, 1] <- "unreliable"
  draws <- list(lower = lo, upper = up, lower_status = ls, upper_status = us)
  out <- bootstrap_set_interval(full, draws, "pointwise", 0.1, 3, 0.5)
  expect_identical(out$summary$reason, c("full-sample side unavailable or unreliable", "reported"))
  expect_true(is.finite(out$summary$ci_upper[2]))
  expect_true(is.na(out$simultaneous$critical))
  expect_identical(out$simultaneous$reason, "nonfinite_endpoint_deviations")
  expect_equal(out$simultaneous$n_common, 6)
  expect_true(all(out$simultaneous$active_sides[, "lower"]))
  full$upper[1] <- 0
  full$upper_status[1] <- "bounded"
  draws$upper[, 1] <- v
  draws$upper_status[, 1] <- "bounded"
  expect_error(bootstrap_set_interval(full, draws, "pointwise", 0.1, 3, 0.5),
    class = "hetid_error"
  )
})
