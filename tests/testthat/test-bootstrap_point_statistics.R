test_that("point tails have hand-calculated finite-draw arithmetic", {
  draws <- matrix(-1:4, 6, 1, dimnames = list(NULL, "a"))
  status <- matrix("bounded", 6, 1, dimnames = dimnames(draws))
  out <- bootstrap_point_statistics(c(a = 2), draws, status, 3, 0.8)
  expect_equal(out$p_value, 4 / 7)
  expect_equal(out$p_lower, 3 / 7)
  expect_equal(out$p_upper, 2 / 7)
  expect_equal(out$se, 1.5 * 1.4826)
  expect_equal(out$statistic, 2 / (1.5 * 1.4826))
  expect_equal(out$p_value_normal, 2 * pnorm(-abs(out$statistic)))
  zero <- bootstrap_point_statistics(c(a = 0), draws, status, 3, 0.8)
  expect_equal(zero$p_value, 1)
  expect_equal(zero$p_lower, 3 / 7)
  expect_equal(zero$p_upper, 6 / 7)
  shifted <- draws + 100
  floor <- bootstrap_point_statistics(c(a = 102), shifted, status, 3, 0.8)
  expect_equal(floor$p_value, 1 / 7)
  expect_equal(floor$p_lower, 1 / 7)
  expect_equal(floor$p_upper, 1 / 7)
})

test_that("point gates preserve status counts and reject unsupported values", {
  x <- bootstrap_fixture()
  draws <- x$draws$lower
  status <- x$draws$lower_status
  draws[1:20, ] <- NA_real_
  status[1:20, ] <- "failed"
  out <- bootstrap_point_statistics(c(a = -2), draws, status, 50, 0.85)
  expect_identical(out$reason, "reported")
  expect_equal(out$n_failed, 20)
  expect_equal(out$n_non_failed, 80)
  status[1:20, ] <- "unreliable"
  expect_identical(
    bootstrap_point_statistics(c(a = -2), draws, status, 50, 0.85)$reason,
    "boundedness unstable across draws"
  )
  expect_identical(
    bootstrap_point_statistics(c(a = NA_real_), draws, status, 50, 0.85)$reason,
    "full-sample point not available"
  )
  status[1:20, ] <- "unbounded"
  expect_error(bootstrap_point_statistics(c(a = -2), draws, status, 50, 0.85),
    class = "hetid_error"
  )
  expect_error(bootstrap_point_statistics(c(wrong = -2), draws, status, 50, 0.85),
    class = "hetid_error"
  )
})


test_that("point inference never computes unused standardized outliers", {
  v <- c(-2e-300, -1e-300, 0, 1e-300, 2e-300, 1e308)
  draws <- matrix(v, 6, 1, dimnames = list(NULL, "a"))
  status <- matrix("bounded", 6, 1, dimnames = dimnames(draws))
  out <- bootstrap_point_statistics(c(a = 0), draws, status, 3, 0.8)
  expect_identical(out$reason, "reported")
  expect_equal(out$se, stats::mad(v))
  expect_identical(out$statistic, 0)
  expect_identical(out$p_value, 1)
  expect_equal(out$p_lower, 4 / 7)
  expect_equal(out$p_upper, 5 / 7)
})
