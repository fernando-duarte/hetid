test_that("share quadratics equal independently computed centered fitted variances", {
  prepared <- variance_share_fixture()
  fit <- variance_share_fit(
    prepared$y, prepared$x, prepared$y2, prepared$z,
    VARIANCE_SHARE_CONTROL
  )
  covariance <- variance_share_covariances(
    prepared$y, prepared$x, prepared$y2,
    VARIANCE_SHARE_CONTROL
  )
  direct_var <- function(x) mean((x - mean(x))^2)
  expect_equal(covariance$s_e, stats::cov(prepared$x) * 219 / 220, tolerance = 1e-12)
  expect_equal(covariance$s_n, stats::cov(prepared$y2) * 219 / 220, tolerance = 1e-12)
  expect_equal(covariance$s_en, stats::cov(prepared$x, prepared$y2) * 219 / 220,
    tolerance = 1e-12
  )
  expect_identical(covariance$s_en, crossprod(
    sweep(prepared$x, 2, colMeans(prepared$x)),
    sweep(prepared$y2, 2, colMeans(prepared$y2))
  ) / 220)
  objectives <- variance_share_objectives(fit, colnames(prepared$x), covariance)
  theta <- c(.3, -.2, .1)
  b_e <- drop(fit$beta1r[-1] - t(fit$beta2r[, -1]) %*% theta)
  expected <- drop(prepared$x %*% b_e)
  news <- drop(prepared$y2 %*% theta)
  want <- 100 * vapply(list(expected, news, expected + news), direct_var, numeric(1)) /
    direct_var(prepared$y)
  got <- vapply(objectives, function(share) share$value(matrix(theta, 1)), numeric(1))
  expect_equal(unname(got), want, tolerance = 1e-12)
  fixed <- variance_share_fixed(b_e, theta, covariance)
  expect_equal(fixed[c(1, 5, 9)], want, tolerance = 1e-12)
  expect_equal(sum(fixed[2:4]), fixed[1], tolerance = 1e-12)
  expect_equal(sum(fixed[6:8]), fixed[5], tolerance = 1e-12)
  expect_gt(abs(fixed[9] - fixed[1] - fixed[5]), .01)
  for (share in objectives) {
    gradient <- vapply(seq_along(theta), function(j) {
      shift <- numeric(length(theta))
      shift[j] <- 1e-6
      (share$value(matrix(theta + shift, 1)) -
        share$value(matrix(theta - shift, 1))) / 2e-6
    }, numeric(1))
    expect_identical(names(share$grad(theta)), colnames(prepared$y2))
    expect_lte(max(abs(share$grad(theta) - gradient)), 1e-8)
  }
})

test_that("component squares retain zero crossings, missing values and one-sided infinity", {
  tab <- data.frame(
    set_lower = c(-.2, .1, -.3, -Inf, 1, NA),
    set_upper = c(.1, .3, -.1, Inf, Inf, NA),
    status = c(rep("bounded", 3), "unbounded", "unbounded", "unreliable")
  )
  got <- variance_share_component_range(tab, diag(c(1, 2, 3, 4, 2, 1)), 4)
  expect_equal(got$lo, c(0, .5, .75, 0, 50, NA), tolerance = 1e-12)
  expect_equal(got$hi, c(1, 4.5, 6.75, Inf, Inf, NA), tolerance = 1e-12)
  expect_identical(got$status, tab$status)
  cc <- list(lo = c(1, .5, .5), hi = c(2, 1, 1))
  expect_identical(variance_share_assert_coherent(
    cc, list(c(1, 2)),
    VARIANCE_SHARE_CONTROL
  ), cc)
  expect_error(variance_share_assert_coherent(
    within(cc, hi[1] <- .5),
    list(c(1, 2)), VARIANCE_SHARE_CONTROL
  ), "max below", class = "hetid_error")
  expect_error(variance_share_assert_coherent(
    within(cc, lo[1] <- .1),
    list(c(1, 2)), VARIANCE_SHARE_CONTROL
  ), "min below", class = "hetid_error")
  unfinished <- within(cc, hi[1] <- NA_real_)
  expect_identical(variance_share_assert_coherent(
    unfinished, list(c(1, 2)),
    VARIANCE_SHARE_CONTROL
  ), unfinished)
})
