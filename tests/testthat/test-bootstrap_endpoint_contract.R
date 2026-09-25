test_that("endpoint axes and scientific statuses are validated before gates", {
  x <- bootstrap_fixture()
  expect_identical(bootstrap_fixture_fit(x)$summary$reason, "reported")
  for (field in names(x$draws)) {
    bad <- x
    colnames(bad$draws[[field]]) <- "wrong"
    expect_error(bootstrap_fixture_fit(bad), class = "hetid_error")
    bad <- x
    rownames(bad$draws[[field]]) <- paste0("row", seq_len(100))
    expect_error(bootstrap_fixture_fit(bad), class = "hetid_error")
  }
  for (status in c(NA_character_, "unknown", "failed", "unbounded")) {
    bad <- x
    bad$draws$lower_status[1, 1] <- status
    expect_error(bootstrap_fixture_fit(bad), class = "hetid_error")
  }
  for (value in c(NA_real_, NaN, Inf)) {
    bad <- x
    bad$draws$lower[1, 1] <- value
    expect_error(bootstrap_fixture_fit(bad), class = "hetid_error")
  }
  bad <- x
  bad$draws$lower[1, 1] <- 10
  expect_error(bootstrap_fixture_fit(bad), class = "hetid_error")
  bad <- x
  bad$full$lower <- 10
  expect_error(bootstrap_fixture_fit(bad), class = "hetid_error")
  bad <- x
  bad$draws$lower[1, 1] <- Inf
  bad$draws$lower_status[1, 1] <- "unbounded"
  expect_error(bootstrap_fixture_fit(bad), class = "hetid_error")
  bad$draws$lower[1, 1] <- -Inf
  expect_identical(bootstrap_fixture_fit(bad)$summary$n_lower, 99L)
})

test_that("failed draws leave the denominator but other unavailable statuses remain", {
  for (status in c("failed", "unreliable", "unbounded")) {
    x <- bootstrap_fixture()
    x$draws$upper[1:20, ] <- NA_real_
    x$draws$upper_status[1:20, ] <- status
    out <- bootstrap_fixture_fit(x)
    expect_equal(out$summary$n_non_failed_upper, if (status == "failed") 80 else 100)
    expect_equal(out$summary$frac_upper, if (status == "failed") 1 else 0.8)
    expect_equal(out$summary$reason, if (status == "failed") {
      "reported"
    } else {
      "boundedness unstable across draws"
    })
    expect_identical(out$draws, x$draws)
  }
})

test_that("side scales and common pools retain distinct meanings", {
  x <- bootstrap_fixture()
  x$draws$lower[1:10, ] <- -3
  x$draws$upper[1:10, ] <- NA_real_
  x$draws$upper_status[1:10, ] <- "unbounded"
  out <- bootstrap_fixture_fit(x)
  expect_equal(out$summary$se_lower, stats::mad(x$draws$lower))
  expect_equal(out$summary$n_common, 90)
  expect_equal(out$summary$n_lower, 100)
  x <- bootstrap_fixture()
  x$draws$lower[1:40, ] <- NA_real_
  x$draws$upper[61:100, ] <- NA_real_
  x$draws$lower_status[1:40, ] <- "unbounded"
  x$draws$upper_status[61:100, ] <- "unbounded"
  out <- bootstrap_set_interval(x$full, x$draws, "containment", 0.1, 50, 0.5)
  expect_identical(out$summary$reason, "insufficient bounded draws")
  expect_equal(out$simultaneous$n_common, 20)
  expect_false(out$simultaneous$meets_min_reps)
  expect_true(is.finite(out$simultaneous$critical))
  expect_true(all(out$simultaneous$active_sides))
})

test_that("point mirrors cannot silently break pairing", {
  x <- bootstrap_fixture()
  x$draws$upper <- x$draws$lower
  x$draws$point <- x$draws$lower
  x$draws$point_status <- x$draws$lower_status
  expect_silent(bootstrap_fixture_fit(x))
  x$draws$point[1, 1] <- x$draws$point[1, 1] + 1
  expect_error(bootstrap_fixture_fit(x), class = "hetid_error")
  x$draws$point <- NULL
  expect_error(bootstrap_fixture_fit(x), class = "hetid_error")
})

test_that("empty draw pools and full-sample half-lines are explicit", {
  x <- bootstrap_fixture()
  empty <- x
  empty$draws <- lapply(empty$draws, function(m) m[FALSE, , drop = FALSE])
  out <- bootstrap_fixture_fit(empty)
  expect_identical(out$summary$reason, "insufficient bounded draws")
  expect_true(is.na(out$summary$ci_lower))
  expect_equal(out$summary$n_lower, 0)
  x$full$lower <- -Inf
  x$full$lower_status <- "unbounded"
  x$draws$lower[, ] <- NA_real_
  x$draws$lower_status[, ] <- "failed"
  out <- bootstrap_fixture_fit(x)$summary
  expect_identical(out$reason, "reported")
  expect_equal(out$ci_lower, -Inf)
  expect_equal(out$n_common, 100)
  expect_equal(out$c_p_upper, out$c_s)
  x$full$upper <- Inf
  x$full$upper_status <- "unbounded"
  expect_identical(
    bootstrap_fixture_fit(x)$summary$reason,
    "full-sample set unbounded on both sides"
  )
})


test_that("matrix-valued full-frame columns fail at the public boundary", {
  x <- bootstrap_fixture()
  for (field in names(x$full)) {
    bad <- x
    bad$full[[field]] <- I(matrix(bad$full[[field]], nrow = 1, ncol = 2))
    expect_error(bootstrap_fixture_fit(bad), class = "hetid_error")
  }
})
