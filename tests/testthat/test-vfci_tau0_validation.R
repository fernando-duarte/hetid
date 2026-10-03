test_that("selectors and required numeric columns fail with structured conditions", {
  observations <- vfci_tau0_frame()
  for (bad in list(
    NULL, character(), NA_character_, "", " ", c("x1", "x1"), 1,
    matrix("x1", 1, 1)
  )) {
    expect_error(vfci_tau0_test_fit(observations, x = bad), class = "hetid_error_bad_argument")
  }
  expect_error(vfci_tau0_test_fit(observations, y = c("y", "x1")),
    class = "hetid_error_bad_argument"
  )
  expect_error(vfci_tau0_test_fit(observations, z = c("z", "x1")),
    class = "hetid_error_bad_argument"
  )
  expect_error(vfci_tau0_test_fit(observations, het = "absent"),
    class = "hetid_error_bad_argument"
  )
  expect_error(vfci_tau0_test_fit(as.matrix(observations)), class = "hetid_error_bad_argument")
  for (value in list(
    as.character(observations$h1), as.list(observations$h1),
    matrix(observations$h1)
  )) {
    changed <- observations
    changed$h1 <- value
    expect_error(vfci_tau0_test_fit(changed), class = "hetid_error_bad_argument")
  }
  duplicated <- observations
  names(duplicated)[2] <- "date"
  expect_error(vfci_tau0_test_fit(duplicated), class = "hetid_error_bad_argument")
  expect_error(vfci_tau0_test_fit(observations[, -1]), class = "hetid_error_bad_argument")
})

test_that("invalid date axes and bounds cannot relabel observations silently", {
  observations <- vfci_tau0_frame()
  for (value in list(
    as.character(observations$date), as.numeric(observations$date),
    as.POSIXct(observations$date), matrix(observations$date)
  )) {
    changed <- observations
    changed$date <- value
    expect_error(vfci_tau0_test_fit(changed), class = "hetid_error_bad_argument")
  }
  for (value in list(as.Date(NA), structure(Inf, class = "Date"))) {
    changed <- observations
    changed$date[1] <- value
    expect_error(vfci_tau0_test_fit(changed), class = "hetid_error_bad_argument")
    expect_error(vfci_tau0_test_fit(observations, date_begin = value),
      class = "hetid_error_bad_argument"
    )
  }
  for (value in list(NULL, "1950-01-01", 1)) {
    expect_error(vfci_tau0_test_fit(observations, date_begin = value),
      class = "hetid_error_bad_argument"
    )
  }
  expect_error(vfci_tau0_test_fit(observations, date_end = observations$date[1:2]),
    class = "hetid_error_dimension_mismatch"
  )
  expect_error(vfci_tau0_test_fit(observations, date_begin = as.Date("2025-01-01")),
    class = "hetid_error_bad_argument"
  )
  duplicate <- observations
  duplicate$date[2] <- as.Date("1950-02-01")
  expect_error(vfci_tau0_test_fit(duplicate), class = "hetid_error_bad_argument")
  expect_error(vfci_tau0_test_fit(observations[c(2, 1, 3:300), ]),
    class = "hetid_error_bad_argument"
  )
})

test_that("nonfinite selected values and undersized equation samples fail", {
  observations <- vfci_tau0_frame()
  for (col in c("y", "x1", "news1", "z", "h1")) {
    changed <- observations
    changed[[col]][20] <- Inf
    expect_error(vfci_tau0_test_fit(changed), class = "hetid_error_bad_argument")
  }
  expect_error(vfci_tau0_test_fit(observations,
    date_begin = as.Date("2025-01-01"),
    date_end = as.Date("2025-12-31")
  ), class = "hetid_error_insufficient_data")
  expect_error(vfci_tau0_test_fit(observations[1:3, ]), class = "hetid_error_insufficient_data")
  observations$h1 <- NA_real_
  expect_error(vfci_tau0_test_fit(observations), class = "hetid_error_insufficient_data")
  observations$h1[2:4] <- 1:3
  expect_error(vfci_tau0_test_fit(observations), class = "hetid_error_insufficient_data")
})

test_that("missing values omit only observations read by that equation", {
  observations <- vfci_tau0_frame()
  observations$h2[1] <- Inf
  observations$y[2] <- NaN
  observations$h2[2] <- Inf
  out <- vfci_tau0_test_fit(observations)
  expect_false(out$masks$mean[2])
  expect_false(out$masks$variance[1])
  expect_false(out$masks$variance[2])
  observations$y[3] <- Inf
  expect_silent(vfci_tau0_test_fit(observations, date_begin = as.Date("1951-01-01")))
})

test_that("identification and existing rank failures remain visible", {
  observations <- vfci_tau0_frame()
  observations$z <- 1
  expect_error(vfci_tau0_test_fit(observations), "no unique consistent solution",
    class = "hetid_error"
  )
  observations <- vfci_tau0_frame()
  observations$x1 <- 1
  expect_error(vfci_tau0_test_fit(observations), class = "hetid_error")
  observations <- vfci_tau0_frame()
  observations$h2 <- observations$h1
  expect_error(vfci_tau0_test_fit(observations), class = "hetid_error")
})

test_that("unsuccessful PPML returns cannot produce an index", {
  observations <- vfci_tau0_frame()
  for (bad in list(
    list(fit_status = "nonconvergence", converged = FALSE, coef = NULL),
    list(fit_status = "ok", converged = FALSE, coef = c(1, 2, 3)),
    list(fit_status = "ok", converged = TRUE, coef = c(1, Inf, 3))
  )) {
    local({
      local_mocked_bindings(fit_log_variance_at_b = function(...) bad, .package = "hetid")
      expect_error(vfci_tau0_test_fit(observations), "did not converge", class = "hetid_error")
    })
  }
})

test_that("PPML coefficient order and underlying structured errors are preserved", {
  observations <- vfci_tau0_frame()
  local_mocked_bindings(fit_log_variance_at_b = function(...) {
    list(
      fit_status = "ok", converged = TRUE,
      coef = c("(Intercept)" = 1, h2 = 2, h1 = 3)
    )
  }, .package = "hetid")
  expect_error(vfci_tau0_test_fit(observations), "coefficient labels",
    class = "hetid_error_bad_argument"
  )
  local_mocked_bindings(fit_log_variance_at_b = function(...) {
    stop_bad_argument("retained estimator error", arg = "x")
  }, .package = "hetid")
  condition <- tryCatch(vfci_tau0_test_fit(observations), hetid_error_bad_argument = identity)
  expect_identical(condition$arg, "x")
  expect_identical(conditionMessage(condition), "retained estimator error")
})
