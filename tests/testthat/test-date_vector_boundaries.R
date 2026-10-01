test_that("dated public results reject nonfinite and dimensioned Date inputs", {
  y <- data.frame(y24 = 4:9, y36 = 6:11)
  tp <- data.frame(tp24 = rep(0.5, 6), tp36 = rep(0.8, 6))
  dates <- as.Date(paste0(2000:2005, "-12-31"))
  matrix_dates <- structure(as.numeric(dates), dim = c(6L, 1L), class = "Date")
  for (bad in list(
    structure(c(Inf, as.numeric(dates[-1])), class = "Date"),
    structure(c(-Inf, as.numeric(dates[-1])), class = "Date"),
    structure(c(NaN, as.numeric(dates[-1])), class = "Date"), matrix_dates
  )) {
    err <- tryCatch(compute_n_hat(y, tp, 24, bad), error = identity)
    expect_s3_class(err, "hetid_error_bad_argument")
    expect_identical(err$arg, "dates")
    err <- tryCatch(validate_dates_vector(bad, 6, "index"), error = identity)
    expect_s3_class(err, "hetid_error_bad_argument")
    if (inherits(err, "error")) expect_identical(err$arg, "index")
  }
  expect_error(compute_n_hat(y, tp, 24, dates[-1]),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("finite Date vectors preserve labels, ordering and empty outputs", {
  dates <- as.Date(c("2020-02-04", "2020-01-02", "2020-01-02"))
  expect_identical(
    withVisible(validate_dates_vector(dates, 3)),
    list(value = TRUE, visible = FALSE)
  )
  empty <- as.Date(character())
  expect_identical(
    withVisible(validate_dates_vector(empty, 0)),
    list(value = TRUE, visible = FALSE)
  )
  y <- data.frame(y24 = 4:6, y36 = 6:8)
  tp <- data.frame(tp24 = rep(0.5, 3), tp36 = rep(0.8, 3))
  fit <- compute_n_hat(y, tp, 24, dates)
  expect_identical(names(fit), c("date", "n_hat"))
  expect_identical(dim(fit), c(3L, 2L))
  expect_identical(fit$date, dates)
  expect_type(fit$n_hat, "double")
  expect_equal(fit$n_hat, (2 * y$y24 - 3 * y$y36 + 3 * tp$tp36 - 2 * tp$tp24) / 100)
  y <- matrix(numeric(), 0, 2, dimnames = list(NULL, c("y24", "y36")))
  tp <- matrix(numeric(), 0, 2, dimnames = list(NULL, c("tp24", "tp36")))
  fit <- compute_n_hat(y, tp, 24, empty)
  expect_identical(fit, data.frame(date = empty, n_hat = numeric()))
})
