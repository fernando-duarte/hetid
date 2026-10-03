test_that("quarterly mean and last are explicit and preserve date-keyed coverage", {
  monthly <- data.frame(
    date = as.Date(c("2024-03-15", "2024-01-01", "2024-02-29")),
    value = c(6, 1, 2), quarter = c(30, 10, 20)
  )
  mean_result <- aggregate_quarterly(monthly, "mean")
  last_result <- aggregate_quarterly(monthly, "last")
  expect_identical(mean_result$data$date, as.Date("2024-03-31"))
  expect_identical(mean_result$data$value, 3)
  expect_identical(last_result$data$value, 6)
  expect_identical(mean_result$data$quarter, 20)
  expect_identical(last_result$data$quarter, 30)
  expect_identical(mean_result$periods, data.frame(
    date = as.Date("2024-03-31"), n_observations = 3L, terminal_month_present = TRUE
  ))
  expect_identical(mean_result$periods, last_result$periods)
  expect_identical(mean_result$method, "mean")
  expect_false(mean_result$na_rm)
  expect_true(mean_result$use_incomplete_quarters)
})

test_that("all-NA and all-NaN means preserve base missingness under both flags", {
  monthly <- data.frame(
    date = as.Date(c("2024-01-31", "2024-02-29", "2024-03-31")),
    all_na = rep(NA_real_, 3), all_nan = rep(NaN, 3), mixed = c(1, NA_real_, 5)
  )
  kept <- aggregate_quarterly(monthly, "mean", na_rm = FALSE)$data
  removed <- aggregate_quarterly(monthly, "mean", na_rm = TRUE)$data
  expect_identical(kept$all_na, NA_real_)
  expect_identical(kept$all_nan, NaN)
  expect_identical(kept$mixed, NA_real_)
  expect_identical(removed$all_na, NaN)
  expect_identical(removed$all_nan, NaN)
  expect_identical(removed$mixed, 3)
  expect_identical(names(removed), names(monthly))
  monthly$mixed[3] <- NA_real_
  expect_identical(aggregate_quarterly(monthly, "last", na_rm = TRUE)$data$mixed, NA_real_)
})

test_that("partial-quarter policy uses terminal month and reports observed row counts", {
  monthly <- data.frame(
    date = as.Date(c("2024-01-31", "2024-03-31", "2024-04-30")), value = c(1, 6, 9)
  )
  expect_warning(kept <- aggregate_quarterly(monthly, "mean"),
    class = "hetid_warning_incomplete_quarter"
  )
  expect_identical(kept$data$value, c(3.5, 9))
  expect_identical(kept$data$date, as.Date(c("2024-03-31", "2024-06-30")))
  expect_identical(kept$periods$n_observations, c(2L, 1L))
  expect_identical(kept$periods$terminal_month_present, c(TRUE, FALSE))
  expect_message(dropped <- aggregate_quarterly(monthly, "last", FALSE), "was dropped")
  expect_identical(dropped$data$value, 6)
  expect_identical(dropped$periods$n_observations, 2L)
  expect_true(dropped$periods$terminal_month_present)
})

test_that("empty aggregation outputs retain typed data and period metadata", {
  empty <- data.frame(date = as.Date(character()), value = numeric())
  for (method in c("mean", "last")) {
    result <- aggregate_quarterly(empty, method)
    expect_identical(result$data, empty)
    expect_identical(result$periods, data.frame(
      date = as.Date(character()), n_observations = integer(), terminal_month_present = logical()
    ))
  }
  partial <- data.frame(date = as.Date("2024-01-31"), value = 1)
  expect_message(result <- aggregate_quarterly(partial, "mean", FALSE), "was dropped")
  expect_equal(nrow(result$data), 0L)
  expect_identical(result$periods$date, as.Date(character()))
})

test_that("monthly aggregation rejects ambiguous months and malformed inputs", {
  monthly <- data.frame(date = as.Date(c("2024-01-01", "2024-01-31")), value = 1:2)
  expect_error(aggregate_quarterly(monthly, "last"), class = "hetid_error_bad_argument")
  monthly <- data.frame(date = as.Date("2024-03-31"), value = 1)
  expect_error(aggregate_quarterly(monthly), class = "hetid_error_bad_argument")
  for (method in list(NA_character_, "other", 1, c("mean", "last"), matrix("mean"))) {
    expect_error(aggregate_quarterly(monthly, method), class = "hetid_error_bad_argument")
  }
  expect_error(aggregate_quarterly(monthly, "mean", na_rm = NA),
    class = "hetid_error_bad_argument"
  )
  expect_error(aggregate_quarterly(monthly, "mean", use_incomplete_quarters = 1),
    class = "hetid_error_bad_argument"
  )
  monthly$date <- as.Date(NA)
  expect_error(aggregate_quarterly(monthly, "mean"), class = "hetid_error_bad_argument")
  monthly$date <- as.Date("2024-03-31")
  monthly$value <- "invalid"
  expect_error(aggregate_quarterly(monthly, "mean"), class = "hetid_error_bad_argument")
  names(monthly) <- c("date", "date")
  expect_error(aggregate_quarterly(monthly, "mean"), class = "hetid_error_bad_argument")
})
