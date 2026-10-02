# convert_to_quarterly() on small synthetic frames: input validation, re-dating, NA dates

test_that("duplicated input dates are rejected up front", {
  dup <- data.frame(
    date = as.Date(c("2020-01-31", "2020-01-31", "2020-02-29")),
    y1 = c(1, 2, 3)
  )

  expect_error(
    convert_to_quarterly(dup),
    "duplicated dates",
    class = "hetid_error_bad_argument"
  )
})

test_that("incomplete quarters are re-dated to calendar quarter-end", {
  # Each obs is in the first month of its quarter, so every quarter is
  # incomplete and re-dated to quarter-end; a genuine quarter column survives
  incomplete_q <- data.frame(
    date = as.Date(c("1962-01-31", "1962-04-30", "1962-07-31", "1962-10-31")),
    quarter = c(1, 2, 3, 4),
    gdpc1 = c(10, 20, 30, 40)
  )

  expect_warning(
    result <- convert_to_quarterly(incomplete_q),
    class = "hetid_warning_incomplete_quarter"
  )

  expect_equal(
    result$date,
    as.Date(c("1962-03-31", "1962-06-30", "1962-09-30", "1962-12-31"))
  )
  expect_equal(result$quarter, incomplete_q$quarter)
  expect_equal(result$gdpc1, incomplete_q$gdpc1)
})

test_that("dropping several incomplete quarters uses plural wording", {
  incomplete_q <- data.frame(
    date = as.Date(c("1962-01-31", "1962-04-30", "1962-07-31", "1962-10-31")),
    quarter = c(1, 2, 3, 4),
    gdpc1 = c(10, 20, 30, 40)
  )

  expect_message(
    result <- convert_to_quarterly(
      incomplete_q,
      use_incomplete_quarters = FALSE
    ),
    regexp = "These quarters were dropped.*To keep them instead"
  )
  expect_equal(nrow(result), 0)
})

test_that("NA-dated rows are dropped with a classed warning, not silently", {
  mixed <- data.frame(
    date = as.Date(c("2020-01-31", NA, "2020-02-29", "2020-03-31")),
    y1 = c(1, 2, 3, 4)
  )

  expect_warning(
    result <- convert_to_quarterly(mixed),
    regexp = "Dropped 1 row with a missing",
    class = "hetid_warning_dropped_na_dates"
  )

  # The NA-dated row is gone; the real Jan/Feb/Mar rows collapse to the
  # single (complete) Q1 observation at the March month-end
  expect_false(anyNA(result$date))
  expect_equal(result$date, as.Date("2020-03-31"))
  expect_equal(result$y1, 4)
})

test_that("an all-NA-date input returns an empty frame with a warning", {
  all_na <- data.frame(
    date = as.Date(c(NA, NA)),
    y1 = c(1, 2)
  )

  expect_warning(
    result <- convert_to_quarterly(all_na),
    class = "hetid_warning_dropped_na_dates"
  )
  expect_equal(nrow(result), 0)
})

test_that("non-tabular input raises a structured error", {
  expect_error(
    convert_to_quarterly(1:5),
    class = "hetid_error_bad_argument"
  )
})

test_that("input without a date column raises a structured error", {
  expect_error(
    convert_to_quarterly(data.frame(x = 1:6)),
    "Missing required columns: date",
    class = "hetid_error_bad_argument"
  )
})
