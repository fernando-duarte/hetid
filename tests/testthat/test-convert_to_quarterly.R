# Bundled ACM term-structure data through extract_acm_data(), plus small
# synthetic frames for the validation and re-dating paths

test_that("quarterly conversion keeps the last monthly observation of each quarter", {
  monthly <- extract_acm_data(
    data_types = "yields", maturities = 12,
    start_date = "2019-01-01", end_date = "2020-12-31"
  )
  quarterly <- extract_acm_data(
    data_types = "yields", maturities = 12,
    start_date = "2019-01-01", end_date = "2020-12-31",
    frequency = "quarterly"
  )

  expect_equal(nrow(quarterly), 8)
  expect_true(all(format(quarterly$date, "%m") %in% c("03", "06", "09", "12")))

  # Each quarterly value is the monthly value observed at that same date
  merged <- merge(quarterly, monthly, by = "date", suffixes = c("_q", "_m"))
  expect_equal(nrow(merged), 8)
  expect_equal(merged$y1_q, merged$y1_m)
})

test_that("complete quarters convert without any warning", {
  expect_no_warning(
    extract_acm_data(
      data_types = "yields", maturities = 12,
      start_date = "2010-01-01", end_date = "2019-12-31",
      frequency = "quarterly"
    )
  )
})

test_that("incomplete quarter warns and is re-dated to the quarter-end month", {
  # end_date in May leaves Q2 2020 with only its April and May observations
  expect_warning(
    quarterly <- extract_acm_data(
      data_types = "yields", maturities = 12,
      start_date = "2019-01-01", end_date = "2020-05-31",
      frequency = "quarterly"
    ),
    regexp = paste0(
      "2020 Q2 \\(last observation in May, quarter ends in June\\)",
      ".*USE_INCOMPLETE_QUARTERS"
    ),
    class = "hetid_warning_incomplete_quarter"
  )

  # Dates are uniform even though the last quarter is incomplete
  expect_true(all(format(quarterly$date, "%m") %in% c("03", "06", "09", "12")))
  expect_equal(max(quarterly$date), as.Date("2020-06-30"))

  # The re-dated row carries the latest available (May 2020) observation
  may_2020 <- extract_acm_data(
    data_types = "yields", maturities = 12,
    start_date = "2020-05-01", end_date = "2020-05-31"
  )
  expect_equal(
    quarterly$y12[quarterly$date == as.Date("2020-06-30")],
    may_2020$y12[nrow(may_2020)]
  )
})

test_that("warning names the months when several are missing", {
  # end_date in April leaves Q2 2020 with only its April observation
  expect_warning(
    extract_acm_data(
      data_types = "yields", maturities = 12,
      start_date = "2020-01-01", end_date = "2020-04-30",
      frequency = "quarterly"
    ),
    regexp = "2020 Q2 \\(last observation in April, quarter ends in June\\)",
    class = "hetid_warning_incomplete_quarter"
  )
})

test_that("incomplete first quarter of a year is re-dated across the year boundary", {
  expect_warning(
    quarterly <- extract_acm_data(
      data_types = "yields", maturities = 12,
      start_date = "2020-10-01", end_date = "2021-01-31",
      frequency = "quarterly"
    ),
    regexp = "2021 Q1 \\(last observation in January, quarter ends in March\\)",
    class = "hetid_warning_incomplete_quarter"
  )

  expect_equal(format(quarterly$date, "%m"), c("12", "03"))
  expect_equal(max(quarterly$date), as.Date("2021-03-31"))
})

test_that("use_incomplete_quarters = FALSE drops the incomplete quarter", {
  # Dropping is what the caller asked for, so it is announced with an
  # informational message naming the quarters, never a warning
  expect_no_warning(
    expect_message(
      quarterly <- extract_acm_data(
        data_types = "yields", maturities = 12,
        start_date = "2019-01-01", end_date = "2020-05-31",
        frequency = "quarterly", use_incomplete_quarters = FALSE
      ),
      regexp = paste0(
        "This quarter was dropped from the quarterly output. ",
        "To keep it instead, set use_incomplete_quarters = TRUE"
      )
    )
  )

  expect_equal(max(quarterly$date), as.Date("2020-03-31"))
  expect_true(all(format(quarterly$date, "%m") %in% c("03", "06", "09", "12")))
})

test_that("use_incomplete_quarters must be a logical flag", {
  expect_error(
    extract_acm_data(frequency = "quarterly", use_incomplete_quarters = NA),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    extract_acm_data(frequency = "quarterly", use_incomplete_quarters = "yes"),
    class = "hetid_error_bad_argument"
  )
})

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
