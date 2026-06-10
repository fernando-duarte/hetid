# All tests use real package data: bundled ACM term-structure data through
# extract_acm_data() and the bundled quarterly `variables` dataset.

test_that("quarterly conversion keeps the last monthly observation of each quarter", {
  monthly <- extract_acm_data(
    data_types = "yields", maturities = 1,
    start_date = "2019-01-01", end_date = "2020-12-31"
  )
  quarterly <- extract_acm_data(
    data_types = "yields", maturities = 1,
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
      data_types = "yields", maturities = 1,
      start_date = "2010-01-01", end_date = "2019-12-31",
      frequency = "quarterly"
    )
  )
})

test_that("incomplete quarter warns and is re-dated to the quarter-end month", {
  # end_date in May leaves Q2 2020 with only its April and May observations
  expect_warning(
    quarterly <- extract_acm_data(
      data_types = "yields", maturities = 1,
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
    data_types = "yields", maturities = 1,
    start_date = "2020-05-01", end_date = "2020-05-31"
  )
  expect_equal(
    quarterly$y1[quarterly$date == as.Date("2020-06-30")],
    may_2020$y1[nrow(may_2020)]
  )
})

test_that("warning names the months when several are missing", {
  # end_date in April leaves Q2 2020 with only its April observation
  expect_warning(
    extract_acm_data(
      data_types = "yields", maturities = 1,
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
      data_types = "yields", maturities = 1,
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
        data_types = "yields", maturities = 1,
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

test_that("quarter-start dated real data is re-dated and keeps its quarter column", {
  data("variables", package = "hetid", envir = environment())
  # Real quarterly macro data dated at quarter starts, with a genuine
  # quarter column that must survive the conversion untouched
  quarterly_vars <- variables[1:4, c("date", "quarter", "gdpc1")]

  expect_warning(
    result <- convert_to_quarterly(quarterly_vars),
    class = "hetid_warning_incomplete_quarter"
  )

  expect_equal(
    result$date,
    as.Date(c("1962-03-31", "1962-06-30", "1962-09-30", "1962-12-31"))
  )
  expect_equal(result$quarter, quarterly_vars$quarter)
  expect_equal(result$gdpc1, quarterly_vars$gdpc1)
})

test_that("dropping several incomplete quarters uses plural wording", {
  data("variables", package = "hetid", envir = environment())
  quarterly_vars <- variables[1:4, c("date", "quarter", "gdpc1")]

  expect_message(
    result <- convert_to_quarterly(
      quarterly_vars,
      use_incomplete_quarters = FALSE
    ),
    regexp = "These quarters were dropped.*To keep them instead"
  )
  expect_equal(nrow(result), 0)
})
