test_that("to_period_end maps quarterly dates to calendar quarter-end", {
  d <- as.Date(c(
    "1962-01-01", # Q1 start  -> Mar 31
    "1961-09-29", # ACM business-day end -> Sep 30
    "1961-12-29", # ACM business-day end -> Dec 31
    "2020-05-15" # mid-Q2 -> Jun 30
  ))
  expect_identical(
    to_period_end(d, "quarterly"),
    as.Date(c("1962-03-31", "1961-09-30", "1961-12-31", "2020-06-30"))
  )
})

test_that("to_period_end maps monthly dates to calendar month-end (leap-aware)", {
  d <- as.Date(c("2020-02-10", "2021-02-10", "2019-01-31", "2019-12-01"))
  expect_identical(
    to_period_end(d, "monthly"),
    as.Date(c("2020-02-29", "2021-02-28", "2019-01-31", "2019-12-31"))
  )
})

test_that("to_period_end maps annual dates to Dec 31", {
  d <- as.Date(c("1999-01-01", "2000-06-30", "2000-12-31"))
  expect_identical(
    to_period_end(d, "annual"),
    as.Date(c("1999-12-31", "2000-12-31", "2000-12-31"))
  )
})

test_that("to_period_end is idempotent", {
  d <- as.Date(c("1962-01-01", "1961-09-29", "2020-05-15"))
  once <- to_period_end(d, "quarterly")
  expect_identical(to_period_end(once, "quarterly"), once)
})

test_that("to_period_end preserves length and accepts character input", {
  expect_length(to_period_end(c("2020-01-15", "2020-04-15"), "quarterly"), 2L)
  expect_identical(
    to_period_end("2020-03-30", "quarterly"),
    as.Date("2020-03-31")
  )
})

test_that("to_period_end rejects missing dates", {
  expect_error(
    to_period_end(as.Date(c("2020-01-01", NA)), "quarterly"),
    class = "hetid_error"
  )
})

test_that("to_period_end rejects an unknown frequency", {
  expect_error(to_period_end(as.Date("2020-01-01"), "weekly"))
})
