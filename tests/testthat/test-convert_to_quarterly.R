test_that("convert_to_quarterly picks last month of each quarter", {
  dates <- as.Date(c(
    "2020-01-15", "2020-02-15", "2020-03-15",
    "2020-04-15", "2020-05-15", "2020-06-15"
  ))
  data <- data.frame(date = dates, value = 1:6)

  result <- convert_to_quarterly(data)

  expect_equal(nrow(result), 2)
  expect_equal(result$date, as.Date(c("2020-03-15", "2020-06-15")))
  expect_equal(result$value, c(3, 6))
})

test_that("convert_to_quarterly warns on incomplete quarters", {
  # Q1 has only Jan and Feb (March missing)
  dates <- as.Date(c(
    "2020-01-15", "2020-02-15",
    "2020-04-15", "2020-05-15", "2020-06-15"
  ))
  data <- data.frame(date = dates, value = 1:5)

  expect_warning(
    result <- convert_to_quarterly(data),
    "Incomplete quarter.*2020 Q1.*month 2 instead of 3"
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$date, as.Date(c("2020-02-15", "2020-06-15")))
  expect_equal(result$value, c(2, 5))
})

test_that("convert_to_quarterly warns when two months missing", {
  # Q1 has only January (Feb and March missing)
  dates <- as.Date(c(
    "2020-01-15",
    "2020-04-15", "2020-05-15", "2020-06-15"
  ))
  data <- data.frame(date = dates, value = 1:4)

  expect_warning(
    result <- convert_to_quarterly(data),
    "Incomplete quarter.*2020 Q1.*month 1 instead of 3"
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$date, as.Date(c("2020-01-15", "2020-06-15")))
  expect_equal(result$value, c(1, 4))
})

test_that("convert_to_quarterly does not warn on complete quarters", {
  dates <- as.Date(c(
    "2020-03-15", "2020-06-15", "2020-09-15", "2020-12-15"
  ))
  data <- data.frame(date = dates, value = 1:4)

  expect_no_warning(result <- convert_to_quarterly(data))

  expect_equal(nrow(result), 4)
  expect_equal(result$value, 1:4)
})

test_that("convert_to_quarterly handles year boundaries correctly", {
  # Q4 2020 complete, Q1 2021 incomplete (only Jan)
  dates <- as.Date(c(
    "2020-10-15", "2020-11-15", "2020-12-15",
    "2021-01-15"
  ))
  data <- data.frame(date = dates, value = 1:4)

  expect_warning(
    result <- convert_to_quarterly(data),
    "Incomplete quarter.*2021 Q1.*month 1 instead of 3"
  )

  expect_equal(nrow(result), 2)
  expect_equal(
    result$date,
    as.Date(c("2020-12-15", "2021-01-15"))
  )
  expect_equal(result$value, c(3, 4))
})
