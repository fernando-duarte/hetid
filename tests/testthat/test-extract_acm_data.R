# Tests ACM data extraction and filtering using the bundled CSV

test_that("extract_acm_data returns expected structure", {
  data <- extract_acm_data()

  expect_s3_class(data, "data.frame")

  expect_true("date" %in% names(data))
  expect_s3_class(data$date, "Date")

  expect_true(all(paste0("y", seq(12, 120, 12)) %in% names(data)))
  expect_true(all(paste0("tp", seq(12, 120, 12)) %in% names(data)))

  expect_gt(nrow(data), 0)
})

test_that("extract_acm_data maturity selection", {
  eval(parse("bodies/extract_acm_data-contracts.R", encoding = "UTF-8")[[1]], environment())
})

test_that("extract_acm_data date filtering", {
  eval(parse("bodies/extract_acm_data-contracts.R", encoding = "UTF-8")[[2]], environment())
})

test_that("extract_acm_data data types selection", {
  eval(parse("bodies/extract_acm_data-contracts.R", encoding = "UTF-8")[[3]], environment())
})

test_that("extract_acm_data frequency conversion", {
  data_monthly <- extract_acm_data(frequency = "monthly")

  data_quarterly <- extract_acm_data(
    frequency = "quarterly",
    use_incomplete_quarters = FALSE
  )

  expect_lt(nrow(data_quarterly), nrow(data_monthly))

  months <- format(data_quarterly$date, "%m")

  expect_true(all(months %in% c("03", "06", "09", "12")))
})

test_that("extract_acm_data handles edge cases", {
  eval(parse("bodies/extract_acm_data-contracts.R", encoding = "UTF-8")[[4]], environment())
})

test_that("extract_acm_data rejects empty or non-numeric maturities", {
  eval(parse("bodies/extract_acm_data-contracts.R", encoding = "UTF-8")[[5]], environment())
})

test_that("extract_acm_data rejects non-character non-Date date bounds", {
  eval(parse("bodies/extract_acm_data-contracts.R", encoding = "UTF-8")[[6]], environment())
})

test_that("extract_acm_data keeps single-variable results as data frames", {
  data <- extract_acm_data(data_types = "yields", maturities = 60)

  expect_s3_class(data, "data.frame")
  expect_named(data, c("date", "y60"))
})

test_that("extract_acm_data errors when a requested column is absent", {
  eval(parse("bodies/extract_acm_data-contracts.R", encoding = "UTF-8")[[7]], environment())
})

test_that("extract_acm_data errors when the date column cannot be parsed", {
  eval(parse("bodies/extract_acm_data-contracts.R", encoding = "UTF-8")[[8]], environment())
})

test_that("extract_acm_data data consistency", {
  eval(parse("bodies/extract_acm_data-contracts.R", encoding = "UTF-8")[[9]], environment())
})

test_that("extract_acm_data preserves data order", {
  data <- extract_acm_data()

  expect_true(all(diff(data$date) >= 0))
})

test_that("extract_acm_data warns on incomplete terminal quarter", {
  eval(parse("bodies/extract_acm_data-inputs.R", encoding = "UTF-8")[[1]], environment())
})

test_that("extract_acm_data error handling", {
  # Mock acm_data_available to simulate missing data
  local_mocked_bindings(
    acm_data_available = function(...) FALSE
  )

  expect_error(
    extract_acm_data(auto_download = FALSE),
    "Term premia data not found",
    class = "hetid_error_insufficient_data"
  )
})

test_that("normalize_acm_date_column converts character ACM dates", {
  acm_df <- data.frame(
    date = c("01-Jan-2020", "01-Feb-2020"),
    y1 = c(1.5, 1.6)
  )

  result <- normalize_acm_date_column(acm_df)
  expect_s3_class(result$date, "Date")
  expect_equal(result$date, as.Date(c("2020-01-01", "2020-02-01")))
})

test_that("normalize_acm_date_column passes through non-character cases", {
  already_date <- data.frame(
    date = as.Date(c("2020-01-01", "2020-02-01")),
    y1 = c(1.5, 1.6)
  )
  expect_identical(normalize_acm_date_column(already_date), already_date)

  no_date_col <- data.frame(y1 = c(1.5, 1.6))
  expect_identical(normalize_acm_date_column(no_date_col), no_date_col)
})

test_that("normalize_acm_date_column errors when nothing parses", {
  acm_df <- data.frame(
    date = c("junk-one", "junk-two"),
    y1 = c(1.5, 1.6)
  )

  expect_error(
    normalize_acm_date_column(acm_df),
    class = "hetid_error"
  )
})

test_that("normalize_acm_date_column warns on partial parse failures", {
  acm_df <- data.frame(
    date = c("01-Jan-2020", "junk"),
    y1 = c(1.5, 1.6)
  )

  expect_warning(
    result <- normalize_acm_date_column(acm_df),
    "could not be parsed"
  )
  expect_equal(result$date[1], as.Date("2020-01-01"))
  expect_true(is.na(result$date[2]))
})

test_that("ACM dates parse independently of LC_TIME locale", {
  eval(parse("bodies/extract_acm_data-inputs.R", encoding = "UTF-8")[[2]], environment())
})

test_that("filter_acm_date_range drops NA dates instead of fabricating rows", {
  eval(parse("bodies/extract_acm_data-inputs.R", encoding = "UTF-8")[[3]], environment())
})

test_that("a length > 1 date bound is rejected", {
  eval(parse("bodies/extract_acm_data-inputs.R", encoding = "UTF-8")[[4]], environment())
})

test_that("a canonical period-end start_date keeps the period it names", {
  # the trailing incomplete quarter warns; that path has its own test above
  quarterly <- suppressWarnings(extract_acm_data(
    data_types = "yields", maturities = 12,
    frequency = "quarterly", start_date = "1962-03-31"
  ))
  expect_equal(quarterly$date[1], as.Date("1962-03-31"))

  monthly <- extract_acm_data(
    data_types = "yields", maturities = 12,
    start_date = "1962-01-31"
  )
  expect_equal(monthly$date[1], as.Date("1962-01-31"))
})

test_that("filter_acm_date_range normalizes only the start bound", {
  eval(parse("bodies/extract_acm_data-inputs.R", encoding = "UTF-8")[[5]], environment())
})

test_that("sub-annual maturities extract from the bundled monthly grid", {
  data <- extract_acm_data(
    data_types = "yields",
    maturities = c(3, 18, 119)
  )

  expect_named(data, c("date", "y3", "y18", "y119"))
  expect_true(all(vapply(data[, -1], is.numeric, logical(1))))
  expect_gt(nrow(data), 0)
})

test_that("annual-only sources reject sub-annual maturity requests", {
  eval(parse("bodies/extract_acm_data-inputs.R", encoding = "UTF-8")[[6]], environment())
})
