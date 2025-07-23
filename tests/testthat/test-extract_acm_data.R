# Test file for extract_acm_data function
# Tests ACM data extraction and filtering

test_that("extract_acm_data returns expected structure", {
  # Function returns a data frame with date column and selected variables
  data <- extract_acm_data(auto_download = TRUE)

  # Check it's a data frame
  expect_s3_class(data, "data.frame")

  # Check for date column
  expect_true("date" %in% names(data))
  expect_s3_class(data$date, "Date")

  # Check for default columns (yields and term premia)
  expect_true(all(paste0("y", 1:10) %in% names(data)))
  expect_true(all(paste0("tp", 1:10) %in% names(data)))

  # Check data has rows
  expect_gt(nrow(data), 0)
})

test_that("extract_acm_data maturity selection", {
  # Test selecting specific maturities
  data <- extract_acm_data(
    maturities = c(2, 5, 10),
    auto_download = TRUE
  )

  # Should have only selected maturities
  expect_true(all(c("y2", "y5", "y10") %in% names(data)))
  expect_true(all(c("tp2", "tp5", "tp10") %in% names(data)))

  # Should not have other maturities
  expect_false("y1" %in% names(data))
  expect_false("y3" %in% names(data))

  # Single maturity
  data_single <- extract_acm_data(
    data_types = "yields",
    maturities = 5,
    auto_download = TRUE
  )
  expect_true("y5" %in% names(data_single))
  expect_equal(sum(grepl("^y\\d+$", names(data_single))), 1)
})

test_that("extract_acm_data date filtering", {
  # Test date range filtering
  start_date <- "2010-01-01"
  end_date <- "2015-12-31"

  data <- extract_acm_data(
    start_date = start_date,
    end_date = end_date,
    auto_download = TRUE
  )

  # Check dates are within range
  expect_true(all(data$date >= as.Date(start_date)))
  expect_true(all(data$date <= as.Date(end_date)))

  # Test with only start date
  data_start <- extract_acm_data(
    start_date = "2020-01-01",
    auto_download = TRUE
  )
  expect_true(all(data_start$date >= as.Date("2020-01-01")))

  # Test with only end date
  data_end <- extract_acm_data(
    end_date = "2010-12-31",
    auto_download = TRUE
  )
  expect_true(all(data_end$date <= as.Date("2010-12-31")))
})

test_that("extract_acm_data data types selection", {
  # Test selecting different data types

  # Only yields
  data_yields <- extract_acm_data(
    data_types = "yields",
    auto_download = TRUE
  )
  expect_true(all(paste0("y", 1:10) %in% names(data_yields)))
  expect_false(any(paste0("tp", 1:10) %in% names(data_yields)))
  expect_false(any(paste0("rn", 1:10) %in% names(data_yields)))

  # Only term premia
  data_tp <- extract_acm_data(
    data_types = "term_premia",
    auto_download = TRUE
  )
  expect_true(all(paste0("tp", 1:10) %in% names(data_tp)))
  expect_false(any(paste0("y", 1:10) %in% names(data_tp)))

  # All three types
  data_all <- extract_acm_data(
    data_types = c("yields", "term_premia", "risk_neutral"),
    auto_download = TRUE
  )
  expect_true(all(paste0("y", 1:10) %in% names(data_all)))
  expect_true(all(paste0("tp", 1:10) %in% names(data_all)))
  expect_true(all(paste0("rn", 1:10) %in% names(data_all)))
})

test_that("extract_acm_data frequency conversion", {
  # Test quarterly conversion
  data_monthly <- extract_acm_data(
    frequency = "monthly",
    auto_download = TRUE
  )

  data_quarterly <- extract_acm_data(
    frequency = "quarterly",
    auto_download = TRUE
  )

  # Quarterly should have fewer observations
  expect_lt(nrow(data_quarterly), nrow(data_monthly))

  # Quarterly dates should be end of quarter
  quarters <- quarters(data_quarterly$date)
  months <- format(data_quarterly$date, "%m")

  # Check that dates are in March, June, September, or December
  expect_true(all(months %in% c("03", "06", "09", "12")))
})

test_that("extract_acm_data handles edge cases", {
  # Empty result from impossible date range
  data_empty <- extract_acm_data(
    start_date = "2050-01-01",
    end_date = "2051-01-01",
    auto_download = TRUE
  )
  expect_equal(nrow(data_empty), 0)
  expect_true("date" %in% names(data_empty))

  # Invalid maturity
  expect_error(
    extract_acm_data(maturities = 11),
    "Maturities must be integers between 1 and 10"
  )

  # Invalid data type
  expect_error(
    extract_acm_data(data_types = "invalid"),
    "Invalid data_types"
  )
})

test_that("extract_acm_data data consistency", {
  # Test that term premium = yield - risk-neutral yield
  data <- extract_acm_data(
    data_types = c("yields", "term_premia", "risk_neutral"),
    maturities = c(2, 5, 10),
    auto_download = TRUE
  )

  # Check relationship for each maturity
  for (mat in c(2, 5, 10)) {
    y_col <- paste0("y", mat)
    tp_col <- paste0("tp", mat)
    rn_col <- paste0("rn", mat)

    # Term premium should equal yield minus risk-neutral yield
    # Allow small tolerance for rounding
    calculated_tp <- data[[y_col]] - data[[rn_col]]
    expect_equal(data[[tp_col]], calculated_tp, tolerance = 1e-6)
  }
})

test_that("extract_acm_data preserves data order", {
  # Data should be sorted by date
  data <- extract_acm_data(auto_download = TRUE)

  expect_true(all(diff(data$date) >= 0))
})

test_that("extract_acm_data error handling", {
  # Test with auto_download = FALSE and no data
  # First, temporarily move any existing data
  data_dir <- system.file("extdata", package = "hetid")
  acm_file <- file.path(data_dir, "ACMTermPremium.csv")
  temp_file <- NULL

  if (file.exists(acm_file)) {
    temp_file <- tempfile()
    file.rename(acm_file, temp_file)
  }

  # Should error when data not available and auto_download = FALSE
  expect_error(
    extract_acm_data(auto_download = FALSE),
    "ACM data not available"
  )

  # Restore file if it was moved
  if (!is.null(temp_file) && file.exists(temp_file)) {
    file.rename(temp_file, acm_file)
  }
})
