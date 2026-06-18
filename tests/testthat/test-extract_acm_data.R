# Test file for extract_acm_data function
# Tests ACM data extraction and filtering using the bundled CSV

test_that("extract_acm_data returns expected structure", {
  # Function returns a data frame with date column and selected variables
  data <- extract_acm_data()

  # Check it's a data frame
  expect_s3_class(data, "data.frame")

  # Check for date column
  expect_true("date" %in% names(data))
  expect_s3_class(data$date, "Date")

  # Check for default columns (yields and term premia)
  expect_true(all(paste0("y", seq(12, 120, 12)) %in% names(data)))
  expect_true(all(paste0("tp", seq(12, 120, 12)) %in% names(data)))

  # Check data has rows
  expect_gt(nrow(data), 0)
})

test_that("extract_acm_data maturity selection", {
  # Test selecting specific maturities
  data <- extract_acm_data(maturities = c(24, 60, 120))

  # Should have only selected maturities
  expect_true(all(c("y24", "y60", "y120") %in% names(data)))
  expect_true(all(c("tp24", "tp60", "tp120") %in% names(data)))

  # Should not have other maturities
  expect_false("y12" %in% names(data))
  expect_false("y36" %in% names(data))

  # Single maturity
  data_single <- extract_acm_data(
    data_types = "yields",
    maturities = 60
  )
  expect_true("y60" %in% names(data_single))
  expect_equal(sum(grepl("^y\\d+$", names(data_single))), 1)
})

test_that("extract_acm_data date filtering", {
  # Test date range filtering
  start_date <- "2010-01-01"
  end_date <- "2015-12-31"

  data <- extract_acm_data(
    start_date = start_date,
    end_date = end_date
  )

  # Check dates are within range
  expect_true(all(data$date >= as.Date(start_date)))
  expect_true(all(data$date <= as.Date(end_date)))

  # Test with only start date
  data_start <- extract_acm_data(start_date = "2020-01-01")
  expect_true(all(data_start$date >= as.Date("2020-01-01")))

  # Test with only end date
  data_end <- extract_acm_data(end_date = "2010-12-31")
  expect_true(all(data_end$date <= as.Date("2010-12-31")))
})

test_that("extract_acm_data data types selection", {
  # Test selecting different data types

  # Only yields
  data_yields <- extract_acm_data(data_types = "yields")
  expect_true(all(paste0("y", seq(12, 120, 12)) %in% names(data_yields)))
  expect_false(any(paste0("tp", seq(12, 120, 12)) %in% names(data_yields)))
  expect_false(any(paste0("rn", seq(12, 120, 12)) %in% names(data_yields)))

  # Only term premia
  data_tp <- extract_acm_data(data_types = "term_premia")
  expect_true(all(paste0("tp", seq(12, 120, 12)) %in% names(data_tp)))
  expect_false(any(paste0("y", seq(12, 120, 12)) %in% names(data_tp)))

  # All three types
  data_all <- extract_acm_data(
    data_types = c("yields", "term_premia", "risk_neutral_yields")
  )
  expect_true(all(paste0("y", seq(12, 120, 12)) %in% names(data_all)))
  expect_true(all(paste0("tp", seq(12, 120, 12)) %in% names(data_all)))
  expect_true(all(paste0("rny", seq(12, 120, 12)) %in% names(data_all)))
})

test_that("extract_acm_data frequency conversion", {
  # Test quarterly conversion
  data_monthly <- extract_acm_data(frequency = "monthly")

  data_quarterly <- extract_acm_data(
    frequency = "quarterly",
    use_incomplete_quarters = FALSE
  )

  # Quarterly should have fewer observations
  expect_lt(nrow(data_quarterly), nrow(data_monthly))

  # Quarterly dates should be end of quarter
  months <- format(data_quarterly$date, "%m")

  # Check that dates are in March, June, September, or December
  expect_true(all(months %in% c("03", "06", "09", "12")))
})

test_that("extract_acm_data handles edge cases", {
  # Empty result from impossible date range
  data_empty <- extract_acm_data(
    start_date = "2050-01-01",
    end_date = "2051-01-01"
  )
  expect_equal(nrow(data_empty), 0)
  expect_true("date" %in% names(data_empty))

  # Quarterly conversion of an empty range returns a zero-row frame
  data_empty_quarterly <- extract_acm_data(
    start_date = "2050-01-01",
    frequency = "quarterly"
  )
  expect_s3_class(data_empty_quarterly, "data.frame")
  expect_equal(nrow(data_empty_quarterly), 0)

  # Invalid maturities: below the 1-month floor and above the ceiling
  expect_error(
    extract_acm_data(maturities = 0),
    "must be between 1 and",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    extract_acm_data(maturities = 121),
    "must be between 1 and",
    class = "hetid_error_bad_argument"
  )

  # The 1- and 2-month maturities are now part of the grid
  one_two <- extract_acm_data(data_types = "yields", maturities = c(1, 2))
  expect_named(one_two, c("date", "y1", "y2"))

  # Invalid data type
  expect_error(
    extract_acm_data(data_types = "invalid"),
    "Invalid data_types"
  )
})

test_that("extract_acm_data rejects empty or non-numeric maturities", {
  expect_error(
    extract_acm_data(maturities = numeric(0)),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    extract_acm_data(maturities = "5"),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    extract_acm_data(maturities = 2.5),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    extract_acm_data(data_types = character(0)),
    class = "hetid_error_bad_argument"
  )
})

test_that("extract_acm_data rejects non-character non-Date date bounds", {
  expect_error(
    extract_acm_data(start_date = 2010),
    "start_date must be a Date or a character string",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    extract_acm_data(end_date = 2020),
    "end_date must be a Date or a character string",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    extract_acm_data(start_date = TRUE),
    class = "hetid_error_bad_argument"
  )
})

test_that("extract_acm_data keeps single-variable results as data frames", {
  data <- extract_acm_data(data_types = "yields", maturities = 60)

  expect_s3_class(data, "data.frame")
  expect_named(data, c("date", "y60"))
})

test_that("extract_acm_data errors when a requested column is absent", {
  withr::with_tempdir({
    temp_csv <- file.path(getwd(), "ACMTermPremium.csv")
    write.csv(
      data.frame(
        DATE = c("01-Jan-2020", "01-Feb-2020"),
        ACMY01 = c(1.5, 1.6)
      ),
      temp_csv,
      row.names = FALSE
    )

    local_mocked_bindings(
      acm_data_available = function(...) TRUE,
      get_acm_data_path = function(...) temp_csv
    )

    # The 5-year column (ACMY05) is absent: a missing required column is
    # an incomplete/corrupt source, signalled rather than silently dropped.
    expect_error(
      extract_acm_data(data_types = "yields", maturities = 60),
      "missing required column",
      class = "hetid_error_insufficient_data"
    )
  })
})

test_that("extract_acm_data errors when the date column cannot be parsed", {
  withr::with_tempdir({
    temp_csv <- file.path(getwd(), "ACMTermPremium.csv")
    write.csv(
      data.frame(
        DATE = c("junk-one", "junk-two"),
        ACMY01 = c(1.5, 1.6)
      ),
      temp_csv,
      row.names = FALSE
    )

    local_mocked_bindings(
      acm_data_available = function(...) TRUE,
      get_acm_data_path = function(...) temp_csv
    )

    expect_error(
      suppressWarnings(
        extract_acm_data(data_types = "yields", maturities = 12)
      ),
      class = "hetid_error"
    )
  })
})

test_that("extract_acm_data data consistency", {
  # Test that term premium = yield - risk-neutral yield
  data <- extract_acm_data(
    data_types = c("yields", "term_premia", "risk_neutral_yields"),
    maturities = c(24, 60, 120)
  )

  # Check relationship for each maturity
  for (mat in c(24, 60, 120)) {
    y_col <- paste0("y", mat)
    tp_col <- paste0("tp", mat)
    rny_col <- paste0("rny", mat)

    # Term premium should equal yield minus risk-neutral yield
    # Allow small tolerance for rounding
    calculated_tp <- data[[y_col]] - data[[rny_col]]
    expect_equal(data[[tp_col]], calculated_tp, tolerance = 1e-6)
  }
})

test_that("extract_acm_data preserves data order", {
  # Data should be sorted by date
  data <- extract_acm_data()

  expect_true(all(diff(data$date) >= 0))
})

test_that("extract_acm_data warns on incomplete terminal quarter", {
  # end_date in May truncates Q2, leaving only Apr and May
  expect_warning(
    data <- extract_acm_data(
      data_types = "yields",
      maturities = 60,
      end_date = "2020-05-15",
      frequency = "quarterly"
    ),
    regexp = "Incomplete quarter",
    class = "hetid_warning_incomplete_quarter"
  )

  # Result still contains data and stays uniformly dated at quarter ends
  expect_gt(nrow(data), 0)
  expect_true("y60" %in% names(data))
  expect_true(all(format(data$date, "%m") %in% c("03", "06", "09", "12")))
  expect_equal(max(data$date), as.Date("2020-06-30"))
})

test_that("extract_acm_data error handling", {
  # Mock acm_data_available to simulate missing data
  local_mocked_bindings(
    acm_data_available = function(...) FALSE
  )

  # Should error when data not available and auto_download = FALSE
  expect_error(
    extract_acm_data(auto_download = FALSE),
    "ACM data not available"
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
  old_locale <- Sys.getlocale("LC_TIME")
  withr::defer(Sys.setlocale("LC_TIME", old_locale))
  set_result <- suppressWarnings(Sys.setlocale("LC_TIME", "fr_FR.UTF-8"))
  skip_if(identical(set_result, ""), "fr_FR.UTF-8 locale not available")

  parsed <- parse_dates_c_locale(
    "30-Jun-1961",
    HETID_CONSTANTS$ACM_DATE_FORMAT
  )
  expect_equal(parsed, as.Date("1961-06-30"))

  # The caller's locale must be restored after parsing
  expect_identical(Sys.getlocale("LC_TIME"), "fr_FR.UTF-8")
})

test_that("filter_acm_date_range drops NA dates instead of fabricating rows", {
  acm_data <- data.frame(
    date = as.Date(c("2020-01-01", NA, "2021-01-01")),
    y1 = c(1, 2, 3)
  )

  filtered <- filter_acm_date_range(acm_data, as.Date("2020-06-01"), NULL)
  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$y1, 3)
  expect_false(anyNA(filtered$date))

  filtered_end <- filter_acm_date_range(acm_data, NULL, as.Date("2020-06-01"))
  expect_equal(nrow(filtered_end), 1)
  expect_equal(filtered_end$y1, 1)
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
  withr::with_tempdir({
    temp_csv <- file.path(getwd(), "annual_only.csv")
    write.csv(
      data.frame(
        DATE = c("2020-01-31", "2020-02-29"),
        ACMY01 = c(1.5, 1.6),
        ACMY02 = c(1.7, 1.8)
      ),
      temp_csv,
      row.names = FALSE
    )

    local_mocked_bindings(
      acm_data_available = function(...) TRUE,
      get_acm_data_path = function(...) temp_csv
    )

    expect_error(
      extract_acm_data(data_types = "yields", maturities = c(12, 18)),
      "only annual maturities",
      class = "hetid_error_insufficient_data"
    )

    # Annual nodes still work against the same source
    annual <- extract_acm_data(data_types = "yields", maturities = c(12, 24))
    expect_named(annual, c("date", "y12", "y24"))
  })
})
