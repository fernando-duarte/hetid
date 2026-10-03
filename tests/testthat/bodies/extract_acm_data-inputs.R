{
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
}

{
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
}

{
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
}

{
  # recycling would otherwise filter rows on alternating parity, silently
  expect_error(
    extract_acm_data(
      data_types = "yields", maturities = 12,
      start_date = c("2000-01-01", "2010-01-01")
    ),
    "start_date must be a single date",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    extract_acm_data(
      data_types = "yields", maturities = 12,
      end_date = c("2000-01-01", "2010-01-01")
    ),
    "end_date must be a single date",
    class = "hetid_error_bad_argument"
  )
}

{
  acm_data <- data.frame(
    date = as.Date(c("2020-01-30", "2020-02-27")),
    y1 = c(1, 2)
  )

  # the January label 2020-01-31 is >= the bound, so the row survives
  from_label <- filter_acm_date_range(acm_data, as.Date("2020-01-31"), NULL)
  expect_equal(nrow(from_label), 2)

  # the end side compares the raw date, keeping real-time availability
  to_raw <- filter_acm_date_range(acm_data, NULL, as.Date("2020-01-31"))
  expect_equal(nrow(to_raw), 1)
  expect_equal(to_raw$y1, 1)
}

{
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
}
