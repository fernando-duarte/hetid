{
  data <- extract_acm_data(maturities = c(24, 60, 120))

  expect_true(all(c("y24", "y60", "y120") %in% names(data)))
  expect_true(all(c("tp24", "tp60", "tp120") %in% names(data)))

  expect_false("y12" %in% names(data))
  expect_false("y36" %in% names(data))

  data_single <- extract_acm_data(
    data_types = "yields",
    maturities = 60
  )
  expect_true("y60" %in% names(data_single))
  expect_equal(sum(grepl("^y\\d+$", names(data_single))), 1)
}

{
  start_date <- "2010-01-01"
  end_date <- "2015-12-31"

  data <- extract_acm_data(
    start_date = start_date,
    end_date = end_date
  )

  expect_true(all(data$date >= as.Date(start_date)))
  expect_true(all(data$date <= as.Date(end_date)))

  data_start <- extract_acm_data(start_date = "2020-01-01")
  expect_true(all(data_start$date >= as.Date("2020-01-01")))

  data_end <- extract_acm_data(end_date = "2010-12-31")
  expect_true(all(data_end$date <= as.Date("2010-12-31")))
}

{
  data_yields <- extract_acm_data(data_types = "yields")
  expect_true(all(paste0("y", seq(12, 120, 12)) %in% names(data_yields)))
  expect_false(any(paste0("tp", seq(12, 120, 12)) %in% names(data_yields)))
  expect_false(any(paste0("rn", seq(12, 120, 12)) %in% names(data_yields)))

  data_tp <- extract_acm_data(data_types = "term_premia")
  expect_true(all(paste0("tp", seq(12, 120, 12)) %in% names(data_tp)))
  expect_false(any(paste0("y", seq(12, 120, 12)) %in% names(data_tp)))

  data_all <- extract_acm_data(
    data_types = c("yields", "term_premia", "risk_neutral_yields")
  )
  expect_true(all(paste0("y", seq(12, 120, 12)) %in% names(data_all)))
  expect_true(all(paste0("tp", seq(12, 120, 12)) %in% names(data_all)))
  expect_true(all(paste0("rny", seq(12, 120, 12)) %in% names(data_all)))
}

{
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

  expect_error(
    extract_acm_data(data_types = "invalid"),
    "Invalid data_types"
  )
}

{
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
}

{
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
}

{
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
    # an incomplete/corrupt source, signaled rather than silently dropped
    expect_error(
      extract_acm_data(data_types = "yields", maturities = 60),
      "missing required column",
      class = "hetid_error_insufficient_data"
    )
  })
}

{
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
}

{
  # term premium = yield - risk-neutral yield
  data <- extract_acm_data(
    data_types = c("yields", "term_premia", "risk_neutral_yields"),
    maturities = c(24, 60, 120)
  )

  for (mat in c(24, 60, 120)) {
    y_col <- paste0("y", mat)
    tp_col <- paste0("tp", mat)
    rny_col <- paste0("rny", mat)

    # Allow small tolerance for rounding
    calculated_tp <- data[[y_col]] - data[[rny_col]]
    expect_equal(data[[tp_col]], calculated_tp, tolerance = 1e-6)
  }
}
