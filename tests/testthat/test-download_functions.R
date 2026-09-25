# download_term_premia and load_term_premia with mocked network access and a
# throwaway per-user cache dir (no real downloads, no writes outside tempdir)

test_that("load_term_premia auto-downloads when file missing", {
  withr::with_tempdir({
    temp_csv <- file.path(getwd(), "ACMTermPremium.csv")

    local_mocked_bindings(
      acm_data_available = function(...) FALSE,
      get_acm_data_path = function(...) temp_csv,
      download_term_premia = function(...) {
        write.csv(
          data.frame(
            DATE = "2020-01-01", ACMY01 = 1.5
          ),
          temp_csv,
          row.names = FALSE
        )
        invisible(temp_csv)
      }
    )

    expect_message(
      result <- load_term_premia(auto_download = TRUE),
      "not found.*Downloading"
    )
    expect_s3_class(result, "data.frame")
    expect_true(file.exists(temp_csv))
  })
})

test_that("load_term_premia errors on a wholly unparseable DATE column", {
  withr::with_tempdir({
    temp_csv <- file.path(getwd(), "ACMTermPremium.csv")
    write.csv(
      data.frame(DATE = "not-a-date", ACMY01 = 1.0),
      temp_csv,
      row.names = FALSE
    )

    local_mocked_bindings(
      acm_data_available = function(...) TRUE,
      get_acm_data_path = function(...) temp_csv
    )

    # A DATE column that no supported format can parse is treated as a
    # stale/corrupt cache (structured error), not silently kept as text
    expect_error(
      load_term_premia(),
      "could not be parsed",
      class = "hetid_error"
    )
  })
})

test_that("load_term_premia warns when chosen format leaves some NA dates", {
  withr::with_tempdir({
    temp_csv <- file.path(getwd(), "ACMTermPremium.csv")
    write.csv(
      data.frame(
        DATE = c("2020-01-15", "garbage"),
        ACMY01 = c(1.5, 1.6)
      ),
      temp_csv,
      row.names = FALSE
    )

    local_mocked_bindings(
      acm_data_available = function(...) TRUE,
      get_acm_data_path = function(...) temp_csv
    )

    expect_warning(
      result <- load_term_premia(),
      "could not be parsed"
    )
    expect_s3_class(result$date, "Date")
    expect_equal(result$date[1], as.Date("2020-01-15"))
    expect_true(is.na(result$date[2]))
  })
})

test_that("load_term_premia parses ACM-format dates", {
  withr::with_tempdir({
    temp_csv <- file.path(getwd(), "ACMTermPremium.csv")
    write.csv(
      data.frame(
        DATE = c("30-Jun-1961", "31-Jul-1961"),
        ACMY01 = c(1.5, 1.6)
      ),
      temp_csv,
      row.names = FALSE
    )

    local_mocked_bindings(
      acm_data_available = function(...) TRUE,
      get_acm_data_path = function(...) temp_csv
    )

    result <- load_term_premia()
    expect_equal(
      result$date,
      as.Date(c("1961-06-30", "1961-07-31"))
    )
  })
})

test_that(
  "load_term_premia errors on read failure",
  {
    local_mocked_bindings(
      acm_data_available = function(...) TRUE,
      get_acm_data_path = function(...) {
        "/nonexistent/path.csv"
      }
    )

    expect_warning(
      expect_error(
        load_term_premia(),
        class = "hetid_error"
      ),
      "cannot open file"
    )
  }
)

test_that("load_term_premia parses ISO dates correctly", {
  withr::with_tempdir({
    temp_csv <- file.path(getwd(), "ACMTermPremium.csv")
    write.csv(
      data.frame(
        DATE = c("2020-01-15", "2020-02-15"),
        ACMY01 = c(1.5, 1.6)
      ),
      temp_csv,
      row.names = FALSE
    )

    local_mocked_bindings(
      acm_data_available = function(...) TRUE,
      get_acm_data_path = function(...) temp_csv
    )

    result <- load_term_premia()
    expect_s3_class(result$date, "Date")
    expect_equal(
      result$date,
      as.Date(c("2020-01-15", "2020-02-15"))
    )
  })
})

test_that("load_term_premia parses a lowercase date header", {
  withr::with_tempdir({
    temp_csv <- file.path(getwd(), "ACMTermPremium.csv")
    write.csv(
      data.frame(
        date = c("2020-01-15", "2020-02-15"),
        ACMY01 = c(1.5, 1.6)
      ),
      temp_csv,
      row.names = FALSE
    )

    local_mocked_bindings(
      acm_data_available = function(...) TRUE,
      get_acm_data_path = function(...) temp_csv
    )

    # lowercase `date` is admitted by validate_acm_schema and must parse to Date
    result <- load_term_premia()
    expect_s3_class(result$date, "Date")
    expect_equal(
      result$date,
      as.Date(c("2020-01-15", "2020-02-15"))
    )
  })
})
