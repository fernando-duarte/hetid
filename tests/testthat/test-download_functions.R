# Test file for download functions
# Tests download_term_premia

test_that("download_term_premia works correctly", {
  # Skip if offline
  skip_if_offline()

  # Test download (force = FALSE to use cached if available)
  expect_silent(download_term_premia(force = FALSE, quiet = TRUE))

  # Check file exists after download
  data_dir <- system.file("extdata", package = "hetid")
  tp_file <- file.path(data_dir, "ACMTermPremium.csv")

  # File should exist (either downloaded or already present)
  expect_true(file.exists(tp_file))

  # File should have content
  if (file.exists(tp_file)) {
    file_size <- file.info(tp_file)$size
    expect_true(file_size > 1000) # Should be at least 1KB
  }
})

test_that("download_term_premia handles force parameter", {
  # Skip if offline
  skip_if_offline()

  # Get file path
  data_dir <- system.file("extdata", package = "hetid")
  tp_file <- file.path(data_dir, "ACMTermPremium.csv")

  # If file exists, check modification time
  if (file.exists(tp_file)) {
    old_mtime <- file.info(tp_file)$mtime

    # Download with force = FALSE (should not re-download)
    download_term_premia(force = FALSE, quiet = TRUE)
    new_mtime <- file.info(tp_file)$mtime

    # Modification time should be unchanged
    expect_equal(old_mtime, new_mtime)
  }
})

test_that("load_term_premia works after download", {
  # Skip if offline
  skip_if_offline()

  # Ensure data is downloaded
  download_term_premia(force = FALSE, quiet = TRUE)

  # Load should work
  tp_data <- load_term_premia()
  expect_s3_class(tp_data, "data.frame")
  expect_true(nrow(tp_data) > 0)
  expect_true("date" %in% names(tp_data))

  # Check that ACM data contains both yields and term premia
  yield_cols <- grep("^ACMY", names(tp_data), value = TRUE)
  tp_cols <- grep("^ACMTP", names(tp_data), value = TRUE)
  rny_cols <- grep("^ACMRNY", names(tp_data), value = TRUE)

  expect_true(length(yield_cols) > 0)
  expect_true(length(tp_cols) > 0)
  expect_true(length(rny_cols) > 0)
})

test_that("download_term_premia quiet parameter", {
  # Skip if offline
  skip_if_offline()

  # With quiet = TRUE, should not print messages
  expect_silent(download_term_premia(force = FALSE, quiet = TRUE))

  # Note: Testing quiet = FALSE would require capturing output,
  # which is environment-dependent
})

test_that("download_term_premia file permissions", {
  # Skip if offline
  skip_if_offline()

  # Download files
  download_term_premia(force = FALSE, quiet = TRUE)

  # Check file is readable
  data_dir <- system.file("extdata", package = "hetid")
  tp_file <- file.path(data_dir, "ACMTermPremium.csv")

  if (file.exists(tp_file)) {
    # Should be able to read first few lines
    expect_silent(readLines(tp_file, n = 5))
  }
})

# --- load_term_premia mock-based tests ---

test_that("load_term_premia auto-downloads when file missing", {
  withr::with_tempdir({
    temp_csv <- file.path(getwd(), "ACMTermPremium.csv")

    local_mocked_bindings(
      check_data_file_exists = function(...) FALSE,
      get_acm_data_path = function() temp_csv,
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

test_that("load_term_premia warns on unparseable dates", {
  withr::with_tempdir({
    temp_csv <- file.path(getwd(), "ACMTermPremium.csv")
    write.csv(
      data.frame(DATE = "not-a-date", ACMY01 = 1.0),
      temp_csv,
      row.names = FALSE
    )

    local_mocked_bindings(
      check_data_file_exists = function(...) TRUE,
      get_acm_data_path = function() temp_csv
    )

    expect_warning(
      result <- load_term_premia(),
      "Could not convert DATE"
    )
    expect_s3_class(result, "data.frame")
    expect_true("date" %in% names(result))
    expect_type(result$date, "character")
  })
})

test_that(
  "load_term_premia errors on read failure",
  {
    local_mocked_bindings(
      check_data_file_exists = function(...) TRUE,
      get_acm_data_path = function() {
        "/nonexistent/path.csv"
      }
    )

    expect_error(
      load_term_premia(),
      class = "hetid_error"
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
      check_data_file_exists = function(...) TRUE,
      get_acm_data_path = function() temp_csv
    )

    result <- load_term_premia()
    expect_s3_class(result$date, "Date")
    expect_equal(
      result$date,
      as.Date(c("2020-01-15", "2020-02-15"))
    )
  })
})

# --- download_term_premia mock-based tests ---

test_that("download_term_premia downloads and saves CSV", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "ACMTermPremium.csv")

    local_mocked_bindings(
      validate_data_directory = function(...) invisible(TRUE),
      get_acm_data_path = function() csv_path
    )
    local_mocked_bindings(
      download.file = function(url, destfile, ...) {
        writeLines("fake-xls", destfile)
        invisible(0)
      },
      .package = "hetid"
    )
    local_mocked_bindings(
      read_excel = function(...) {
        data.frame(
          DATE = c("01-Jan-2020", "01-Feb-2020"),
          ACMY01 = c(1.5, 1.6)
        )
      },
      .package = "readxl"
    )

    result <- expect_message(
      download_term_premia(force = TRUE, quiet = FALSE),
      "Downloading"
    )

    expect_true(file.exists(csv_path))
    saved <- read.csv(csv_path)
    expect_equal(nrow(saved), 2)
  })
})

test_that("download_term_premia skips when file exists", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "ACMTermPremium.csv")
    writeLines("existing", csv_path)

    local_mocked_bindings(
      validate_data_directory = function(...) invisible(TRUE),
      get_acm_data_path = function() csv_path
    )

    result <- expect_message(
      download_term_premia(force = FALSE, quiet = FALSE),
      "already exists"
    )
    expect_equal(readLines(csv_path), "existing")
  })
})

test_that("download_term_premia quiet mode suppresses messages", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "ACMTermPremium.csv")
    writeLines("existing", csv_path)

    local_mocked_bindings(
      validate_data_directory = function(...) invisible(TRUE),
      get_acm_data_path = function() csv_path
    )

    expect_silent(
      download_term_premia(force = FALSE, quiet = TRUE)
    )
  })
})

test_that("download_term_premia errors on download failure", {
  withr::with_tempdir({
    csv_path <- file.path(getwd(), "ACMTermPremium.csv")

    local_mocked_bindings(
      validate_data_directory = function(...) invisible(TRUE),
      get_acm_data_path = function() csv_path
    )
    local_mocked_bindings(
      download.file = function(...) stop("network error"),
      .package = "hetid"
    )

    expect_error(
      download_term_premia(force = TRUE, quiet = TRUE),
      "Failed to download"
    )
  })
})
