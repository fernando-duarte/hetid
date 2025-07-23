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
