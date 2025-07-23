# Test file for download functions
# Tests download_term_premia and download_yield_curve

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

test_that("download_yield_curve works correctly", {
  # Skip if offline
  skip_if_offline()

  # Test download for default dataset
  expect_silent(download_yield_curve(force = FALSE, quiet = TRUE))

  # Check file exists
  data_dir <- system.file("extdata", package = "hetid")
  yc_file <- file.path(data_dir, "feds200628.csv")

  # File should exist
  expect_true(file.exists(yc_file))

  # Test alternative dataset
  expect_silent(download_yield_curve(
    dataset = "feds200533",
    force = FALSE, quiet = TRUE
  ))

  alt_file <- file.path(data_dir, "feds200533.csv")
  expect_true(file.exists(alt_file))
})

test_that("download functions handle force parameter", {
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

test_that("download functions validate dataset parameter", {
  # Invalid dataset should error
  expect_error(download_yield_curve(dataset = "invalid_dataset"))

  # Valid datasets should work
  valid_datasets <- c("feds200628", "feds200533")

  for (dataset in valid_datasets) {
    # Should not error (but might not download if offline)
    expect_error(download_yield_curve(dataset = dataset, quiet = TRUE), NA)
  }
})

test_that("load functions work after download", {
  # Skip if offline
  skip_if_offline()

  # Ensure data is downloaded
  download_term_premia(force = FALSE, quiet = TRUE)
  download_yield_curve(force = FALSE, quiet = TRUE)

  # Load should work
  tp_data <- load_term_premia()
  expect_s3_class(tp_data, "data.frame")
  expect_true(nrow(tp_data) > 0)
  expect_true("date" %in% names(tp_data))

  yc_data <- load_yield_curve()
  expect_s3_class(yc_data, "data.frame")
  expect_true(nrow(yc_data) > 0)
  expect_true("date" %in% names(yc_data))
})

test_that("download functions quiet parameter", {
  # Skip if offline
  skip_if_offline()

  # With quiet = TRUE, should not print messages
  expect_silent(download_term_premia(force = FALSE, quiet = TRUE))

  # Note: Testing quiet = FALSE would require capturing output,
  # which is environment-dependent
})

test_that("download error handling", {
  # Test with invalid URL (simulate by using wrong dataset)
  # This should give informative error
  expect_error(download_yield_curve(dataset = "nonexistent"),
    class = "error"
  )
})

test_that("file permissions after download", {
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
