test_that("get_package_data_dir returns a character path", {
  path <- get_package_data_dir()

  expect_type(path, "character")
  expect_length(path, 1)
  expect_true(grepl("extdata$", path))
})

test_that("get_data_file_path appends filename to data dir", {
  path <- get_data_file_path("test.csv")

  expect_true(grepl("extdata", path))
  expect_true(grepl("test\\.csv$", path))
})

test_that("get_data_file_path rejects non-string input", {
  expect_error(get_data_file_path(123), "single character string")
  expect_error(get_data_file_path(c("a", "b")), "single character string")
  expect_error(get_data_file_path(NULL), "single character string")
})

test_that("check_data_file_exists returns logical", {
  result <- check_data_file_exists("nonexistent_file.csv")

  expect_type(result, "logical")
  expect_false(result)
})

test_that("get_acm_data_path points to ACMTermPremium.csv", {
  path <- get_acm_data_path()

  expect_true(grepl("ACMTermPremium\\.csv$", path))
})

test_that("validate_data_directory returns TRUE for existing dir", {
  # The installed package extdata should exist
  skip_if_not_installed("hetid")

  result <- validate_data_directory(create_if_missing = FALSE)
  expect_true(result)
})

test_that("validate_data_directory errors when missing and create FALSE", {
  withr::with_tempdir({
    mock_dir <- file.path(
      getwd(), "nonexistent", "extdata"
    )
    local_mocked_bindings(
      get_package_data_dir = function() mock_dir
    )
    expect_error(
      validate_data_directory(create_if_missing = FALSE),
      "Data directory does not exist"
    )
  })
})

test_that("validate_data_directory creates dir when missing", {
  withr::with_tempdir({
    new_dir <- file.path(getwd(), "newpkg", "extdata")
    expect_false(dir.exists(new_dir))

    local_mocked_bindings(
      get_package_data_dir = function() new_dir
    )
    result <- validate_data_directory(
      create_if_missing = TRUE
    )
    expect_true(result)
    expect_true(dir.exists(new_dir))
  })
})

test_that("validate_data_directory warns for non-writable dir", {
  skip_on_os("windows")
  withr::with_tempdir({
    ro_dir <- file.path(getwd(), "readonly_extdata")
    dir.create(ro_dir)
    Sys.chmod(ro_dir, mode = "0444")
    withr::defer(Sys.chmod(ro_dir, mode = "0755"))

    local_mocked_bindings(
      get_package_data_dir = function() ro_dir
    )
    expect_warning(
      validate_data_directory(create_if_missing = FALSE),
      "may not be writable"
    )
  })
})

test_that("validate_data_directory wraps creation errors", {
  skip_on_os("windows")
  withr::with_tempdir({
    bad_dir <- file.path(getwd(), "impossible", "extdata")
    local_mocked_bindings(
      get_package_data_dir = function() bad_dir
    )
    # Mock dir.create to simulate an error condition
    # that the tryCatch handler guards against
    local_mocked_bindings(
      dir.create = function(...) {
        stop("simulated permission error")
      },
      .package = "base"
    )
    expect_error(
      validate_data_directory(create_if_missing = TRUE),
      "Cannot create data directory"
    )
  })
})
