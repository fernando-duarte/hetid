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

test_that("validate_data_directory errors for missing dir", {
  # Mock a non-existent path by temporarily overriding
  # Just test the create_if_missing = FALSE branch with
  # a path that doesn't exist
  skip("Requires mocking get_package_data_dir")
})
