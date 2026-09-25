test_that("the nyfed source writes its own cache file, not the package", {
  skip_if_not_installed("readxl")
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("fake-xls", destfile)
      invisible(0L)
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

  bundled_path <- file.path(
    get_package_data_dir(), HETID_CONSTANTS$ACM_DATA_FILENAME
  )
  bundled_mtime <- file.info(bundled_path)$mtime

  expect_message(
    result <- download_term_premia(source = "nyfed", quiet = FALSE),
    "Downloading"
  )

  user_csv <- file.path(
    get_user_data_dir(), HETID_CONSTANTS$ACM_NYFED_FILENAME
  )
  expect_true(file.exists(user_csv))
  expect_equal(result, user_csv)
  saved <- read.csv(user_csv)
  expect_equal(nrow(saved), 2)

  # The bundled copy in the package library must be untouched
  expect_identical(file.info(bundled_path)$mtime, bundled_mtime)

  # The bundled github-family data never suppresses an explicit nyfed
  # download: the call above ran even though bundled data exists
  expect_true(acm_data_available("auto"))
})

test_that("download_term_premia skips when the bundled copy satisfies it", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

  expect_message(
    result <- download_term_premia(force = FALSE, quiet = FALSE),
    "already exists"
  )

  expect_true(file.exists(result))

  # No download happened, so the user cache stays empty
  user_csv <- file.path(
    get_user_data_dir(), HETID_CONSTANTS$ACM_DATA_FILENAME
  )
  expect_false(file.exists(user_csv))
})

test_that("download_term_premia skips when the user copy exists", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  user_csv <- file.path(
    get_user_data_dir(create = TRUE), HETID_CONSTANTS$ACM_DATA_FILENAME
  )
  writeLines("existing", user_csv)

  expect_message(
    result <- download_term_premia(force = FALSE, quiet = FALSE),
    "already exists"
  )
  expect_equal(result, user_csv)
  expect_equal(readLines(user_csv), "existing")
})

test_that("download_term_premia quiet mode suppresses messages", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

  expect_silent(
    download_term_premia(force = FALSE, quiet = TRUE)
  )
})

test_that("force re-download overwrites the nyfed cache only", {
  skip_if_not_installed("readxl")
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  user_csv <- file.path(
    get_user_data_dir(create = TRUE), HETID_CONSTANTS$ACM_NYFED_FILENAME
  )
  writeLines("stale", user_csv)

  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("fake-xls", destfile)
      invisible(0L)
    },
    .package = "hetid"
  )
  local_mocked_bindings(
    read_excel = function(...) {
      data.frame(DATE = "01-Jan-2020", ACMY01 = 1.5)
    },
    .package = "readxl"
  )

  bundled_path <- file.path(
    get_package_data_dir(), HETID_CONSTANTS$ACM_DATA_FILENAME
  )
  bundled_first_line <- readLines(bundled_path, n = 1)

  download_term_premia(source = "nyfed", force = TRUE, quiet = TRUE)

  expect_false(identical(readLines(user_csv), "stale"))
  expect_identical(readLines(bundled_path, n = 1), bundled_first_line)
})
