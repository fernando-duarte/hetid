test_that("download_term_premia errors on download failure", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

  local_mocked_bindings(
    download.file = function(...) stop("network error"),
    .package = "hetid"
  )

  # github flow: the release-metadata fetch is the first network touch
  expect_error(
    download_term_premia(force = TRUE, quiet = TRUE),
    "Failed to download",
    class = "hetid_error"
  )

  # nyfed flow fails closed too
  skip_if_not_installed("readxl")
  expect_error(
    download_term_premia(source = "nyfed", force = TRUE, quiet = TRUE),
    "Failed to download",
    class = "hetid_error"
  )
})

test_that("nyfed cache is written to a temp file, then renamed into place", {
  # Temp+rename (mirrors the github source): a partial write never half-overwrites
  # the cache without deleting the existing target first
  skip_if_not_installed("readxl")
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  user_csv <- file.path(
    get_user_data_dir(create = TRUE), HETID_CONSTANTS$ACM_NYFED_FILENAME
  )
  writeLines("OLD", user_csv)

  written_path <- NULL
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("fake-xls", destfile)
      invisible(0L)
    },
    write.csv = function(x, file, ...) {
      written_path <<- file
      utils::write.csv(x, file, ...)
    },
    .package = "hetid"
  )
  local_mocked_bindings(
    read_excel = function(...) {
      data.frame(DATE = "01-Jan-2020", ACMY01 = 1.5)
    },
    .package = "readxl"
  )

  download_term_premia(source = "nyfed", force = TRUE, quiet = TRUE)

  # The write targeted a temp file in the cache dir, never the cache path
  expect_false(identical(written_path, user_csv))
  expect_identical(dirname(written_path), dirname(user_csv))
  # The rename left the real cache holding the new content
  expect_equal(nrow(read.csv(user_csv)), 1)
})

test_that("nyfed load without a cache raises a structured error", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

  expect_error(
    load_term_premia(source = "nyfed"),
    "NY Fed ACM cache not found",
    class = "hetid_error_insufficient_data"
  )
})

test_that("nyfed auto_download triggers despite available github data", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  requested_source <- NULL

  local_mocked_bindings(
    download_term_premia = function(source = c("github", "nyfed"), ...) {
      source <- match.arg(source)
      requested_source <<- source
      target <- get_acm_download_path(source)
      write.csv(
        data.frame(DATE = "2020-01-31", ACMY01 = 1.5),
        target,
        row.names = FALSE
      )
      invisible(target)
    }
  )

  # Bundled github data is available, but the explicit nyfed request
  # must still download the nyfed source
  expect_true(acm_data_available("auto"))
  expect_message(
    result <- load_term_premia(auto_download = TRUE, source = "nyfed"),
    "Downloading"
  )
  expect_identical(requested_source, "nyfed")
  expect_s3_class(result, "data.frame")
})
