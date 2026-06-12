# Test file for download functions
# Tests download_term_premia and load_term_premia with mocked network
# access and a throwaway per-user cache directory (no real downloads,
# no writes outside tempdir)

# --- load_term_premia mock-based tests ---

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

test_that("load_term_premia warns on unparseable dates", {
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

    expect_warning(
      result <- load_term_premia(),
      "Could not convert DATE"
    )
    expect_s3_class(result, "data.frame")
    expect_true("date" %in% names(result))
    expect_type(result$date, "character")
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

# --- download_term_premia mock-based tests ---

test_that("the nyfed source writes its own cache file, not the package", {
  skip_if_not_installed("readxl")
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

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
      invisible(0)
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
