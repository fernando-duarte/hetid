# Daily-frequency ACM asset: download, load, extract paths. GitHub-only
# and cache-only (never bundled); network mocked

# Write a tiny business-day csv.gz to the daily cache path
write_daily_cache <- function() {
  path <- get_acm_download_path("github", "daily")
  con <- gzfile(path, "wb")
  writeLines(
    c("DATE,ACMY01", "2020-01-02,1.5", "2020-01-03,1.6", "2020-01-06,1.7"),
    con
  )
  close(con)
  path
}

daily_dates <- as.Date(c("2020-01-02", "2020-01-03", "2020-01-06"))

test_that("daily github download verifies the digest and caches the daily asset", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  fixture <- make_github_fixture(
    withr::local_tempdir(),
    asset_name = HETID_CONSTANTS$ACM_DAILY_DATA_FILENAME
  )

  local_mocked_bindings(
    download.file = mock_github_download(fixture),
    .package = "hetid"
  )

  # No force: the bundled monthly copy must not suppress a daily download
  result <- download_term_premia(frequency = "daily", quiet = TRUE)

  expect_identical(result, get_acm_download_path("github", "daily"))
  expect_true(file.exists(result))
  expect_identical(unname(tools::sha256sum(result)), fixture$sha)

  # Provenance sidecar records the digest; the monthly cache is untouched
  meta <- readLines(paste0(result, ".meta"))
  expect_true(any(grepl(fixture$sha, meta, fixed = TRUE)))
  expect_false(file.exists(get_acm_download_path("github")))
})

test_that("download_term_premia rejects the nyfed source for daily data", {
  expect_error(
    download_term_premia(
      source = "nyfed", frequency = "daily", force = TRUE, quiet = TRUE
    ),
    "only available from the GitHub source",
    class = "hetid_error_bad_argument"
  )
})

test_that("download_term_premia skips when the daily cache exists", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  cache_path <- write_daily_cache()

  expect_message(
    result <- download_term_premia(frequency = "daily"),
    "already exists"
  )
  expect_identical(result, cache_path)
})

test_that("load_term_premia daily reads the daily cache", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  write_daily_cache()

  result <- load_term_premia(frequency = "daily")

  expect_s3_class(result, "data.frame")
  expect_identical(result$date, daily_dates)
})

test_that("load_term_premia daily missing hints at the daily download", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

  expect_message(
    result <- load_term_premia(frequency = "daily"),
    'frequency = "daily"',
    fixed = TRUE
  )
  expect_null(result)
})

test_that("load_term_premia auto-download propagates the daily frequency", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  seen <- NULL

  local_mocked_bindings(
    download_term_premia = function(source, force = FALSE, quiet = FALSE,
                                    frequency = "monthly") {
      seen <<- frequency
      write_daily_cache()
    },
    .package = "hetid"
  )

  result <- suppressMessages(
    load_term_premia(auto_download = TRUE, frequency = "daily")
  )

  expect_identical(seen, "daily")
  expect_identical(result$date, daily_dates)
})

test_that("extract_acm_data daily returns business-day rows unchanged", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  write_daily_cache()

  result <- extract_acm_data(
    data_types = "yields", maturities = 12, frequency = "daily"
  )

  # No month-end relabeling: the business-day dates come through as-is
  expect_identical(names(result), c("date", "y12"))
  expect_identical(result$date, daily_dates)
})

test_that("extract_acm_data daily without a cache names the daily download", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

  expect_error(
    suppressMessages(extract_acm_data(frequency = "daily")),
    'frequency = "daily"',
    fixed = TRUE,
    class = "hetid_error_insufficient_data"
  )
})
