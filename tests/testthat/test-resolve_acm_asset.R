test_that("unpinned resolution preserves cache preference without claiming provenance", {
  root <- withr::local_tempdir()
  withr::local_envvar(c(R_USER_DATA_DIR = root))
  result <- resolve_acm_asset()
  expect_identical(names(result), c("release", "sha256", "frequency", "source_url", "path"))
  expect_null(result$release)
  expect_null(result$sha256)
  expect_null(result$source_url)
  expect_identical(result$frequency, "monthly")
  expect_identical(basename(result$path), HETID_CONSTANTS$ACM_DATA_FILENAME)
  expect_true(file.exists(result$path))
  expect_false(dir.exists(tools::R_user_dir("hetid", "data")))
  cache <- tools::R_user_dir("hetid", "data")
  dir.create(cache, recursive = TRUE)
  cached <- file.path(cache, HETID_CONSTANTS$ACM_DATA_FILENAME)
  writeLines("decoy", cached)
  expect_identical(resolve_acm_asset()$path, cached)
  expect_identical(resolve_acm_asset("github")$path, cached)
})

test_that("daily and NY Fed descriptors are cache-only", {
  root <- withr::local_tempdir()
  withr::local_envvar(c(R_USER_DATA_DIR = root))
  cache <- tools::R_user_dir("hetid", "data")
  expect_identical(
    resolve_acm_asset(frequency = "daily")$path,
    file.path(cache, HETID_CONSTANTS$ACM_DAILY_DATA_FILENAME)
  )
  expect_identical(
    resolve_acm_asset(source = "nyfed")$path,
    file.path(cache, HETID_CONSTANTS$ACM_NYFED_FILENAME)
  )
  expect_false(dir.exists(cache))
  expect_error(resolve_acm_asset("nyfed", "daily"), class = "hetid_error_bad_argument")
})

test_that("pin descriptors preserve identity and escaped URLs without creating assets", {
  root <- withr::local_tempdir()
  withr::local_envvar(c(R_USER_DATA_DIR = root))
  local_mocked_bindings(download.file = function(...) stop("network forbidden"))
  digest <- strrep("A", 64)
  pin <- resolve_acm_asset("github", "daily", "a/b%20", digest)
  expect_identical(pin$release, "a/b%20")
  expect_identical(pin$sha256, tolower(digest))
  expect_identical(pin$frequency, "daily")
  expect_identical(pin$source_url, paste0(
    "https://github.com/fernando-duarte/ACM_term_premium/releases/download/",
    "a%2Fb%2520/", HETID_CONSTANTS$ACM_DAILY_DATA_FILENAME
  ))
  expect_true(startsWith(pin$path, file.path(tools::R_user_dir("hetid", "data"), "releases")))
  expect_false(file.exists(pin$path))
  expect_false(dir.exists(dirname(pin$path)))
  expect_identical(resolve_acm_asset("auto", "daily", "a/b%20", digest), pin)
})

test_that("public descriptors support offline pinned reads and discriminate corrupt pins", {
  x <- local_acm_pin_fixture()
  args <- c(pin_load_args(x), list(frequency = "daily"))
  pin <- do.call(resolve_acm_asset, args)
  dir.create(dirname(pin$path), recursive = TRUE)
  file.copy(x$fixture$gz, pin$path)
  write.dcf(list(
    release = pin$release, sha256 = pin$sha256, frequency = pin$frequency,
    source_url = pin$source_url, retrieved = "2026-01-01T00:00:00Z", acquisition = "download"
  ), paste0(pin$path, ".meta"))
  local_mocked_bindings(download.file = function(...) stop("network forbidden"))
  result <- do.call(load_term_premia, c(args, list(auto_download = FALSE)))
  expect_identical(result$date, as.Date("2020-01-31"))
  expect_identical(result$ACMY01, 1.5)
  generic <- resolve_acm_asset(frequency = "daily")$path
  writeLines("decoy", generic)
  writeLines("corrupt snapshot", pin$path)
  expect_identical(do.call(resolve_acm_asset, args), pin)
  expect_error(do.call(load_term_premia, c(args, list(auto_download = FALSE))),
    "sha256 mismatch or missing asset",
    class = "hetid_error"
  )
})

test_that("resolver selectors and paired pin arguments use structured conditions", {
  for (bad in list(NULL, NA_character_, 1, "", "other", c("auto", "github"), matrix("auto"))) {
    expect_error(resolve_acm_asset(source = bad), class = "hetid_error_bad_argument")
    expect_error(resolve_acm_asset(frequency = bad), class = "hetid_error_bad_argument")
  }
  for (args in list(
    list(release = "tag"), list(expected_sha256 = strrep("a", 64)),
    list(release = "tag", expected_sha256 = "bad"),
    list(release = "bad tag", expected_sha256 = strrep("a", 64)),
    list(source = "nyfed", release = "tag", expected_sha256 = strrep("a", 64))
  )) {
    expect_error(do.call(resolve_acm_asset, args), class = "hetid_error_bad_argument")
  }
})
