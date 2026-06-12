# Tests for data path resolution: per-user cache preferred,
# bundled package copy as read-only fallback

test_that("get_package_data_dir returns the bundled extdata path", {
  path <- get_package_data_dir()

  expect_type(path, "character")
  expect_length(path, 1)
  expect_true(grepl("extdata$", path))
})

test_that("get_data_file_path rejects non-string input", {
  expect_error(get_data_file_path(123), "single character string")
  expect_error(get_data_file_path(c("a", "b")), "single character string")
  expect_error(get_data_file_path(NULL), "single character string")
})

test_that("get_data_file_path falls back to the bundled copy", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

  path <- get_data_file_path(HETID_CONSTANTS$ACM_DATA_FILENAME)
  expect_true(grepl("extdata", path))
  expect_true(file.exists(path))
})

test_that("get_data_file_path prefers the user-cache copy when present", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  user_csv <- file.path(
    get_user_data_dir(create = TRUE), HETID_CONSTANTS$ACM_DATA_FILENAME
  )
  writeLines("user-copy", user_csv)

  path <- get_data_file_path(HETID_CONSTANTS$ACM_DATA_FILENAME)
  expect_identical(path, user_csv)
})

test_that("check_data_file_exists returns logical", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

  expect_false(check_data_file_exists("nonexistent_file.csv"))
  expect_true(check_data_file_exists(HETID_CONSTANTS$ACM_DATA_FILENAME))
})

test_that("get_acm_data_path resolves the bundled github-family file", {
  path <- get_acm_data_path()

  expect_true(grepl(HETID_CONSTANTS$ACM_DATA_FILENAME, path, fixed = TRUE))
  expect_true(file.exists(path))
})

test_that("auto resolution prefers a github cache and ignores nyfed", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  user_dir <- get_user_data_dir(create = TRUE)

  # A nyfed cache alone must not shadow the bundled github data
  writeLines("nyfed", file.path(user_dir, HETID_CONSTANTS$ACM_NYFED_FILENAME))
  expect_true(grepl("extdata", get_acm_data_path("auto")))

  # A github cache takes precedence over the bundled copy
  github_cache <- file.path(user_dir, HETID_CONSTANTS$ACM_DATA_FILENAME)
  writeLines("github", github_cache)
  expect_identical(get_acm_data_path("auto"), github_cache)
  expect_identical(get_acm_data_path("github"), github_cache)
})

test_that("nyfed resolution targets only the nyfed cache file", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

  path <- get_acm_data_path("nyfed")
  expect_identical(
    path,
    file.path(get_user_data_dir(), HETID_CONSTANTS$ACM_NYFED_FILENAME)
  )
  expect_false(acm_data_available("nyfed"))
  expect_true(acm_data_available("auto"))
})

test_that("legacy cache files earn a one-time advisory and are never resolved", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  user_dir <- get_user_data_dir(create = TRUE)
  # Both retired names: the old xls-to-CSV cache and the superseded
  # 6-month-floor release asset
  for (legacy in HETID_CONSTANTS$ACM_LEGACY_FILENAMES) {
    writeLines("legacy", file.path(user_dir, legacy))
  }

  # Reset the session flag so this test is order-independent
  rm(
    list = ls(envir = hetid:::.acm_path_state),
    envir = hetid:::.acm_path_state
  )
  expect_message(
    path <- get_acm_data_path(),
    regexp = "legacy ACM cache"
  )
  expect_false(basename(path) %in% HETID_CONSTANTS$ACM_LEGACY_FILENAMES)
  expect_no_message(get_acm_data_path())
})

test_that("get_user_data_dir respects R_USER_DATA_DIR", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

  user_dir <- get_user_data_dir()
  expect_identical(user_dir, file.path(user_root, "R", "hetid"))
  expect_false(dir.exists(user_dir))
})

test_that("get_user_data_dir creates the directory on demand", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

  created <- get_user_data_dir(create = TRUE)
  expect_true(dir.exists(created))

  # Creating again is a no-op
  expect_identical(get_user_data_dir(create = TRUE), created)
})

test_that("get_user_data_dir errors when the directory cannot be created", {
  # Point the cache under a regular file so dir.create() fails.
  blocker <- withr::local_tempfile()
  writeLines("", blocker)
  withr::local_envvar(R_USER_DATA_DIR = file.path(blocker, "sub"))

  expect_error(
    get_user_data_dir(create = TRUE),
    regexp = "Cannot create user data directory",
    class = "hetid_error"
  )
})

test_that("get_acm_download_path targets the per-source user cache", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

  path <- get_acm_download_path("github")
  expect_identical(
    path,
    file.path(user_root, "R", "hetid", HETID_CONSTANTS$ACM_DATA_FILENAME)
  )
  expect_true(dir.exists(dirname(path)))

  expect_identical(
    get_acm_download_path("nyfed"),
    file.path(user_root, "R", "hetid", HETID_CONSTANTS$ACM_NYFED_FILENAME)
  )
})
