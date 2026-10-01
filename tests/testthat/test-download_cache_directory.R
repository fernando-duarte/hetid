test_that("existing cache directories fail before a success message or download", {
  withr::local_options(warn = 2)
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  local_mocked_bindings(
    download_acm_github = function(...) testthat::fail("Unexpected GitHub download"),
    download_acm_nyfed = function(...) testthat::fail("Unexpected NY Fed download"),
    .package = "hetid"
  )
  cases <- list(c("github", "monthly"), c("github", "daily"), c("nyfed", "monthly"))
  for (case in cases) {
    path <- get_acm_download_path(case[1], case[2])
    expect_identical(dirname(path), tools::R_user_dir("hetid", "data"))
    expect_true(startsWith(path, user_root))
    dir.create(path)
    for (quiet in c(FALSE, TRUE)) {
      expect_silent(err <- tryCatch(
        download_term_premia(source = case[1], frequency = case[2], quiet = quiet),
        error = identity
      ))
      expect_s3_class(err, "hetid_error")
      message <- if (inherits(err, "condition")) conditionMessage(err) else ""
      expect_match(message, path, fixed = TRUE)
      expect_true(dir.exists(path))
    }
  }
})

test_that("existing regular cache files stay unchecked and invisible", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  local_mocked_bindings(
    download_acm_github = function(...) testthat::fail("Unexpected GitHub download"),
    download_acm_nyfed = function(...) testthat::fail("Unexpected NY Fed download"),
    .package = "hetid"
  )
  cases <- list(c("github", "monthly"), c("github", "daily"), c("nyfed", "monthly"))
  for (case in cases) {
    path <- get_acm_download_path(case[1], case[2])
    writeLines("Unchecked local bytes", path)
    expect_message(
      result <- withVisible(download_term_premia(source = case[1], frequency = case[2])),
      "already exists"
    )
    expect_false(result$visible)
    expect_identical(result$value, path)
    expect_identical(readLines(path), "Unchecked local bytes")
    expect_silent(quiet <- withVisible(download_term_premia(
      source = case[1], frequency = case[2], quiet = TRUE
    )))
    expect_identical(quiet, result)
  }
})

test_that("forced calls retain source and frequency routing with directory caches", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  local_mocked_bindings(
    download_acm_github = function(quiet, frequency) list(quiet, frequency),
    download_acm_nyfed = function(quiet) list(quiet, "nyfed"),
    .package = "hetid"
  )
  for (frequency in c("monthly", "daily")) {
    path <- get_acm_download_path("github", frequency)
    dir.create(path)
    expect_identical(
      download_term_premia(force = TRUE, quiet = TRUE, frequency = frequency),
      list(TRUE, frequency)
    )
  }
  dir.create(get_acm_download_path("nyfed", "monthly"))
  expect_identical(
    download_term_premia(source = "nyfed", force = TRUE, quiet = TRUE),
    list(TRUE, "nyfed")
  )
})
