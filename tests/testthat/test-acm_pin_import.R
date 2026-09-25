test_that("matching generic caches import without modifying the original", {
  x <- local_acm_pin_fixture()
  generic <- get_acm_download_path("github", "daily")
  file.copy(x$fixture$gz, generic)
  writeLines("old provenance", paste0(generic, ".meta"))
  before <- tools::sha256sum(c(generic, paste0(generic, ".meta")))
  local_mocked_bindings(download.file = function(...) stop("network forbidden"))
  args <- c(x$args, list(frequency = "daily"))
  expect_error(do.call(load_term_premia, args[names(args) != "quiet"]),
    class = "hetid_error_insufficient_data"
  )
  path <- do.call(download_term_premia, args)
  expect_false(identical(path, generic))
  expect_identical(
    unname(read.dcf(paste0(path, ".meta"))[1, "acquisition"]),
    "verified-existing-asset"
  )
  expect_identical(tools::sha256sum(c(generic, paste0(generic, ".meta"))), before)
})

test_that("unrelated generic caches remain untouched while pins download", {
  x <- local_acm_pin_fixture()
  generic <- get_acm_download_path("github", "daily")
  writeLines("unrelated vintage", generic)
  before <- tools::sha256sum(generic)
  path <- do.call(download_term_premia, c(x$args, list(frequency = "daily")))
  expect_identical(tools::sha256sum(generic), before)
  expect_identical(unname(tools::sha256sum(path)), x$fixture$sha)
})

test_that("copied cache bytes are reverified before publication", {
  x <- local_acm_pin_fixture()
  generic <- get_acm_download_path("github", "daily")
  file.copy(x$fixture$gz, generic)
  local_mocked_bindings(file.copy = function(from, to, ...) {
    writeLines("changed between hash and copy", to)
    TRUE
  }, .package = "base")
  expect_error(do.call(download_term_premia, c(x$args, list(frequency = "daily"))),
    class = "hetid_error"
  )
  expect_identical(unname(tools::sha256sum(generic)), x$fixture$sha)
})

test_that("an identical concurrent publisher can win without replacement", {
  x <- local_acm_pin_fixture()
  rename <- base::file.rename
  local_mocked_bindings(file.rename = function(from, to) {
    rename(from, to)
    FALSE
  }, .package = "base")
  path <- do.call(download_term_premia, x$args)
  expect_identical(do.call(load_term_premia, pin_load_args(x))$ACMY01, 1.5)
  expect_identical(unname(tools::sha256sum(path)), x$fixture$sha)
})


test_that("forcing a matching existing asset still requires a fresh download", {
  x <- local_acm_pin_fixture()
  generic <- get_acm_download_path("github", "daily")
  file.copy(x$fixture$gz, generic)
  transfers <- 0L
  local_mocked_bindings(download.file = function(url, destfile, ...) {
    transfers <<- transfers + 1L
    mock_github_download(x$fixture)(url, destfile, ...)
  })
  args <- c(x$args, list(frequency = "daily"))
  path <- do.call(download_term_premia, args)
  expect_identical(transfers, 0L)
  before <- tools::sha256sum(c(path, paste0(path, ".meta")))
  expect_identical(do.call(download_term_premia, c(args, list(force = TRUE))), path)
  expect_identical(transfers, 1L)
  expect_identical(tools::sha256sum(c(path, paste0(path, ".meta"))), before)
})

test_that("a public daily extraction reads exactly the requested pinned asset", {
  x <- local_acm_pin_fixture()
  result <- do.call(extract_acm_data, c(pin_load_args(x), list(
    frequency = "daily", auto_download = TRUE, data_types = "yields", maturities = 12
  )))
  expect_identical(result, data.frame(date = as.Date("2020-01-31"), y12 = 1.5))
})
