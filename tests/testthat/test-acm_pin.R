test_that("explicit pins download by tag, round-trip, and reuse offline", {
  x <- local_acm_pin_fixture()
  urls <- character()
  local_mocked_bindings(download.file = function(url, destfile, ...) {
    urls <<- c(urls, url)
    mock_github_download(x$fixture)(url, destfile, ...)
  })
  path <- do.call(download_term_premia, x$args)
  expect_match(urls, "/download/test-release/", fixed = TRUE)
  expect_length(urls, 1L)
  meta <- read.dcf(paste0(path, ".meta"))
  expect_identical(unname(meta[1, "release"]), "test-release")
  expect_identical(unname(meta[1, "acquisition"]), "download")
  local_mocked_bindings(download.file = function(...) stop("network forbidden"))
  expect_identical(do.call(download_term_premia, x$args), path)
  value <- do.call(load_term_premia, pin_load_args(x))
  expect_identical(value$date, as.Date("2020-01-31"))
  expect_identical(value$ACMY01, 1.5)
  expect_warning(selected <- do.call(extract_acm_data, c(pin_load_args(x), list(
    data_types = "yields", maturities = 12, frequency = "quarterly"
  ))), class = "hetid_warning_incomplete_quarter")
  expect_identical(selected$y12, 1.5)
  expect_identical(selected$date, as.Date("2020-03-31"))
})

test_that("release, digest, and frequency define separate snapshots", {
  x <- local_acm_pin_fixture()
  paths <- vapply(c("tag", "TAG", "../tag", "tag/part", "tag%2Fpart"), function(tag) {
    do.call(download_term_premia, modifyList(x$args, list(release = tag)))
  }, character(1))
  expect_length(unique(paths), length(paths))
  expect_true(all(startsWith(paths, file.path(get_user_data_dir(), "releases"))))
  daily <- do.call(download_term_premia, c(x$args, list(frequency = "daily")))
  monthly <- do.call(download_term_premia, x$args)
  expect_false(identical(daily, monthly))
  other <- pin_fixture_text(x$root, c("DATE,ACMY01", "2020-01-31,2.5"))
  local_mocked_bindings(download.file = mock_github_download(other))
  path <- do.call(download_term_premia, modifyList(x$args, list(expected_sha256 = other$sha)))
  expect_false(identical(path, monthly))
  expect_identical(unname(tools::sha256sum(monthly)), x$fixture$sha)
  expect_identical(unname(tools::sha256sum(path)), other$sha)
})

test_that("paired pin arguments are validated before network or cache writes", {
  x <- local_acm_pin_fixture()
  local_mocked_bindings(download.file = function(...) stop("network forbidden"))
  bad_tags <- list(
    NULL, NA_character_, "", "\n", " tag", "tag ", "tag part", 1,
    c("a", "b"), matrix("a")
  )
  for (release in bad_tags) {
    expect_error(download_term_premia(release = release, expected_sha256 = x$fixture$sha),
      class = "hetid_error_bad_argument"
    )
  }
  for (sha in list(NULL, NA_character_, "", "bad", 1, rep(x$fixture$sha, 2))) {
    expect_error(load_term_premia(release = "tag", expected_sha256 = sha),
      class = "hetid_error_bad_argument"
    )
  }
  expect_error(do.call(download_term_premia, c(x$args, list(source = "nyfed"))),
    class = "hetid_error_bad_argument"
  )
  expect_error(do.call(load_term_premia, pin_load_args(x)),
    class = "hetid_error_insufficient_data"
  )
  expect_false(dir.exists(file.path(get_user_data_dir(), "releases")))
})

test_that("auto-download and uppercase digests select the requested snapshot", {
  x <- local_acm_pin_fixture()
  args <- modifyList(pin_load_args(x), list(expected_sha256 = toupper(x$fixture$sha)))
  value <- do.call(load_term_premia, c(args, list(auto_download = TRUE)))
  expect_identical(value$ACMY01, 1.5)
  expect_identical(value, do.call(load_term_premia, pin_load_args(x)))
})
