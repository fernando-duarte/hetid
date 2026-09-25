test_that("failed forced downloads preserve data and provenance", {
  x <- local_acm_pin_fixture()
  path <- do.call(download_term_premia, x$args)
  before <- tools::sha256sum(c(path, paste0(path, ".meta")))
  failures <- list(
    function(...) stop("offline"), function(...) warning("transfer interrupted"),
    function(...) 1L, function(url, destfile, ...) {
      file.create(destfile)
      0L
    },
    function(url, destfile, ...) {
      writeLines("corrupt", destfile)
      0L
    }
  )
  for (fetch in failures) {
    local_mocked_bindings(download.file = fetch)
    expect_error(do.call(download_term_premia, c(x$args, list(force = TRUE))),
      class = "hetid_error"
    )
    expect_identical(tools::sha256sum(c(path, paste0(path, ".meta"))), before)
  }
  local_mocked_bindings(download.file = mock_github_download(x$fixture))
  expect_identical(do.call(download_term_premia, c(x$args, list(force = TRUE))), path)
  expect_identical(tools::sha256sum(c(path, paste0(path, ".meta"))), before)
  expect_length(list.files(dirname(dirname(path)), pattern = "^acm_stage_"), 0L)
})

test_that("matching digests do not bypass schema or date checks", {
  x <- local_acm_pin_fixture()
  invalid <- list(
    c("DATE,wrong", "2020-01-31,1"), c("DATE,ACMY01", "2020-01-31,bad"),
    c("DATE,ACMY01", "bad,1"), c("DATE,ACMY01", "2020-01-31,1", "bad,2"),
    c("DATE,ACMY01", ",1"), "DATE,ACMY01"
  )
  for (content in invalid) {
    fixture <- pin_fixture_text(x$root, content)
    local_mocked_bindings(download.file = mock_github_download(fixture))
    args <- modifyList(x$args, list(expected_sha256 = fixture$sha))
    pin <- acm_pin(args$release, args$expected_sha256, "github", "monthly")
    expect_error(do.call(download_term_premia, args), class = "hetid_error")
    expect_false(dir.exists(dirname(pin$path)))
  }
})

test_that("pinned reads reject altered bytes and incomplete or wrong provenance", {
  x <- local_acm_pin_fixture()
  path <- do.call(download_term_premia, x$args)
  sidecar <- paste0(path, ".meta")
  original <- readLines(sidecar)
  for (text in list("release: wrong", sub("test-release", "wrong", original))) {
    writeLines(text, sidecar)
    expect_error(do.call(load_term_premia, pin_load_args(x)), class = "hetid_error")
  }
  unlink(sidecar)
  expect_error(do.call(load_term_premia, pin_load_args(x)), class = "hetid_error")
  writeLines(original, sidecar)
  writeLines("corrupt", path)
  expect_error(do.call(load_term_premia, pin_load_args(x)), class = "hetid_error")
  expect_error(do.call(download_term_premia, c(x$args, list(force = TRUE))),
    class = "hetid_error"
  )
})

test_that("failed publication and provenance writes leave no partial snapshot", {
  x <- local_acm_pin_fixture()
  pin <- acm_pin(x$args$release, x$fixture$sha, "github", "monthly")
  rename <- base::file.rename
  local_mocked_bindings(file.rename = function(...) FALSE, .package = "base")
  expect_error(do.call(download_term_premia, x$args), class = "hetid_error")
  expect_false(dir.exists(dirname(pin$path)))
  local_mocked_bindings(
    file.rename = rename,
    write.dcf = function(...) stop("disk full"), .package = "base"
  )
  expect_error(do.call(download_term_premia, x$args), class = "hetid_error")
  expect_false(dir.exists(dirname(pin$path)))
  expect_length(list.files(dirname(dirname(pin$path)), pattern = "^acm_stage_"), 0L)
})


test_that("provenance write warnings and invalid sidecars never publish", {
  x <- local_acm_pin_fixture()
  pin <- acm_pin(x$args$release, x$fixture$sha, "github", "monthly")
  local_mocked_bindings(write.dcf = function(...) warning("short write"), .package = "base")
  expect_error(do.call(download_term_premia, x$args), class = "hetid_error")
  expect_false(dir.exists(dirname(pin$path)))
  local_mocked_bindings(write.dcf = function(x, file, ...) {
    writeLines("release: wrong", file)
    invisible(NULL)
  }, .package = "base")
  expect_error(do.call(download_term_premia, x$args), class = "hetid_error")
  expect_false(dir.exists(dirname(pin$path)))
})

test_that("a changed provenance URL does not change snapshot identity", {
  x <- local_acm_pin_fixture()
  path <- do.call(download_term_premia, x$args)
  meta <- read.dcf(paste0(path, ".meta"))
  meta[1, "source_url"] <- "https://example.org/previous-location"
  write.dcf(meta, paste0(path, ".meta"))
  expect_identical(do.call(load_term_premia, pin_load_args(x))$ACMY01, 1.5)
})
