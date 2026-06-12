# Mocked tests for the digest-verified GitHub download flow: release
# metadata parsing, checksum verification, and the cache move. No real
# network access.

# Build a gz fixture plus matching release-API JSON; the digest can be
# corrupted or the JSON malformed per scenario
make_github_fixture <- function(dir, digest = NULL, json = NULL) {
  gz_path <- file.path(dir, "fixture.csv.gz")
  con <- gzfile(gz_path, "wb")
  writeLines(c("DATE,ACMY01", "2020-01-31,1.5"), con)
  close(con)
  true_sha <- unname(tools::sha256sum(gz_path))
  if (is.null(digest)) {
    digest <- true_sha
  }
  if (is.null(json)) {
    json <- sprintf(
      paste0(
        '{"tag_name":"test-tag","name":"Test Release","assets":[',
        '{"name":"other_asset.xls","digest":"sha256:%s"},',
        '{"name":"%s","digest":"sha256:%s"}]}'
      ),
      strrep("0", 64), HETID_CONSTANTS$ACM_DATA_FILENAME, digest
    )
  }
  list(gz = gz_path, sha = true_sha, json = json)
}

mock_github_download <- function(fixture) {
  function(url, destfile, ...) {
    if (grepl("api.github.com", url, fixed = TRUE)) {
      writeLines(fixture$json, destfile)
    } else {
      file.copy(fixture$gz, destfile, overwrite = TRUE)
    }
    invisible(0L)
  }
}

test_that("github download verifies the digest and caches the file", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  fixture <- make_github_fixture(withr::local_tempdir())

  local_mocked_bindings(
    download.file = mock_github_download(fixture),
    .package = "hetid"
  )

  result <- download_term_premia(force = TRUE, quiet = TRUE)

  expect_identical(result, get_acm_download_path("github"))
  expect_true(file.exists(result))
  expect_identical(unname(tools::sha256sum(result)), fixture$sha)

  # Provenance sidecar records the digest
  meta <- readLines(paste0(result, ".meta"))
  expect_true(any(grepl(fixture$sha, meta, fixed = TRUE)))
})

test_that("a digest mismatch fails closed without caching", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  fixture <- make_github_fixture(
    withr::local_tempdir(),
    digest = strrep("a", 64)
  )

  local_mocked_bindings(
    download.file = mock_github_download(fixture),
    .package = "hetid"
  )

  expect_error(
    download_term_premia(force = TRUE, quiet = TRUE),
    "Checksum mismatch",
    class = "hetid_error"
  )
  expect_false(file.exists(get_acm_download_path("github")))
})

test_that("metadata without the asset name fails closed", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  fixture <- make_github_fixture(
    withr::local_tempdir(),
    json = '{"assets":[{"name":"unrelated.csv","digest":"sha256:00"}]}'
  )

  local_mocked_bindings(
    download.file = mock_github_download(fixture),
    .package = "hetid"
  )

  expect_error(
    download_term_premia(force = TRUE, quiet = TRUE),
    "exactly one asset",
    class = "hetid_error"
  )
})

test_that("metadata with a duplicated asset name fails closed", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  dup_entry <- sprintf(
    '{"name":"%s","digest":"sha256:%s"}',
    HETID_CONSTANTS$ACM_DATA_FILENAME, strrep("b", 64)
  )
  fixture <- make_github_fixture(
    withr::local_tempdir(),
    json = sprintf('{"assets":[%s,%s]}', dup_entry, dup_entry)
  )

  local_mocked_bindings(
    download.file = mock_github_download(fixture),
    .package = "hetid"
  )

  expect_error(
    download_term_premia(force = TRUE, quiet = TRUE),
    "exactly one asset",
    class = "hetid_error"
  )
})

test_that("metadata without a usable digest fails closed", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  fixture <- make_github_fixture(
    withr::local_tempdir(),
    json = sprintf(
      '{"assets":[{"name":"%s","digest":null}]}',
      HETID_CONSTANTS$ACM_DATA_FILENAME
    )
  )

  local_mocked_bindings(
    download.file = mock_github_download(fixture),
    .package = "hetid"
  )

  expect_error(
    download_term_premia(force = TRUE, quiet = TRUE),
    "no usable sha256 digest",
    class = "hetid_error"
  )
})

test_that("force re-download replaces an existing github cache", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)
  fixture <- make_github_fixture(withr::local_tempdir())

  cache_path <- get_acm_download_path("github")
  writeLines("stale", cache_path)

  local_mocked_bindings(
    download.file = mock_github_download(fixture),
    .package = "hetid"
  )

  result <- download_term_premia(force = TRUE, quiet = TRUE)
  expect_identical(result, cache_path)
  expect_identical(unname(tools::sha256sum(cache_path)), fixture$sha)
})
