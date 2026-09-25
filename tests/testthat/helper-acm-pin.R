# Offline fixtures keep all pinned-cache tests outside the real user data directory.
local_acm_pin_fixture <- function(.local_envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = .local_envir)
  withr::local_envvar(c(R_USER_DATA_DIR = root), .local_envir = .local_envir)
  fixture <- make_github_fixture(root)
  testthat::local_mocked_bindings(
    download.file = mock_github_download(fixture), .env = .local_envir
  )
  list(root = root, fixture = fixture, args = list(
    release = "test-release", expected_sha256 = fixture$sha, quiet = TRUE
  ))
}

pin_load_args <- function(x) {
  x$args[setdiff(names(x$args), "quiet")]
}

pin_fixture_text <- function(root, text) {
  path <- tempfile(tmpdir = root, fileext = ".csv.gz")
  con <- gzfile(path, "wb")
  writeLines(text, con)
  close(con)
  list(gz = path, sha = unname(tools::sha256sum(path)), json = "must not fetch metadata")
}
