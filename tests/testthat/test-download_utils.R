# atomic_replace internals, hermetic (no network, tempdir-only); covers the
# fail-closed rename branch the mocked download tests do not reach

test_that("atomic_replace errors when the rename fails", {
  # A nonexistent source makes file.rename() return FALSE on every platform,
  # exercising the stop_hetid branch without network or fixture
  missing_src <- file.path(tempdir(), "hetid-atomic-replace-absent.tmp")
  unlink(missing_src)
  dest <- tempfile()
  expect_error(
    suppressWarnings(atomic_replace(missing_src, dest, "test artifact")),
    class = "hetid_error"
  )
})


test_that("failed replacement preserves the previous cache", {
  dest <- tempfile()
  on.exit(unlink(dest))
  writeLines("valid existing cache", dest)
  before <- tools::sha256sum(dest)
  expect_error(
    suppressWarnings(atomic_replace(tempfile(), dest, "missing source")),
    class = "hetid_error"
  )
  expect_identical(tools::sha256sum(dest), before)
})

test_that("successful replacement installs all new bytes", {
  root <- withr::local_tempdir()
  dest <- file.path(root, "cache")
  temp <- file.path(root, "stage")
  writeLines("old", dest)
  writeLines("new", temp)
  expect_identical(atomic_replace(temp, dest, "new cache"), dest)
  expect_identical(readLines(dest), "new")
  expect_false(file.exists(temp))
})
