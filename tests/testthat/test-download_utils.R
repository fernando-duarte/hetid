# atomic_replace internals, hermetic (no network, tempdir-only); covers the
# fail-closed rename branch the mocked download tests do not reach

test_that("atomic_replace errors when the rename fails", {
  # A nonexistent source makes file.rename() return FALSE deterministically
  # on every platform, exercising the stop_hetid branch without any network
  # or fixture.
  missing_src <- file.path(tempdir(), "hetid-atomic-replace-absent.tmp")
  unlink(missing_src)
  dest <- tempfile()
  expect_error(
    suppressWarnings(atomic_replace(missing_src, dest, "test artifact")),
    class = "hetid_error"
  )
})
