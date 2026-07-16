# Tests for the post-read schema guard: stale or corrupt caches fail
# at load with a structured error naming the offending file

test_that("a frame without a date column fails schema validation", {
  expect_error(
    validate_acm_schema(data.frame(x = 1), "some/path.csv"),
    "no DATE column",
    class = "hetid_error"
  )
})

test_that("a frame without yield columns fails schema validation", {
  expect_error(
    validate_acm_schema(
      data.frame(DATE = "2020-01-31", ACMTP01 = 0.5),
      "some/path.csv"
    ),
    "no ACMY yield columns",
    class = "hetid_error"
  )
})

test_that("non-numeric family columns fail schema validation", {
  expect_error(
    validate_acm_schema(
      data.frame(
        DATE = "2020-01-31", ACMY01 = "corrupted",
        stringsAsFactors = FALSE
      ),
      "some/path.csv"
    ),
    "non-numeric columns: ACMY01",
    class = "hetid_error"
  )
})

test_that("both sources' real shapes pass schema validation", {
  # GitHub-family shape: monthly maturities present
  github_shape <- data.frame(
    DATE = "2020-01-31", ACMY006M = 1.2, ACMY01 = 1.5,
    ACMTP01 = 0.4, ACMRNY01 = 1.1
  )
  expect_true(validate_acm_schema(github_shape, "github.csv.gz"))

  # NY Fed shape: annual nodes only
  nyfed_shape <- data.frame(
    DATE = "2020-01-31", ACMY01 = 1.5, ACMTP01 = 0.4, ACMRNY01 = 1.1
  )
  expect_true(validate_acm_schema(nyfed_shape, "nyfed.csv"))
})

test_that("a corrupt cache fails at load with the schema error", {
  user_root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = user_root)

  # An HTML error page saved where the github cache should be
  cache_path <- get_acm_download_path("github")
  writeLines(
    c("<!DOCTYPE html>", "<html><body>404</body></html>"),
    cache_path
  )

  expect_error(
    load_term_premia(),
    "failed schema validation",
    class = "hetid_error"
  )
})
