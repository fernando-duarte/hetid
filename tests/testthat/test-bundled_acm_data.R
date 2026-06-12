# Guards on the vendored data file: integrity against the release
# digest, the monthly-maturity raw schema, and the mid-quarter tail.

test_that("the bundled gz matches the vendored release digest", {
  bundled <- system.file(
    "extdata", HETID_CONSTANTS$ACM_DATA_FILENAME,
    package = "hetid"
  )
  expect_true(nzchar(bundled))
  expect_identical(
    unname(tools::sha256sum(bundled)),
    "ad6fc66a0c2dba73d32f83a01a8432fc4ee1b7719ed44413351d18c61db34e29"
  )
})

test_that("the bundled data loads with Date dates and monthly maturities", {
  acm <- load_term_premia()

  expect_s3_class(acm$date, "Date")
  expect_identical(min(acm$date), as.Date("1961-06-30"))

  # Annual nodes keep the official names; sub-annual months are new,
  # down to the 3-month floor across all three families
  expect_true(all(paste0("ACMY", sprintf("%02d", 1:10)) %in% names(acm)))
  expect_true(all(c("ACMY003M", "ACMTP003M", "ACMRNY003M") %in% names(acm)))
  expect_true("ACMTP119M" %in% names(acm))
})

test_that("the bundled data's mid-quarter tail raises the classed warning", {
  expect_warning(
    extract_acm_data(frequency = "quarterly"),
    class = "hetid_warning_incomplete_quarter"
  )
})
