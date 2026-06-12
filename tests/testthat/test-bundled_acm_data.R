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
    "d95255a08200aadbf66dd33bdd54111e9f58bfab987a5683e93ec81c6b087e89"
  )
})

test_that("the bundled data loads with Date dates and monthly maturities", {
  acm <- load_term_premia()

  expect_s3_class(acm$date, "Date")
  expect_identical(min(acm$date), as.Date("1961-06-30"))

  # Annual nodes keep the official names; sub-annual months are new
  expect_true(all(paste0("ACMY", sprintf("%02d", 1:10)) %in% names(acm)))
  expect_true("ACMY006M" %in% names(acm))
  expect_true("ACMTP119M" %in% names(acm))
})

test_that("the bundled data's mid-quarter tail raises the classed warning", {
  expect_warning(
    extract_acm_data(frequency = "quarterly"),
    class = "hetid_warning_incomplete_quarter"
  )
})
