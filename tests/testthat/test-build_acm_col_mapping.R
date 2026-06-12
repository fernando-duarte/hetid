# Tests for the dual raw-name convention: whole-year months map to the
# official padded-year names, sub-annual months to the 3-digit-month
# form, and package names always carry the month suffix.

test_that("whole-year months map to official padded-year raw names", {
  mapping <- build_acm_col_mapping("yields", c(12, 60, 120))

  expect_equal(mapping[["y12"]], "ACMY01")
  expect_equal(mapping[["y60"]], "ACMY05")
  expect_equal(mapping[["y120"]], "ACMY10")
  expect_length(mapping, 3)
})

test_that("sub-annual months map to three-digit month raw names", {
  mapping <- build_acm_col_mapping("yields", c(3, 6, 18, 119))

  expect_equal(mapping[["y3"]], "ACMY003M")
  expect_equal(mapping[["y6"]], "ACMY006M")
  expect_equal(mapping[["y18"]], "ACMY018M")
  expect_equal(mapping[["y119"]], "ACMY119M")
  expect_length(mapping, 4)
})

test_that("the format boundary sits exactly at whole years", {
  expect_identical(
    unname(acm_raw_column_name("yields", c(11, 12, 13))),
    c("ACMY011M", "ACMY01", "ACMY013M")
  )
})

test_that("term_premia mapping produces correct pairs", {
  mapping <- build_acm_col_mapping("term_premia", c(24, 30, 84))

  expect_equal(mapping[["tp24"]], "ACMTP02")
  expect_equal(mapping[["tp30"]], "ACMTP030M")
  expect_equal(mapping[["tp84"]], "ACMTP07")
  expect_length(mapping, 3)
})

test_that("risk_neutral_yields mapping produces correct pairs", {
  mapping <- build_acm_col_mapping("risk_neutral_yields", 36)

  expect_equal(mapping[["rny36"]], "ACMRNY03")
  expect_length(mapping, 1)
})

test_that("multiple data types produce combined mapping", {
  mapping <- build_acm_col_mapping(
    c("yields", "term_premia"), c(12, 24)
  )

  expect_equal(mapping[["y12"]], "ACMY01")
  expect_equal(mapping[["y24"]], "ACMY02")
  expect_equal(mapping[["tp12"]], "ACMTP01")
  expect_equal(mapping[["tp24"]], "ACMTP02")
  expect_length(mapping, 4)
})

test_that("empty maturities returns empty mapping", {
  mapping <- build_acm_col_mapping("yields", integer(0))

  expect_length(mapping, 0)
})

test_that("unknown data type raises a structured error", {
  expect_error(
    build_acm_col_mapping("nonexistent", c(12, 24)),
    class = "hetid_error_bad_argument"
  )
})
