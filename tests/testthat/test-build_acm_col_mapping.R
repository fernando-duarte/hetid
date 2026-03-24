test_that("yields mapping produces correct old/new column pairs", {
  mapping <- build_acm_col_mapping("yields", c(1, 5, 10))

  expect_equal(mapping[["y1"]], "ACMY01")
  expect_equal(mapping[["y5"]], "ACMY05")
  expect_equal(mapping[["y10"]], "ACMY10")
  expect_length(mapping, 3)
})

test_that("term_premia mapping produces correct pairs", {
  mapping <- build_acm_col_mapping("term_premia", c(2, 7))

  expect_equal(mapping[["tp2"]], "ACMTP02")
  expect_equal(mapping[["tp7"]], "ACMTP07")
  expect_length(mapping, 2)
})

test_that("risk_neutral_yields mapping produces correct pairs", {
  mapping <- build_acm_col_mapping("risk_neutral_yields", c(3))

  expect_equal(mapping[["rny3"]], "ACMRNY03")
  expect_length(mapping, 1)
})

test_that("multiple data types produce combined mapping", {
  mapping <- build_acm_col_mapping(
    c("yields", "term_premia"), c(1, 2)
  )

  expect_equal(mapping[["y1"]], "ACMY01")
  expect_equal(mapping[["y2"]], "ACMY02")
  expect_equal(mapping[["tp1"]], "ACMTP01")
  expect_equal(mapping[["tp2"]], "ACMTP02")
  expect_length(mapping, 4)
})

test_that("empty maturities returns empty mapping", {
  mapping <- build_acm_col_mapping("yields", integer(0))

  expect_length(mapping, 0)
})

test_that("unknown data type is silently skipped", {
  mapping <- build_acm_col_mapping("nonexistent", c(1, 2))

  expect_length(mapping, 0)
})
