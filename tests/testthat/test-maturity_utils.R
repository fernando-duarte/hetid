# Tests for maturity naming helpers

test_that("maturity_names builds the maturity_N labels", {
  expect_identical(
    maturity_names(c(2, 4, 6)),
    c("maturity_2", "maturity_4", "maturity_6")
  )
  expect_identical(maturity_names(1L), "maturity_1")
})

test_that("maturity_names uses the configured prefix constant", {
  expect_identical(
    maturity_names(3),
    paste0(HETID_CONSTANTS$MATURITY_PREFIX, 3)
  )
})
