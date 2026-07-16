test_that("maturity_names builds the maturity_N labels", {
  expect_identical(
    maturity_names(c(24, 48, 72)),
    c("maturity_24", "maturity_48", "maturity_72")
  )
  expect_identical(maturity_names(12L), "maturity_12")
})

test_that("maturity_names uses the configured prefix constant", {
  expect_identical(
    maturity_names(36),
    paste0(HETID_CONSTANTS$MATURITY_PREFIX, 36)
  )
})
