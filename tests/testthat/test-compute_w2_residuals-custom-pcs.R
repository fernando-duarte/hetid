test_that("custom instrument matrices may exceed the bundled PC cap", {
  test_data <- create_synthetic_test_data(n = 60, n_maturities = 4)
  wide <- matrix(
    rnorm(60 * 8),
    nrow = 60, dimnames = list(NULL, paste0("f", 1:8))
  )
  res <- compute_w2_residuals(
    test_data$yields, test_data$term_premia,
    maturities = c(24, 36), n_pcs = 8, pcs = wide
  )
  expect_identical(
    colnames(res$coefficients),
    c("(Intercept)", paste0("f", 1:8))
  )
})

test_that("non-numeric custom pcs are rejected", {
  test_data <- create_synthetic_test_data(n = 40, n_maturities = 3)
  bad <- data.frame(a = rnorm(40), b = letters[rep(1:8, 5)])
  expect_error(
    compute_w2_residuals(
      test_data$yields, test_data$term_premia,
      maturities = 24, n_pcs = 2, pcs = bad
    ),
    class = "hetid_error_bad_argument"
  )
})

test_that("n_pcs above the custom matrix width is rejected", {
  test_data <- create_synthetic_test_data(n = 60, n_maturities = 4)
  narrow <- matrix(rnorm(60 * 2), nrow = 60)
  expect_error(
    compute_w2_residuals(
      test_data$yields, test_data$term_premia,
      maturities = 24, n_pcs = 5, pcs = narrow
    ),
    class = "hetid_error"
  )
})
