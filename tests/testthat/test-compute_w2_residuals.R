test_that("compute_w2_residuals works for single maturity", {
  eval(parse("bodies/compute_w2_residuals-contracts.R", encoding = "UTF-8")[[1]], environment())
})

test_that("compute_w2_residuals works for maturity 12", {
  eval(parse("bodies/compute_w2_residuals-contracts.R", encoding = "UTF-8")[[2]], environment())
})

test_that("compute_w2_residuals works for multiple maturities", {
  test_env <- setup_standard_test_env()

  set.seed(123)
  pcs <- matrix(stats::rnorm(nrow(test_env$yields) * 4), nrow = nrow(test_env$yields))

  maturities <- c(12, 24, 60, 84)
  res_y2 <- suppressWarnings(compute_w2_residuals(test_env$yields, test_env$term_premia,
    maturities = maturities, n_pcs = 4, pcs = pcs, dates = test_env$data$date
  ))

  expect_length(res_y2$residuals, length(maturities))
  expect_length(res_y2$r_squared, length(maturities))
  expect_equal(nrow(res_y2$coefficients), length(maturities))
})

test_that("residual properties check", {
  test_env <- setup_standard_test_env()
  set.seed(123)
  pcs <- matrix(stats::rnorm(nrow(test_env$yields) * 4), nrow = nrow(test_env$yields))

  res_y2 <- suppressWarnings(compute_w2_residuals(test_env$yields, test_env$term_premia,
    maturities = 36, n_pcs = 4, pcs = pcs, dates = test_env$data$date
  ))
  residuals <- res_y2$residuals[[1]]

  expect_lt(abs(mean(residuals, na.rm = TRUE)), 1e-10)

  expect_true(all(is.finite(residuals) | is.na(residuals)))
})

test_that("compute_w2_residuals uses SDF innovations", {
  eval(parse("bodies/compute_w2_residuals-contracts.R", encoding = "UTF-8")[[3]], environment())
})

test_that("R-squared matches manual regression", {
  eval(parse("bodies/compute_w2_residuals-contracts.R", encoding = "UTF-8")[[4]], environment())
})

test_that("quarterly data alignment test", {
  eval(parse("bodies/compute_w2_residuals-contracts.R", encoding = "UTF-8")[[5]], environment())
})

test_that("length verification for output", {
  eval(parse("bodies/compute_w2_residuals-contracts.R", encoding = "UTF-8")[[6]], environment())
})

test_that("no message when user provides PCs", {
  eval(parse("bodies/compute_w2_residuals-inputs.R", encoding = "UTF-8")[[1]], environment())
})

test_that("return_df date column is the real t+1 Date when user provides PCs", {
  eval(parse("bodies/compute_w2_residuals-inputs.R", encoding = "UTF-8")[[2]], environment())
})

test_that("no message for dates when user provides dates", {
  eval(parse("bodies/compute_w2_residuals-inputs.R", encoding = "UTF-8")[[3]], environment())
})

test_that("return_df dates align correctly with interior NA in PCs", {
  eval(parse("bodies/compute_w2_residuals-inputs.R", encoding = "UTF-8")[[4]], environment())
})

test_that("error when user pcs has wrong number of rows", {
  test_env <- setup_standard_test_env()

  # PCs with 5 rows but yields has many more
  bad_pcs <- matrix(rnorm(5 * 4), ncol = 4)

  expect_error(
    compute_w2_residuals(
      test_env$yields, test_env$term_premia,
      maturities = 60, n_pcs = 4, pcs = bad_pcs,
      dates = test_env$data$date
    ),
    "must match number of rows"
  )
})

test_that("error when pcs has fewer columns than n_pcs", {
  eval(parse("bodies/compute_w2_residuals-inputs.R", encoding = "UTF-8")[[5]], environment())
})

test_that("error when n_pcs is invalid", {
  eval(parse("bodies/compute_w2_residuals-inputs.R", encoding = "UTF-8")[[6]], environment())
})

test_that("error when maturities are non-integer or negative", {
  eval(parse("bodies/compute_w2_residuals-inputs.R", encoding = "UTF-8")[[7]], environment())
})

# Synthetic inputs for the warn-and-skip contract tests: yields/term_premia
# restricted to the requested column indices, with the required PCs supplied
make_w2_skip_inputs <- function(col_indices, n = 40, seed = 123) {
  set.seed(seed)
  yields <- as.data.frame(
    matrix(rnorm(n * length(col_indices), mean = 2), nrow = n)
  )
  names(yields) <- paste0("y", col_indices)
  term_premia <- as.data.frame(
    matrix(rnorm(n * length(col_indices), mean = 0.5), nrow = n)
  )
  names(term_premia) <- paste0("tp", col_indices)
  list(
    yields = yields,
    term_premia = term_premia,
    pcs = matrix(rnorm(n * 2), ncol = 2),
    dates = seq(as.Date("1990-03-31"), by = "quarter", length.out = n)
  )
}

test_that("maturity equal to the highest available column skips while others succeed", {
  inputs <- make_w2_skip_inputs(c(12, 24))

  expect_warning(
    result <- compute_w2_residuals(
      inputs$yields, inputs$term_premia,
      maturities = c(12, 24), n_pcs = 2, pcs = inputs$pcs,
      dates = inputs$dates
    ),
    "[Ss]kipping maturity 24"
  )
  expect_named(result$residuals, "maturity_12")
  expect_true(is.finite(result$r_squared[1]))
  expect_true(is.na(result$r_squared[2]))
})

test_that("one bad maturity does not abort valid maturities", {
  eval(parse("bodies/compute_w2_residuals-inputs.R", encoding = "UTF-8")[[8]], environment())
})

test_that("non-contiguous column subsets pass validation and compute", {
  # Maturity 60 needs only columns 48/60/72; y12/tp12 are extra
  inputs <- make_w2_skip_inputs(c(12, 48, 60, 72))

  result <- compute_w2_residuals(
    inputs$yields, inputs$term_premia,
    maturities = 60, n_pcs = 2, pcs = inputs$pcs,
    dates = inputs$dates
  )
  expect_named(result$residuals, "maturity_60")
  expect_true(is.finite(result$r_squared[1]))
  expect_true(result$n_obs[1] > 0)
})

test_that("skipped maturities report NA, not zero", {
  eval(parse("bodies/compute_w2_residuals-validation.R", encoding = "UTF-8")[[1]], environment())
})

test_that("all maturities skipped with return_df gives zero-row data frame", {
  inputs <- make_w2_skip_inputs(c(12, 24))

  expect_warning(
    result <- compute_w2_residuals(
      inputs$yields, inputs$term_premia,
      maturities = 36, n_pcs = 2, pcs = inputs$pcs,
      return_df = TRUE, dates = inputs$dates
    ),
    "[Ss]kipping maturity 36"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, c("date", "maturity", "residuals", "fitted"))
  expect_named(attr(result, "skipped_maturities"), "maturity_36")
})

test_that("list mode returns per-maturity dates parallel to residuals", {
  eval(parse("bodies/compute_w2_residuals-validation.R", encoding = "UTF-8")[[2]], environment())
})

test_that("list-mode dates are ragged when maturities drop different rows", {
  eval(parse("bodies/compute_w2_residuals-validation.R", encoding = "UTF-8")[[3]], environment())
})

test_that("list-mode dates are real Dates per maturity for custom PCs", {
  eval(parse("bodies/compute_w2_residuals-validation.R", encoding = "UTF-8")[[4]], environment())
})

test_that("list-mode and data-frame-mode dates agree", {
  eval(parse("bodies/compute_w2_residuals-validation.R", encoding = "UTF-8")[[5]], environment())
})
