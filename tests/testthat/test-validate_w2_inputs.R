# Tests for input validation and the bundled-PC fallback branches

test_that("non-contiguous column subsets pass validation", {
  # Maturity 60 only needs columns 48/60/72 downstream; the ncol of
  # the inputs is not a bound on admissible maturities
  yields <- data.frame(y12 = 1:5, y48 = 1:5, y60 = 1:5, y72 = 1:5)
  term_premia <- data.frame(tp12 = 1:5, tp48 = 1:5, tp60 = 1:5, tp72 = 1:5)

  validated <- validate_w2_inputs(yields, term_premia, maturities = 60)
  expect_equal(validated$maturities, 60)
})

test_that("maturities beyond the effective maximum are rejected", {
  yields <- data.frame(y12 = 1:5, y24 = 1:5)
  term_premia <- data.frame(tp12 = 1:5, tp24 = 1:5)

  expect_error(
    validate_w2_inputs(
      yields, term_premia,
      maturities = effective_max_maturity(HETID_CONSTANTS$DEFAULT_STEP) + 1
    ),
    "must be between",
    class = "hetid_error_bad_argument"
  )
})

test_that("maturities violating the news contract are rejected", {
  # In range but neither the boundary (step) nor step-above MIN:
  # 13 - 12 = 1 falls below MIN_MATURITY
  yields <- data.frame(y12 = 1:5, y24 = 1:5)
  term_premia <- data.frame(tp12 = 1:5, tp24 = 1:5)

  expect_error(
    validate_w2_inputs(yields, term_premia, maturities = 13),
    "must equal step \\(12\\) or satisfy",
    class = "hetid_error_bad_argument"
  )
})
