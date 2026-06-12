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

test_that("bundled fallback errors when PC columns are missing", {
  data("variables", package = "hetid", envir = environment())
  only_two_pcs <- variables[, setdiff(names(variables), c("pc3", "pc4", "pc5", "pc6"))]
  local_mocked_bindings(get_bundled_variables = function() only_two_pcs)

  expect_error(
    expect_warning(
      load_w2_pcs(NULL, n_pcs = 4, n_obs = 100),
      "bundled"
    ),
    "Missing PC columns",
    class = "hetid_error_bad_argument"
  )
})

test_that("bundled fallback returns NULL dates without a date column", {
  data("variables", package = "hetid", envir = environment())
  no_date <- variables[, setdiff(names(variables), "date")]
  local_mocked_bindings(get_bundled_variables = function() no_date)

  expect_warning(
    result <- load_w2_pcs(NULL, n_pcs = 2, n_obs = 50),
    "bundled"
  )
  expect_null(result$dates)
  expect_equal(ncol(result$pcs), 2)
})
