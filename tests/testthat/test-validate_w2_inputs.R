# Tests for input validation and the bundled-PC fallback branches

test_that("non-contiguous column subsets pass validation", {
  # Maturity 5 only needs columns 4:6 downstream; the ncol of the
  # inputs is not a bound on admissible maturities
  yields <- data.frame(y1 = 1:5, y4 = 1:5, y5 = 1:5, y6 = 1:5)
  term_premia <- data.frame(tp1 = 1:5, tp4 = 1:5, tp5 = 1:5, tp6 = 1:5)

  validated <- validate_w2_inputs(yields, term_premia, maturities = 5)
  expect_equal(validated$maturities, 5)
})

test_that("maturities beyond the effective maximum are rejected", {
  yields <- data.frame(y1 = 1:5, y2 = 1:5)
  term_premia <- data.frame(tp1 = 1:5, tp2 = 1:5)

  expect_error(
    validate_w2_inputs(
      yields, term_premia,
      maturities = effective_max_maturity(HETID_CONSTANTS$DEFAULT_STEP) + 1
    ),
    "must be between",
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
