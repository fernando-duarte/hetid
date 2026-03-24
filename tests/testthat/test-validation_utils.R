test_that("validate_maturity_index rejects non-integer values", {
  expect_error(validate_maturity_index(1.5), "integer")
  expect_error(validate_maturity_index(2.7), "integer")
})

test_that("validate_maturity_index rejects out-of-range values", {
  expect_error(validate_maturity_index(0), "between")
  expect_error(validate_maturity_index(11), "between")
  expect_error(validate_maturity_index(-1), "between")
})

test_that("validate_maturity_index rejects invalid types", {
  expect_error(
    validate_maturity_index("a"), "single finite numeric"
  )
  expect_error(
    validate_maturity_index(NA), "single finite numeric"
  )
  expect_error(
    validate_maturity_index(Inf), "single finite numeric"
  )
  expect_error(
    validate_maturity_index(c(1, 2)), "single finite numeric"
  )
  expect_error(
    validate_maturity_index(NULL), "single finite numeric"
  )
})

test_that("validate_maturity_index respects custom max_maturity", {
  expect_error(
    validate_maturity_index(10, max_maturity = 9), "between"
  )
  expect_true(validate_maturity_index(9, max_maturity = 9))
})

test_that("validate_maturity_index accepts valid inputs", {
  expect_true(validate_maturity_index(1))
  expect_true(validate_maturity_index(10))
  expect_true(validate_maturity_index(5L))
  expect_true(validate_maturity_index(5.0))
})

# --- validate_min_observations ---

test_that("validate_min_observations accepts sufficient counts", {
  expect_true(validate_min_observations(3))
  expect_true(validate_min_observations(100))
  expect_true(validate_min_observations(3L))
})

test_that("validate_min_observations rejects too few observations", {
  expect_error(
    validate_min_observations(2),
    "Not enough complete observations"
  )
  expect_error(
    validate_min_observations(0),
    "need at least 3"
  )
  expect_error(
    validate_min_observations(1),
    "need at least 3"
  )
})

test_that("validate_min_observations respects custom minimum", {
  expect_true(validate_min_observations(10, min_obs = 10))
  expect_error(
    validate_min_observations(9, min_obs = 10),
    "need at least 10"
  )
})

test_that("validate_min_observations rejects non-numeric inputs", {
  expect_error(
    validate_min_observations("a"),
    "single finite numeric"
  )
  expect_error(
    validate_min_observations(NA),
    "single finite numeric"
  )
  expect_error(
    validate_min_observations(Inf),
    "single finite numeric"
  )
  expect_error(
    validate_min_observations(c(5, 10)),
    "single finite numeric"
  )
  expect_error(
    validate_min_observations(NULL),
    "single finite numeric"
  )
})

test_that("validate_min_observations boundary at default minimum", {
  min_obs <- HETID_CONSTANTS$MIN_OBSERVATIONS
  expect_true(validate_min_observations(min_obs))
  expect_error(
    validate_min_observations(min_obs - 1),
    "Not enough complete observations"
  )
})

# --- validate_n_pcs ---

test_that("validate_n_pcs accepts valid values", {
  expect_true(validate_n_pcs(1))
  expect_true(validate_n_pcs(4))
  expect_true(validate_n_pcs(6))
  expect_true(validate_n_pcs(3L))
})

test_that("validate_n_pcs rejects out-of-range values", {
  expect_error(
    validate_n_pcs(0),
    "n_pcs must be between 1 and"
  )
  expect_error(
    validate_n_pcs(-1),
    "n_pcs must be between 1 and"
  )
  expect_error(
    validate_n_pcs(7),
    "n_pcs must be between 1 and 6"
  )
})

test_that("validate_n_pcs rejects non-numeric inputs", {
  expect_error(
    validate_n_pcs("a"),
    "single finite numeric"
  )
  expect_error(
    validate_n_pcs(NA),
    "single finite numeric"
  )
  expect_error(
    validate_n_pcs(Inf),
    "single finite numeric"
  )
  expect_error(
    validate_n_pcs(c(1, 2)),
    "single finite numeric"
  )
  expect_error(
    validate_n_pcs(NULL),
    "single finite numeric"
  )
})

test_that("validate_n_pcs boundary at max", {
  max_pcs <- HETID_CONSTANTS$MAX_N_PCS
  expect_true(validate_n_pcs(max_pcs))
  expect_error(
    validate_n_pcs(max_pcs + 1),
    "n_pcs must be between"
  )
})

# --- validate_numeric_inputs ---

test_that("validate_numeric_inputs accepts all numeric", {
  expect_true(
    validate_numeric_inputs(x = c(1, 2, 3), y = c(4, 5, 6))
  )
  expect_true(
    validate_numeric_inputs(a = 1L, b = 2.5)
  )
  expect_true(
    validate_numeric_inputs(
      m = matrix(1:4, 2, 2)
    )
  )
})

test_that("validate_numeric_inputs rejects non-numeric named", {
  expect_error(
    validate_numeric_inputs(x = "text"),
    "x must be a numeric vector"
  )
  expect_error(
    validate_numeric_inputs(
      good = c(1, 2), bad = "text"
    ),
    "bad must be a numeric vector"
  )
})

test_that("validate_numeric_inputs rejects non-numeric unnamed", {
  expect_error(
    validate_numeric_inputs("text"),
    "input_1 must be a numeric vector"
  )
  expect_error(
    validate_numeric_inputs(c(1, 2), "text"),
    "input_2 must be a numeric vector"
  )
})

test_that("validate_numeric_inputs rejects logical inputs", {
  expect_error(
    validate_numeric_inputs(flag = TRUE),
    "flag must be a numeric vector"
  )
})

test_that("validate_numeric_inputs handles single input", {
  expect_true(validate_numeric_inputs(val = 42))
  expect_error(
    validate_numeric_inputs(val = "no"),
    "val must be a numeric vector"
  )
})

# --- validate_time_series_lengths ---

test_that("validate_time_series_lengths accepts matching lengths", {
  expect_true(
    validate_time_series_lengths(
      c(1, 2, 3), c(4, 5, 6)
    )
  )
  expect_true(
    validate_time_series_lengths(
      c(1, 2), c(3, 4), c(5, 6)
    )
  )
})

test_that("validate_time_series_lengths rejects mismatched", {
  expect_error(
    validate_time_series_lengths(
      c(1, 2, 3), c(4, 5)
    ),
    "same length"
  )
  expect_error(
    validate_time_series_lengths(
      c(1, 2, 3), c(4, 5)
    ),
    "Got lengths: 3, 2"
  )
})

test_that("validate_time_series_lengths rejects fewer than two", {
  expect_error(
    validate_time_series_lengths(c(1, 2, 3)),
    "At least two time series required"
  )
  expect_error(
    validate_time_series_lengths(),
    "At least two time series required"
  )
})

test_that("validate_time_series_lengths reports all lengths", {
  expect_error(
    validate_time_series_lengths(
      c(1, 2), c(3, 4, 5), c(6, 7, 8, 9)
    ),
    "Got lengths: 2, 3, 4"
  )
})

test_that("validate_time_series_lengths with empty vectors", {
  expect_true(
    validate_time_series_lengths(
      integer(0), integer(0)
    )
  )
  expect_error(
    validate_time_series_lengths(
      integer(0), c(1, 2)
    ),
    "same length"
  )
})

# --- validate_data_dimensions ---

test_that("validate_data_dimensions accepts matching dims", {
  syn <- create_synthetic_test_data(n = 50, n_maturities = 5)
  expect_true(
    validate_data_dimensions(syn$yields, syn$term_premia)
  )
})

test_that("validate_data_dimensions rejects row mismatch", {
  y <- matrix(1:20, nrow = 10, ncol = 2)
  tp <- matrix(1:14, nrow = 7, ncol = 2)
  expect_error(
    validate_data_dimensions(y, tp),
    "same number of observations"
  )
  expect_error(
    validate_data_dimensions(y, tp),
    "10 vs 7 rows"
  )
})

test_that("validate_data_dimensions rejects column mismatch", {
  y <- matrix(1:30, nrow = 10, ncol = 3)
  tp <- matrix(1:20, nrow = 10, ncol = 2)
  expect_error(
    validate_data_dimensions(y, tp),
    "same number of maturities"
  )
  expect_error(
    validate_data_dimensions(y, tp),
    "3 vs 2 columns"
  )
})

test_that("validate_data_dimensions works with data frames", {
  y <- data.frame(a = 1:5, b = 6:10)
  tp <- data.frame(c = 11:15, d = 16:20)
  expect_true(validate_data_dimensions(y, tp))
})

test_that("validate_data_dimensions with single column", {
  y <- matrix(1:5, ncol = 1)
  tp <- matrix(6:10, ncol = 1)
  expect_true(validate_data_dimensions(y, tp))
})

test_that("validate_data_dimensions checks rows before cols", {
  y <- matrix(1:12, nrow = 4, ncol = 3)
  tp <- matrix(1:10, nrow = 5, ncol = 2)
  expect_error(
    validate_data_dimensions(y, tp),
    "same number of observations"
  )
})
