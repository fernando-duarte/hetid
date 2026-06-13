test_that("compute_k2_hat returns a single positive value", {
  test_env <- setup_standard_test_env()

  k2_60 <- compute_k2_hat(test_env$yields, test_env$term_premia, i = 60)

  expect_single_finite_value(k2_60,
    should_be_positive = TRUE,
    label = "k2_hat should be positive (fourth moment of price news)"
  )
})

test_that("k2_hat matches the fourth moment of price news over the bound set", {
  test_env <- setup_standard_test_env()
  step <- HETID_CONSTANTS$DEFAULT_STEP
  i <- 60

  k2 <- compute_k2_hat(test_env$yields, test_env$term_premia, i = i)

  # Manual: mean of (price news)^4 over T_i = {1, ..., T - i/step}
  delta_p <- compute_price_news(test_env$yields, test_env$term_premia, i = i)
  horizon <- i %/% step
  keep <- delta_p[seq_len(length(delta_p) - horizon + 1L)]
  keep <- keep[!is.na(keep)]

  expect_equal(k2, mean(keep^4),
    tolerance = 1e-12,
    label = "k2_hat should equal mean of (price news)^4 over T_i"
  )
})

test_that("k2_hat is positive at the one-period maturity where k1 vanishes", {
  test_env <- setup_standard_test_env()

  # At i = step the realized forecast error is zero (k1 = 0), but the
  # price news is not, so k2 carries the one-period bound.
  k2_12 <- compute_k2_hat(test_env$yields, test_env$term_premia, i = 12)
  k1_12 <- compute_k_hat(test_env$yields, test_env$term_premia, i = 12)

  expect_equal(k1_12, 0, label = "k1 is zero at the one-period maturity")
  expect_gt(k2_12, 0, label = "k2 is strictly positive at the one-period maturity")
})

test_that("compute_k2_hat rejects a maturity that is not a multiple of step", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_k2_hat(test_env$yields, test_env$term_premia, i = 18),
    class = "hetid_error_bad_argument"
  )
})

test_that("compute_k2_hat raises a structured error on a short series", {
  # T = 5 rows but i = 108, step = 12 needs i/step = 9 news periods, so the
  # bound index set is empty: must signal hetid_error_insufficient_data,
  # not a bare seq_len(negative) error.
  syn <- create_synthetic_test_data(n = 5)
  expect_error(
    compute_k2_hat(syn$yields, syn$term_premia, i = 108),
    "Not enough observations",
    class = "hetid_error_insufficient_data"
  )
})

test_that("compute_k2_hat rejects mismatched yields and term_premia rows", {
  syn_long <- create_synthetic_test_data(n = 30)
  syn_short <- create_synthetic_test_data(n = 15)
  expect_error(
    compute_k2_hat(syn_long$yields, syn_short$term_premia, i = 60),
    "same number of observations",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("compute_k2_hat returns typed numeric NA on an all-NA news series", {
  test_env <- setup_standard_test_env()
  yields_na <- test_env$yields

  # All-NA y48 makes n_hat(48) - hence the i = 60 price news - all NA
  yields_na$y48 <- NA_real_

  k2 <- compute_k2_hat(yields_na, test_env$term_premia, i = 60)
  expect_identical(k2, NA_real_)
})
