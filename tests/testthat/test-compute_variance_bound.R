test_that("compute_variance_bound returns single positive value", {
  test_env <- setup_standard_test_env()

  var_bound_60 <- compute_variance_bound(test_env$yields, test_env$term_premia, i = 60)

  expect_type(var_bound_60, "double")
  expect_length(var_bound_60, 1)
  expect_true(is.finite(var_bound_60))
  expect_gt(var_bound_60, 0, label = "Variance bound should be positive")
})

test_that("one-period bound (i=12) is the strictly positive k2 term", {
  test_env <- setup_standard_test_env()

  # At the one-period maturity i = step the k_hat (k1) term is 0, but the
  # bound is the strictly positive k2_hat contribution (spec U_step)
  var_bound_12 <- compute_variance_bound(test_env$yields, test_env$term_premia, i = 12)
  c_hat_12 <- compute_c_hat(test_env$yields, test_env$term_premia, i = 12)
  k_hat_12 <- compute_k_hat(test_env$yields, test_env$term_premia, i = 12)
  k2_hat_12 <- compute_k2_hat(test_env$yields, test_env$term_premia, i = 12)

  expect_equal(k_hat_12, 0, label = "k1 is zero at the one-period maturity")
  expect_gt(var_bound_12, 0, label = "one-period bound is positive via k2")
  expect_equal(var_bound_12, 0.25 * c_hat_12 * k2_hat_12, tolerance = 1e-12)
})

test_that("variance bound is positive across the annual nodes", {
  test_env <- setup_standard_test_env()

  # Every node, including the one-period boundary i = 12, has a strictly
  # positive bound (the boundary is carried entirely by the k2 term)
  for (i in seq(12, 108, by = 12)) {
    var_bound_i <- compute_variance_bound(test_env$yields, test_env$term_premia, i = i)
    expect_gt(var_bound_i, 0,
      label = paste("Variance bound should be positive for i =", i)
    )
  }
})

test_that("variance bound formula verification", {
  test_env <- setup_standard_test_env()

  i <- 48
  var_bound_48 <- compute_variance_bound(test_env$yields, test_env$term_premia, i = i)

  c_hat_48 <- compute_c_hat(test_env$yields, test_env$term_premia, i = i)
  k_hat_48 <- compute_k_hat(test_env$yields, test_env$term_premia, i = i)
  k2_hat_48 <- compute_k2_hat(test_env$yields, test_env$term_premia, i = i)

  expected_bound <- 0.25 * c_hat_48 * (k_hat_48 + k2_hat_48)

  expect_equal(var_bound_48, expected_bound,
    tolerance = 1e-10,
    label = "Variance bound should equal 0.25 * c_hat * (k_hat + k2_hat)"
  )
})

test_that("variance bound generally increases with maturity", {
  test_env <- setup_standard_test_env()

  maturities <- seq(12, 108, by = 12)
  var_bounds <- numeric(length(maturities))
  for (k in seq_along(maturities)) {
    var_bounds[k] <- compute_variance_bound(
      test_env$yields, test_env$term_premia,
      i = maturities[k]
    )
  }

  # Check general increasing trend (allowing for some non-monotonicity)
  increases <- sum(diff(var_bounds[2:9]) > 0) # Compare the non-boundary nodes
  expect_gte(increases, 4,
    label = "Variance bound should generally increase with maturity"
  )
})

test_that("compute_variance_bound rejects mismatched yields and term_premia rows", {
  syn_long <- create_synthetic_test_data(n = 30)
  syn_short <- create_synthetic_test_data(n = 15)
  expect_error(
    compute_variance_bound(syn_long$yields, syn_short$term_premia, i = 60),
    "same number of observations",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("compute_variance_bound rejects invalid maturity values", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_variance_bound(test_env$yields, test_env$term_premia, i = 1.5),
    "integer"
  )
  expect_error(
    compute_variance_bound(test_env$yields, test_env$term_premia, i = 120),
    "between"
  )
})

test_that("compute_variance_bound returns typed numeric NA on a degenerate component", {
  test_env <- setup_standard_test_env()
  yields_na <- test_env$yields

  # All-NA y60 leaves c_hat without a valid paired observation at both nodes,
  # so the assembled bound must stay NA rather than report a number
  yields_na$y60 <- NA_real_

  # vapply(..., numeric(1)) enforces a double return on the NA branch
  vb_vals <- vapply(
    c(48, 60),
    function(i) compute_variance_bound(yields_na, test_env$term_premia, i = i),
    numeric(1)
  )

  expect_type(vb_vals, "double")
  expect_true(all(is.na(vb_vals)))
  expect_identical(
    compute_variance_bound(yields_na, test_env$term_premia, i = 60),
    NA_real_
  )
})
