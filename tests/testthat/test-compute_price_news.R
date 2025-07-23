# Test file for compute_price_news function
# Tests unexpected bond price changes calculations

test_that("compute_price_news returns expected structure", {
  # Load test data
  data <- extract_acm_data()

  # Test with default parameters
  result <- compute_price_news(data, data, i = 5)

  # Check output structure
  expect_type(result, "double")
  expect_true(is.vector(result))

  # Length should be one less than input (due to differencing)
  expect_equal(length(result), nrow(data) - 1)

  # Should contain mostly finite values
  expect_true(sum(is.finite(result)) > length(result) * 0.9)
})

test_that("compute_price_news manual calculation verification", {
  # Create simple test data
  set.seed(321)
  n_obs <- 50

  # Create data frame with increasing yields
  test_data <- data.frame(
    y1 = seq(2, 4, length.out = n_obs) + rnorm(n_obs, 0, 0.01),
    y2 = seq(2.2, 4.2, length.out = n_obs) + rnorm(n_obs, 0, 0.01),
    y3 = seq(2.4, 4.4, length.out = n_obs) + rnorm(n_obs, 0, 0.01),
    tp1 = rep(1, n_obs),
    tp2 = rep(1.2, n_obs),
    tp3 = rep(1.4, n_obs)
  )

  # Compute price news
  price_news <- compute_price_news(test_data, test_data, i = 2)

  # With increasing yields, bond prices fall, so price news should be mostly negative
  expect_true(mean(price_news, na.rm = TRUE) < 0)

  # Check reasonable magnitude
  expect_true(all(abs(price_news[is.finite(price_news)]) < 1))
})

test_that("compute_price_news properties across maturities", {
  # Load test data
  data <- extract_acm_data()

  # Compute for different maturities
  pn_short <- compute_price_news(data, data, i = 2)
  pn_long <- compute_price_news(data, data, i = 9)

  # Both should have similar length
  expect_equal(length(pn_short), length(pn_long))
})

test_that("compute_price_news zero news for constant data", {
  # Create perfectly constant data
  n_obs <- 100
  const_data <- data.frame(
    y1 = rep(3, n_obs),
    y2 = rep(3.2, n_obs),
    y3 = rep(3.4, n_obs),
    y4 = rep(3.6, n_obs),
    y5 = rep(3.8, n_obs),
    tp1 = rep(1, n_obs),
    tp2 = rep(1.2, n_obs),
    tp3 = rep(1.4, n_obs),
    tp4 = rep(1.6, n_obs),
    tp5 = rep(1.8, n_obs)
  )

  # Price news should be essentially zero
  price_news <- compute_price_news(const_data, const_data, i = 3)

  # All values should be very close to zero (allowing for numerical precision)
  finite_pn <- price_news[is.finite(price_news)]
  expect_true(all(abs(finite_pn) < 1e-10))
})

test_that("compute_price_news handles missing data", {
  # Load test data
  data <- extract_acm_data()

  # Create copy with missing data
  data_na <- data
  data_na[25:35, c("y4", "tp4")] <- NA

  # Should still compute
  result <- compute_price_news(data_na, data_na, i = 4)

  expect_type(result, "double")
  expect_equal(length(result), nrow(data) - 1)

  # Should have NAs in appropriate places
  expect_true(any(is.na(result)))

  # But should also have valid values
  expect_true(sum(is.finite(result)) > 0)
})

test_that("compute_price_news consistency check", {
  # Load test data
  data <- extract_acm_data()

  # Run multiple times
  pn1 <- compute_price_news(data, data, i = 6)
  pn2 <- compute_price_news(data, data, i = 6)

  # Should be identical
  expect_equal(pn1, pn2)
})

test_that("compute_price_news relationship with yield changes", {
  # Load test data
  data <- extract_acm_data()

  # Compute price news
  i <- 5
  price_news <- compute_price_news(data, data, i = i)

  # Compute yield changes
  yield_col <- paste0("y", i)
  yield_changes <- diff(data[[yield_col]])

  # Price news and yield changes should be negatively correlated
  # (yields up -> prices down)
  valid_idx <- is.finite(price_news) & is.finite(yield_changes)
  if (sum(valid_idx) > 10) {
    correlation <- cor(price_news[valid_idx], yield_changes[valid_idx])
    expect_true(correlation < 0)
  }
})


test_that("compute_price_news error handling", {
  data <- extract_acm_data()

  # Invalid maturity
  expect_error(compute_price_news(data, data, i = -5))
  expect_error(compute_price_news(data, data, i = 50))

  # Missing required columns
  bad_data <- data[, c("date", "y1", "y2")] # Missing many columns
  expect_error(compute_price_news(bad_data, bad_data, i = 5))

  # Too few observations - now checking that it works with 2 observations
  tiny_data <- data[1:2, ]
  result <- compute_price_news(tiny_data, tiny_data, i = 2)
  expect_type(result, "double")
  expect_equal(length(result), 1) # 2 observations -> 1 difference
})
