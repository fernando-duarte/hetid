test_that("compute_c_hat returns single numeric value for given maturity", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for maturity 5
  c_hat_5 <- compute_c_hat(yields, term_premia, i = 5)

  expect_type(c_hat_5, "double")
  expect_length(c_hat_5, 1)
  expect_true(is.finite(c_hat_5))
})

test_that("c_hat is always positive", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for all maturities
  for (i in 1:9) {
    c_hat_i <- compute_c_hat(yields, term_premia, i = i)
    expect_gt(c_hat_i, 0,
      label = paste("c_hat should be positive for maturity", i)
    )
  }
})

test_that("c_hat bounds are correct", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # For maturity 1, should be < 1.02
  c_hat_1 <- compute_c_hat(yields, term_premia, i = 1)
  expect_lt(c_hat_1, 1.02,
    label = "c_hat for maturity 1 should be below 1.02"
  )

  # For maturities > 1, should be < 1
  for (i in 2:9) {
    c_hat_i <- compute_c_hat(yields, term_premia, i = i)
    expect_lt(c_hat_i, 1,
      label = paste("c_hat should be below 1 for maturity", i)
    )
  }
})

test_that("c_hat equals exp(2 * max(n_hat))", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for several maturities
  for (i in c(3, 5, 7)) {
    c_hat_i <- compute_c_hat(yields, term_premia, i = i)
    n_hat_i <- compute_n_hat(yields, term_premia, i = i)

    expected_c_hat <- exp(2 * max(n_hat_i, na.rm = TRUE))

    expect_equal(c_hat_i, expected_c_hat,
      tolerance = 1e-10,
      label = paste("c_hat should equal exp(2*max(n_hat)) for maturity", i)
    )
  }
})
