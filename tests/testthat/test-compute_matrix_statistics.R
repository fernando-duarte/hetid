test_that("compute_matrix_statistics validates w1 input", {
  expect_error(
    compute_matrix_statistics("bad", matrix(1:6, 3, 2)),
    "w1 must be a numeric vector"
  )
})

test_that("compute_matrix_statistics validates w2 input", {
  expect_error(
    compute_matrix_statistics(1:3, "bad"),
    "w2 must be a matrix or data frame"
  )
})

test_that("compute_matrix_statistics returns correct structure", {
  # Create test data
  set.seed(123)
  n_obs <- 100
  I <- 3
  w1 <- rnorm(n_obs)
  w2 <- matrix(rnorm(n_obs * I), n_obs, I)

  result <- compute_matrix_statistics(w1, w2)

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("s_i_1", "s_i_2"))

  # Check s_i_1 structure
  expect_type(result$s_i_1, "list")
  expect_length(result$s_i_1, I)
  expect_named(result$s_i_1, paste0("maturity_", 1:I))

  # Check each s_i_1 vector
  for (i in 1:I) {
    vec <- result$s_i_1[[i]]
    expect_type(vec, "double")
    expect_length(vec, I)
    expect_named(vec, paste0("maturity_", 1:I))
  }

  # Check s_i_2 structure
  expect_type(result$s_i_2, "list")
  expect_length(result$s_i_2, I)
  expect_named(result$s_i_2, paste0("maturity_", 1:I))

  # Check each s_i_2 matrix
  for (i in 1:I) {
    mat <- result$s_i_2[[i]]
    expect_true(is.matrix(mat))
    expect_equal(dim(mat), c(I, I))
    expect_equal(rownames(mat), paste0("maturity_", 1:I))
    expect_equal(colnames(mat), paste0("maturity_", 1:I))
  }
})

test_that("compute_matrix_statistics computes correct values", {
  # Simple test case with known values
  w1 <- c(1, 2, 3)
  w2 <- matrix(c(2, 3, 4, 5, 6, 7), nrow = 3, ncol = 2)

  result <- compute_matrix_statistics(w1, w2)

  # Manual calculation for maturity 1
  # W_2^{circ 1} is diag(w2_1) times w2, i.e., w2_1 scales each row
  w2_1 <- w2[, 1]
  w2_circ_1 <- w2_1 * w2

  # S_1^(1) = (1/3) * (w1 ⊙ w2_1)^T * W_2^{circ 1}
  # w1 ⊙ w2_1 = c(1*2, 2*3, 3*4) = c(2, 6, 12)
  hadamard <- w1 * w2_1
  expected_s_1_1 <- t(hadamard) %*% w2_circ_1 / 3
  expect_equal(unname(result$s_i_1[[1]]), as.vector(expected_s_1_1))

  # S_1^(2) = (1/3) * (W_2^{circ 1})^T * W_2^{circ 1}
  expected_s_1_2 <- t(w2_circ_1) %*% w2_circ_1 / 3
  expect_equal(unname(result$s_i_2[[1]]), unname(expected_s_1_2))
})

test_that("compute_matrix_statistics handles subset of maturities", {
  set.seed(456)
  n_obs <- 50
  I <- 5
  w1 <- rnorm(n_obs)
  w2 <- matrix(rnorm(n_obs * I), n_obs, I)

  # Test with subset of maturities
  maturities <- c(2, 4)
  result <- compute_matrix_statistics(w1, w2, maturities = maturities)

  # Check only requested maturities are computed
  expect_length(result$s_i_1, length(maturities))
  expect_length(result$s_i_2, length(maturities))
  expect_named(result$s_i_1, paste0("maturity_", maturities))
  expect_named(result$s_i_2, paste0("maturity_", maturities))

  # But each result still has dimension I
  expect_length(result$s_i_1[[1]], I)
  expect_equal(dim(result$s_i_2[[1]]), c(I, I))
})

test_that("compute_matrix_statistics produces symmetric S_i^(2)", {
  set.seed(789)
  n_obs <- 20
  I <- 3
  w1 <- rnorm(n_obs)
  w2 <- matrix(rnorm(n_obs * I), n_obs, I)

  result <- compute_matrix_statistics(w1, w2)

  # S_i^(2) should be symmetric since it's (W_2^{circ i})^T * W_2^{circ i}
  for (i in 1:I) {
    s_i_2_mat <- result$s_i_2[[i]]
    expect_true(isSymmetric(s_i_2_mat))
  }
})

test_that("compute_matrix_statistics handles zero residuals correctly", {
  # Test with zero w1
  w1 <- rep(0, 10)
  w2 <- matrix(rnorm(20), nrow = 10, ncol = 2)

  result <- compute_matrix_statistics(w1, w2)

  # S_i^(1) should be all zeros when w1 is zero
  expect_equal(unname(result$s_i_1[[1]]), rep(0, 2))
  expect_equal(unname(result$s_i_1[[2]]), rep(0, 2))

  # S_i^(2) should still be non-zero
  expect_false(all(result$s_i_2[[1]] == 0))

  # Test with zero column in w2
  w1 <- rnorm(10)
  w2 <- matrix(c(rep(0, 10), rnorm(10)), nrow = 10, ncol = 2)

  result <- compute_matrix_statistics(w1, w2)

  # When w2_1 is zero, W_2^{circ 1} is zero, so S_1^(1) and S_1^(2) are zero
  expect_equal(unname(result$s_i_1[[1]]), rep(0, 2))
  expect_equal(unname(result$s_i_2[[1]]), matrix(0, 2, 2))
})
