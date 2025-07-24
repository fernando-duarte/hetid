test_that("compute_vector_statistics validates inputs correctly", {
  # Test w1 validation
  expect_error(
    compute_vector_statistics("not numeric", matrix(1:10, 5, 2), matrix(1:15, 5, 3)),
    "w1 must be a numeric vector"
  )

  # Test w2 validation
  expect_error(
    compute_vector_statistics(1:5, "not matrix", matrix(1:15, 5, 3)),
    "w2 must be a matrix or data frame"
  )

  # Test pcs validation
  expect_error(
    compute_vector_statistics(1:5, matrix(1:10, 5, 2), "not matrix"),
    "pcs must be a matrix or data frame"
  )

  # Test dimension mismatches
  expect_error(
    compute_vector_statistics(1:5, matrix(1:12, 6, 2), matrix(1:15, 5, 3)),
    "w1 and w2 must have the same number of observations"
  )
  expect_error(
    compute_vector_statistics(1:5, matrix(1:10, 5, 2), matrix(1:18, 6, 3)),
    "pcs must have the same number of observations as w1 and w2"
  )

  # Test invalid maturities
  expect_error(
    compute_vector_statistics(1:5, matrix(1:10, 5, 2), matrix(1:15, 5, 3),
      maturities = c(0, 1)
    ),
    "maturities must be between 1 and ncol\\(w2\\)"
  )
})

test_that("compute_vector_statistics returns correct structure", {
  # Create test data
  set.seed(123)
  T_obs <- 100
  I <- 3
  J <- 4
  w1 <- rnorm(T_obs)
  w2 <- matrix(rnorm(T_obs * I), T_obs, I)
  pcs <- matrix(rnorm(T_obs * J), T_obs, J)

  result <- compute_vector_statistics(w1, w2, pcs)

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("r_i_0", "r_i_1", "p_i_0"))

  # Check r_i_0 dimensions and structure
  expect_true(is.matrix(result$r_i_0))
  expect_equal(dim(result$r_i_0), c(J, I))
  expect_equal(rownames(result$r_i_0), paste0("pc", 1:J))
  expect_equal(colnames(result$r_i_0), paste0("maturity_", 1:I))

  # Check r_i_1 structure
  expect_type(result$r_i_1, "list")
  expect_length(result$r_i_1, I)
  expect_named(result$r_i_1, paste0("maturity_", 1:I))

  # Check each r_i_1 matrix
  for (i in 1:I) {
    mat <- result$r_i_1[[i]]
    expect_true(is.matrix(mat))
    expect_equal(dim(mat), c(J, I))
    expect_equal(rownames(mat), paste0("pc", 1:J))
    expect_equal(colnames(mat), paste0("maturity_", 1:I))
  }

  # Check p_i_0 dimensions and structure
  expect_true(is.matrix(result$p_i_0))
  expect_equal(dim(result$p_i_0), c(J, I))
  expect_equal(rownames(result$p_i_0), paste0("pc", 1:J))
  expect_equal(colnames(result$p_i_0), paste0("maturity_", 1:I))
})

test_that("compute_vector_statistics computes correct values", {
  # Simple test case with known values
  w1 <- c(1, 2, 3)
  w2 <- matrix(c(2, 3, 4, 5, 6, 7), nrow = 3, ncol = 2)
  pcs <- matrix(c(1, 0, 0, 0, 1, 0), nrow = 3, ncol = 2) # Simple PC matrix

  result <- compute_vector_statistics(w1, w2, pcs)

  # Manual calculation for R_1^(0)
  # w2_1 = c(2, 3, 4)
  # w1 ⊙ w2_1 = c(1*2, 2*3, 3*4) = c(2, 6, 12)
  # PC^T * (w1 ⊙ w2_1) = [1,0,0; 0,1,0] * [2,6,12]^T = [2, 6]
  # R_1^(0) = (1/3) * [2, 6]
  hadamard <- w1 * w2[, 1]
  expected_r_1_0 <- t(pcs) %*% hadamard / 3
  expect_equal(unname(result$r_i_0[, 1]), as.vector(expected_r_1_0))

  # Manual calculation for P_1^(0)
  # w2_1^2 = c(4, 9, 16)
  # PC^T * w2_1^2 = [1,0,0; 0,1,0] * [4,9,16]^T = [4, 9]
  # P_1^(0) = (1/3) * [4, 9]
  w2_1_sq <- w2[, 1]^2
  expected_p_1_0 <- t(pcs) %*% w2_1_sq / 3
  expect_equal(unname(result$p_i_0[, 1]), as.vector(expected_p_1_0))
})

test_that("compute_vector_statistics handles subset of maturities", {
  set.seed(456)
  T_obs <- 50
  I <- 5
  J <- 3
  w1 <- rnorm(T_obs)
  w2 <- matrix(rnorm(T_obs * I), T_obs, I)
  pcs <- matrix(rnorm(T_obs * J), T_obs, J)

  # Test with subset of maturities
  maturities <- c(2, 4)
  result <- compute_vector_statistics(w1, w2, pcs, maturities = maturities)

  # Check dimensions
  expect_equal(ncol(result$r_i_0), length(maturities))
  expect_equal(ncol(result$p_i_0), length(maturities))
  expect_length(result$r_i_1, length(maturities))

  # Check names
  expect_equal(colnames(result$r_i_0), paste0("maturity_", maturities))
  expect_equal(colnames(result$p_i_0), paste0("maturity_", maturities))
  expect_named(result$r_i_1, paste0("maturity_", maturities))
})

test_that("compute_vector_statistics handles orthogonal PCs correctly", {
  # Test with orthogonal PCs
  T_obs <- 4
  w1 <- c(1, 2, 3, 4)
  w2 <- matrix(c(2, 3, 4, 5), nrow = 4, ncol = 1)

  # Orthogonal PCs
  pcs <- matrix(c(
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0
  ), nrow = 4, ncol = 3, byrow = FALSE)

  result <- compute_vector_statistics(w1, w2, pcs)

  # With these PCs, each PC picks out one element
  # R_1^(0) should be (1/4) * [w1[1]*w2[1], w1[2]*w2[2], w1[3]*w2[3]]
  expected_r_1_0 <- c(w1[1] * w2[1, 1], w1[2] * w2[2, 1], w1[3] * w2[3, 1]) / 4
  expect_equal(unname(result$r_i_0[, 1]), expected_r_1_0)
})
