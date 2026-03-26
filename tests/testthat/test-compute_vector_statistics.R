test_that("compute_vector_statistics validates w1 input", {
  expect_error(
    compute_vector_statistics(
      "bad", matrix(1:6, 3, 2), matrix(1:6, 3, 2)
    ),
    "w1 must be a numeric vector"
  )
})

test_that("compute_vector_statistics validates w2 input", {
  expect_error(
    compute_vector_statistics(
      1:3, "bad", matrix(1:6, 3, 2)
    ),
    "w2 must be a matrix or data frame"
  )
})

test_that("compute_vector_statistics returns correct structure", {
  # Create test data
  set.seed(123)
  n_obs <- 100
  I <- 3
  J <- 4
  w1 <- rnorm(n_obs)
  w2 <- matrix(rnorm(n_obs * I), n_obs, I)
  pcs <- matrix(rnorm(n_obs * J), n_obs, J)

  result <- compute_vector_statistics(w1, w2, pcs)

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("r_i_0", "r_i_1", "p_i_0"))

  # Check r_i_0 dimensions and structure
  expect_true(is.matrix(result$r_i_0))
  expect_equal(dim(result$r_i_0), c(J, I))
  expect_equal(rownames(result$r_i_0), get_pc_column_names(J))
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
    expect_equal(rownames(mat), get_pc_column_names(J))
    expect_equal(colnames(mat), paste0("maturity_", 1:I))
  }

  # Check p_i_0 dimensions and structure
  expect_true(is.matrix(result$p_i_0))
  expect_equal(dim(result$p_i_0), c(J, I))
  expect_equal(rownames(result$p_i_0), get_pc_column_names(J))
  expect_equal(colnames(result$p_i_0), paste0("maturity_", 1:I))
})

test_that("compute_vector_statistics computes correct values", {
  # Simple test case with known values
  w1 <- c(1, 2, 3)
  w2 <- matrix(c(2, 3, 4, 5, 6, 7), nrow = 3, ncol = 2)
  pcs <- matrix(c(1, 0, 0, 0, 1, 0), nrow = 3, ncol = 2) # Simple PC matrix

  result <- compute_vector_statistics(w1, w2, pcs)

  # R_i^(0) is the mean of PC^T times the Hadamard product of w1 and w2_i
  hadamard <- w1 * w2[, 1]
  expected_r_1_0 <- t(pcs) %*% hadamard / 3
  expect_equal(unname(result$r_i_0[, 1]), as.vector(expected_r_1_0))

  # P_i^(0) is the mean of PC^T times the squared elements of w2_i
  w2_1_sq <- w2[, 1]^2
  expected_p_1_0 <- t(pcs) %*% w2_1_sq / 3
  expect_equal(unname(result$p_i_0[, 1]), as.vector(expected_p_1_0))
})

test_that("compute_vector_statistics handles subset of maturities", {
  set.seed(456)
  n_obs <- 50
  I <- 5
  J <- 3
  w1 <- rnorm(n_obs)
  w2 <- matrix(rnorm(n_obs * I), n_obs, I)
  pcs <- matrix(rnorm(n_obs * J), n_obs, J)

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
  n_obs <- 4
  w1 <- c(1, 2, 3, 4)
  w2 <- matrix(c(2, 3, 4, 5), nrow = n_obs, ncol = 1)

  # Orthogonal PCs
  pcs <- matrix(c(
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0
  ), nrow = n_obs, ncol = 3, byrow = FALSE)

  result <- compute_vector_statistics(w1, w2, pcs)

  # With these PCs, each PC picks out one element
  # R_1^(0) should be (1/4) * [w1[1]*w2[1], w1[2]*w2[2], w1[3]*w2[3]]
  expected_r_1_0 <- c(w1[1] * w2[1, 1], w1[2] * w2[2, 1], w1[3] * w2[3, 1]) / 4
  expect_equal(unname(result$r_i_0[, 1]), expected_r_1_0)
})
