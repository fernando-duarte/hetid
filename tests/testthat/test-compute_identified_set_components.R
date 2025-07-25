test_that("compute_identified_set_components validates inputs correctly", {
  # Test gamma validation
  expect_error(
    compute_identified_set_components(
      "not matrix",
      matrix(1:12, 3, 4),
      list(matrix(1:12, 3, 4)),
      matrix(1:12, 3, 4)
    ),
    "gamma must be a matrix"
  )

  # Test r_i_0 validation
  expect_error(
    compute_identified_set_components(
      matrix(1:12, 3, 4),
      "not matrix",
      list(matrix(1:12, 3, 4)),
      matrix(1:12, 3, 4)
    ),
    "r_i_0 must be a matrix"
  )

  # Test r_i_1 validation
  expect_error(
    compute_identified_set_components(
      matrix(1:12, 3, 4),
      matrix(1:12, 3, 4),
      "not list",
      matrix(1:12, 3, 4)
    ),
    "r_i_1 must be a list"
  )

  # Test p_i_0 validation
  expect_error(
    compute_identified_set_components(
      matrix(1:12, 3, 4),
      matrix(1:12, 3, 4),
      list(matrix(1:12, 3, 4)),
      "not matrix"
    ),
    "p_i_0 must be a matrix"
  )

  # Test dimension mismatches
  expect_error(
    compute_identified_set_components(
      matrix(1:12, 3, 4),
      matrix(1:20, 4, 5), # Wrong dimensions
      list(matrix(1:12, 3, 4)),
      matrix(1:12, 3, 4)
    ),
    "r_i_0 must have dimensions J x I matching gamma"
  )

  # Test r_i_1 length
  expect_error(
    compute_identified_set_components(
      matrix(1:12, 3, 4),
      matrix(1:12, 3, 4),
      list(matrix(1:12, 3, 4), matrix(1:12, 3, 4)), # Wrong length
      matrix(1:12, 3, 4)
    ),
    "r_i_1 must have I elements"
  )

  # Test invalid maturities
  expect_error(
    compute_identified_set_components(
      matrix(1:12, 3, 4),
      matrix(1:12, 3, 4),
      list(
        matrix(1:12, 3, 4), matrix(1:12, 3, 4),
        matrix(1:12, 3, 4), matrix(1:12, 3, 4)
      ),
      matrix(1:12, 3, 4),
      maturities = c(0, 1)
    ),
    "maturities must be between 1 and I"
  )
})

test_that("compute_identified_set_components returns correct structure", {
  # Create test data
  set.seed(123)
  J <- 4 # number of PCs
  I <- 3 # number of maturities

  gamma <- matrix(rnorm(J * I), J, I)
  r_i_0 <- matrix(rnorm(J * I), J, I)
  p_i_0 <- matrix(rnorm(J * I), J, I)

  # Create r_i_1 as list of matrices
  r_i_1 <- lapply(1:I, function(i) matrix(rnorm(J * I), J, I))

  result <- compute_identified_set_components(gamma, r_i_0, r_i_1, p_i_0)

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("L_i", "V_i", "Q_i"))

  # Check L_i
  expect_type(result$L_i, "double")
  expect_length(result$L_i, I)
  expect_named(result$L_i, paste0("maturity_", 1:I))

  # Check V_i
  expect_type(result$V_i, "double")
  expect_length(result$V_i, I)
  expect_named(result$V_i, paste0("maturity_", 1:I))

  # Check Q_i
  expect_type(result$Q_i, "list")
  expect_length(result$Q_i, I)
  expect_named(result$Q_i, paste0("maturity_", 1:I))

  # Each Q_i element should be a vector of length I
  for (i in 1:I) {
    expect_type(result$Q_i[[i]], "double")
    expect_length(result$Q_i[[i]], I)
    expect_named(result$Q_i[[i]], paste0("maturity_", 1:I))
  }
})

test_that("compute_identified_set_components computes values correctly", {
  # Create simple test case with known values
  J <- 2
  I <- 2

  gamma <- matrix(c(1, 0, 0, 1), J, I) # Identity-like
  r_i_0 <- matrix(c(2, 3, 4, 5), J, I)
  p_i_0 <- matrix(c(1, 2, 3, 4), J, I)
  r_i_1 <- list(
    matrix(c(1, 2, 3, 4), J, I),
    matrix(c(5, 6, 7, 8), J, I)
  )

  result <- compute_identified_set_components(gamma, r_i_0, r_i_1, p_i_0)

  # Test L_i = gamma_i^T * r_i_0[, i]
  # For i=1: gamma_1 = [1, 0]^T, r_i_0[,1] = [2, 3]^T
  # L_1 = 1*2 + 0*3 = 2
  expect_equal(unname(result$L_i[1]), 2)

  # For i=2: gamma_2 = [0, 1]^T, r_i_0[,2] = [4, 5]^T
  # L_2 = 0*4 + 1*5 = 5
  expect_equal(unname(result$L_i[2]), 5)

  # Test V_i = gamma_i^T * (p_i_0[,i] * p_i_0[,i]^T) * gamma_i
  # For i=1: p_i_0[,1] = [1, 2]^T
  # P_outer = [1, 2] * [1, 2]^T = [[1, 2], [2, 4]]
  # V_1 = [1, 0] * [[1, 2], [2, 4]] * [1, 0]^T = 1
  expect_equal(unname(result$V_i[1]), 1)

  # For i=2: p_i_0[,2] = [3, 4]^T
  # P_outer = [3, 4] * [3, 4]^T = [[9, 12], [12, 16]]
  # V_2 = [0, 1] * [[9, 12], [12, 16]] * [0, 1]^T = 16
  expect_equal(unname(result$V_i[2]), 16)

  # Test Q_i = gamma_i^T * R_i^(1)
  # For i=1: gamma_1 = [1, 0]^T, R_1^(1) = [[1, 3], [2, 4]]
  # Q_1 = [1, 0] * [[1, 3], [2, 4]] = [1, 3]
  expect_equal(result$Q_i[[1]], c(maturity_1 = 1, maturity_2 = 3))

  # For i=2: gamma_2 = [0, 1]^T, R_2^(1) = [[5, 7], [6, 8]]
  # Q_2 = [0, 1] * [[5, 7], [6, 8]] = [6, 8]
  expect_equal(result$Q_i[[2]], c(maturity_1 = 6, maturity_2 = 8))
})

test_that("compute_identified_set_components handles subset of maturities", {
  # Create test data
  set.seed(123)
  J <- 3
  I <- 4

  gamma <- matrix(rnorm(J * I), J, I)
  r_i_0 <- matrix(rnorm(J * I), J, I)
  p_i_0 <- matrix(rnorm(J * I), J, I)
  r_i_1 <- lapply(1:I, function(i) matrix(rnorm(J * I), J, I))

  # Compute for subset of maturities
  maturities <- c(2, 4)
  result <- compute_identified_set_components(
    gamma, r_i_0, r_i_1, p_i_0,
    maturities = maturities
  )

  # Check that we only get results for requested maturities
  expect_length(result$L_i, 2)
  expect_length(result$V_i, 2)
  expect_length(result$Q_i, 2)
  expect_named(result$L_i, paste0("maturity_", maturities))
  expect_named(result$V_i, paste0("maturity_", maturities))
  expect_named(result$Q_i, paste0("maturity_", maturities))
})
