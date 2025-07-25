test_that("compute_identified_set_quadratic validates inputs correctly", {
  # Create minimal valid inputs for other parameters
  gamma <- matrix(1:12, 3, 4)
  tau <- rep(1, 4)
  L_i <- rep(1, 4)
  V_i <- rep(1, 4)
  Q_i <- lapply(1:4, function(i) rep(1, 4))
  s_i_0 <- rep(1, 4)
  s_i_1 <- lapply(1:4, function(i) rep(1, 4))
  s_i_2 <- lapply(1:4, function(i) matrix(1, 4, 4))
  sigma_i_sq <- rep(1, 4)

  # Test gamma validation
  expect_error(
    compute_identified_set_quadratic(
      "not matrix", tau, L_i, V_i, Q_i, s_i_0, s_i_1, s_i_2, sigma_i_sq
    ),
    "gamma must be a matrix"
  )

  # Test tau validation
  expect_error(
    compute_identified_set_quadratic(
      gamma, "not numeric", L_i, V_i, Q_i, s_i_0, s_i_1, s_i_2, sigma_i_sq
    ),
    "tau must be a numeric vector"
  )

  # Test tau positive values
  expect_error(
    compute_identified_set_quadratic(
      gamma, c(1, -1, 1, 1), L_i, V_i, Q_i, s_i_0, s_i_1, s_i_2, sigma_i_sq
    ),
    "All elements of tau must be positive"
  )

  # Test tau length
  expect_error(
    compute_identified_set_quadratic(
      gamma, c(1, 1, 1), L_i, V_i, Q_i, s_i_0, s_i_1, s_i_2, sigma_i_sq
    ),
    "tau must have length I"
  )

  # Test L_i and V_i numeric
  expect_error(
    compute_identified_set_quadratic(
      gamma, tau, "not numeric", V_i, Q_i, s_i_0, s_i_1, s_i_2, sigma_i_sq
    ),
    "L_i and V_i must be numeric vectors"
  )

  # Test Q_i list
  expect_error(
    compute_identified_set_quadratic(
      gamma, tau, L_i, V_i, "not list", s_i_0, s_i_1, s_i_2, sigma_i_sq
    ),
    "Q_i must be a list"
  )

  # Test s_i_1 and s_i_2 lists
  expect_error(
    compute_identified_set_quadratic(
      gamma, tau, L_i, V_i, Q_i, s_i_0, "not list", s_i_2, sigma_i_sq
    ),
    "s_i_1 and s_i_2 must be lists"
  )
})

test_that("compute_identified_set_quadratic returns correct structure", {
  # Create test data
  set.seed(123)
  J <- 3
  I <- 4

  gamma <- matrix(rnorm(J * I), J, I)
  tau <- runif(I, 0.5, 2)
  L_i <- runif(I)
  V_i <- runif(I)
  Q_i <- lapply(1:I, function(i) rnorm(I))
  s_i_0 <- runif(I)
  s_i_1 <- lapply(1:I, function(i) rnorm(I))
  s_i_2 <- lapply(1:I, function(i) {
    M <- matrix(rnorm(I * I), I, I)
    M %*% t(M) # Make symmetric positive semi-definite
  })
  sigma_i_sq <- runif(I, 0.1, 1)

  result <- compute_identified_set_quadratic(
    gamma, tau, L_i, V_i, Q_i, s_i_0, s_i_1, s_i_2, sigma_i_sq
  )

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("d_i", "A_i", "b_i", "c_i"))

  # Check d_i
  expect_type(result$d_i, "double")
  expect_length(result$d_i, I)
  expect_named(result$d_i, paste0("maturity_", 1:I))

  # Check A_i
  expect_type(result$A_i, "list")
  expect_length(result$A_i, I)
  expect_named(result$A_i, paste0("maturity_", 1:I))

  # Check b_i
  expect_type(result$b_i, "list")
  expect_length(result$b_i, I)
  expect_named(result$b_i, paste0("maturity_", 1:I))

  # Check c_i
  expect_type(result$c_i, "double")
  expect_length(result$c_i, I)
  expect_named(result$c_i, paste0("maturity_", 1:I))

  # Check dimensions of A_i and b_i elements
  for (i in 1:I) {
    expect_true(is.matrix(result$A_i[[i]]))
    expect_equal(dim(result$A_i[[i]]), c(I, I))
    expect_type(result$b_i[[i]], "double")
    expect_length(result$b_i[[i]], I)
    expect_named(result$b_i[[i]], paste0("maturity_", 1:I))
  }
})

test_that("A_i matrices are symmetric", {
  # Create test data
  set.seed(456)
  J <- 4
  I <- 5

  gamma <- matrix(rnorm(J * I), J, I)
  tau <- runif(I, 0.5, 2)
  L_i <- runif(I)
  V_i <- runif(I)
  Q_i <- lapply(1:I, function(i) rnorm(I))
  s_i_0 <- runif(I)
  s_i_1 <- lapply(1:I, function(i) rnorm(I))
  s_i_2 <- lapply(1:I, function(i) {
    M <- matrix(rnorm(I * I), I, I)
    M %*% t(M) # Make symmetric positive semi-definite
  })
  sigma_i_sq <- runif(I, 0.1, 1)

  result <- compute_identified_set_quadratic(
    gamma, tau, L_i, V_i, Q_i, s_i_0, s_i_1, s_i_2, sigma_i_sq
  )

  # Check that each A_i is symmetric
  for (i in 1:I) {
    A_i_mat <- result$A_i[[i]]
    expect_true(
      isSymmetric(A_i_mat, tol = 1e-10),
      info = paste("A_i matrix for maturity", i, "should be symmetric")
    )

    # Also check element-wise that A[i,j] == A[j,i]
    for (row in 1:I) {
      for (col in 1:I) {
        expect_equal(
          A_i_mat[row, col],
          A_i_mat[col, row],
          tolerance = 1e-10,
          info = paste("A_i[", row, ",", col, "] should equal A_i[", col, ",", row, "]")
        )
      }
    }
  }
})

test_that("compute_identified_set_quadratic computes values correctly", {
  # Create simple test case with known values
  J <- 2
  I <- 2

  gamma <- matrix(c(1, 0, 0, 1), J, I)
  tau <- c(2, 3)
  L_i <- c(5, 7)
  V_i <- c(4, 6)
  Q_i <- list(c(1, 2), c(3, 4))
  s_i_0 <- c(0.5, 0.8)
  s_i_1 <- list(c(0.1, 0.2), c(0.3, 0.4))
  s_i_2 <- list(
    matrix(c(1, 0.5, 0.5, 1), 2, 2),
    matrix(c(2, 1, 1, 2), 2, 2)
  )
  sigma_i_sq <- c(2, 3)

  result <- compute_identified_set_quadratic(
    gamma, tau, L_i, V_i, Q_i, s_i_0, s_i_1, s_i_2, sigma_i_sq
  )

  # Test d_i = tau_i^2 * V_i / sigma_i^2
  # For i=1: d_1 = 2^2 * 4 / 2 = 16 / 2 = 8
  expect_equal(unname(result$d_i[1]), 8)

  # For i=2: d_2 = 3^2 * 6 / 3 = 54 / 3 = 18
  expect_equal(unname(result$d_i[2]), 18)

  # Test A_i = Q_i * Q_i^T - d_i * S_i^(2)
  # For i=1: Q_1 = [1, 2], Q_1 * Q_1^T = [[1, 2], [2, 4]]
  # A_1 = [[1, 2], [2, 4]] - 8 * [[1, 0.5], [0.5, 1]]
  #     = [[1, 2], [2, 4]] - [[8, 4], [4, 8]]
  #     = [[-7, -2], [-2, -4]]
  expected_A_1 <- matrix(c(-7, -2, -2, -4), 2, 2)
  expect_equal(result$A_i[[1]], expected_A_1)

  # Test b_i = -2 * L_i * Q_i + 2 * d_i * S_i^(1)
  # For i=1: b_1 = -2 * 5 * [1, 2] + 2 * 8 * [0.1, 0.2]
  #              = [-10, -20] + [1.6, 3.2]
  #              = [-8.4, -16.8]
  expect_equal(result$b_i[[1]], c(maturity_1 = -8.4, maturity_2 = -16.8))

  # Test c_i = L_i^2 - d_i * S_i^(0)
  # For i=1: c_1 = 5^2 - 8 * 0.5 = 25 - 4 = 21
  expect_equal(unname(result$c_i[1]), 21)

  # For i=2: c_2 = 7^2 - 18 * 0.8 = 49 - 14.4 = 34.6
  expect_equal(unname(result$c_i[2]), 34.6)
})

test_that("compute_identified_set_quadratic handles subset of maturities", {
  # Create test data
  set.seed(789)
  J <- 3
  I <- 6

  gamma <- matrix(rnorm(J * I), J, I)
  tau <- runif(I, 0.5, 2)
  L_i <- runif(I)
  V_i <- runif(I)
  Q_i <- lapply(1:I, function(i) rnorm(I))
  s_i_0 <- runif(I)
  s_i_1 <- lapply(1:I, function(i) rnorm(I))
  s_i_2 <- lapply(1:I, function(i) {
    M <- matrix(rnorm(I * I), I, I)
    M %*% t(M) # Make symmetric positive semi-definite
  })
  sigma_i_sq <- runif(I, 0.1, 1)

  # Compute for subset of maturities
  maturities <- c(1, 3, 5)
  result <- compute_identified_set_quadratic(
    gamma, tau, L_i[maturities], V_i[maturities], Q_i[maturities],
    s_i_0[maturities], s_i_1, s_i_2, sigma_i_sq[maturities],
    maturities = maturities
  )

  # Check that we only get results for requested maturities
  expect_length(result$d_i, 3)
  expect_length(result$A_i, 3)
  expect_length(result$b_i, 3)
  expect_length(result$c_i, 3)
  expect_named(result$d_i, paste0("maturity_", maturities))
  expect_named(result$A_i, paste0("maturity_", maturities))
  expect_named(result$b_i, paste0("maturity_", maturities))
  expect_named(result$c_i, paste0("maturity_", maturities))
})
