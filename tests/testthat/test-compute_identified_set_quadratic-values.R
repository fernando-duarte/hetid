test_that("A_i matrices are symmetric", {
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
    M %*% t(M)
  })
  sigma_i_sq <- runif(I, 0.1, 1)

  result <- compute_identified_set_quadratic(
    gamma, tau, L_i, V_i, Q_i,
    s_i_0, s_i_1, s_i_2, sigma_i_sq
  )

  # Check that each A_i is symmetric
  for (i in 1:I) {
    A_i_mat <- result$A_i[[i]]
    expect_true(
      isSymmetric(A_i_mat, tol = 1e-10),
      info = paste("A_i for maturity", i, "not symmetric")
    )
    # Element-wise transpose equality
    expect_equal(
      A_i_mat, t(A_i_mat),
      tolerance = 1e-10,
      info = paste("A_i for maturity", i, "!= t(A_i)")
    )
  }
})

test_that(
  "compute_identified_set_quadratic computes values correctly",
  {
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
      gamma, tau, L_i, V_i, Q_i,
      s_i_0, s_i_1, s_i_2, sigma_i_sq
    )

    # d_i is tau_i squared times V_i over sigma_i squared
    expect_equal(unname(result$d_i[1]), 8)
    expect_equal(unname(result$d_i[2]), 18)

    # A_i is the outer product of Q_i minus d_i times S_i^(2)
    expected_A_1 <- matrix(c(-7, -2, -2, -4), 2, 2)
    expect_equal(result$A_i[[1]], expected_A_1)

    # b_i is negative two L_i Q_i plus two d_i S_i^(1)
    expect_equal(
      result$b_i[[1]],
      c(maturity_1 = -8.4, maturity_2 = -16.8)
    )

    # c_i is L_i squared minus d_i times S_i^(0)
    expect_equal(unname(result$c_i[1]), 21)
    expect_equal(unname(result$c_i[2]), 34.6)
  }
)

test_that(
  "compute_identified_set_quadratic handles subset of maturities",
  {
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
      M %*% t(M)
    })
    sigma_i_sq <- runif(I, 0.1, 1)

    # Compute for subset of maturities
    maturities <- c(1, 3, 5)
    result <- compute_identified_set_quadratic(
      gamma, tau,
      L_i[maturities], V_i[maturities],
      Q_i[maturities],
      s_i_0[maturities],
      s_i_1[maturities], s_i_2[maturities],
      sigma_i_sq[maturities],
      maturities = maturities
    )

    # Check results only for requested maturities
    expect_length(result$d_i, 3)
    expect_length(result$A_i, 3)
    expect_length(result$b_i, 3)
    expect_length(result$c_i, 3)
    expect_named(result$d_i, paste0("maturity_", maturities))
    expect_named(result$A_i, paste0("maturity_", maturities))
    expect_named(result$b_i, paste0("maturity_", maturities))
    expect_named(result$c_i, paste0("maturity_", maturities))
  }
)

test_that(
  "subset results match individual maturity computations",
  {
    set.seed(42)
    J <- 3
    I <- 6
    gamma <- matrix(rnorm(J * I), J, I)
    tau <- runif(I, 0.5, 2)
    L_i <- runif(I)
    V_i <- runif(I)
    Q_i <- lapply(seq_len(I), function(k) rnorm(I))
    s_i_0 <- runif(I)
    s_i_1 <- lapply(seq_len(I), function(k) rnorm(I))
    s_i_2 <- lapply(seq_len(I), function(k) {
      M <- matrix(rnorm(I * I), I, I)
      M %*% t(M)
    })
    sigma_i_sq <- runif(I, 0.1, 1)

    maturities <- c(2, 4, 6)

    # Compute with subset
    result_batch <- compute_identified_set_quadratic(
      gamma, tau,
      L_i[maturities], V_i[maturities],
      Q_i[maturities],
      s_i_0[maturities],
      s_i_1[maturities], s_i_2[maturities],
      sigma_i_sq[maturities],
      maturities = maturities
    )

    # Compute individually for each maturity
    for (idx in seq_along(maturities)) {
      m <- maturities[idx]
      result_single <- compute_identified_set_quadratic(
        gamma, tau,
        L_i[m], V_i[m], Q_i[m],
        s_i_0[m],
        s_i_1[m], s_i_2[m],
        sigma_i_sq[m],
        maturities = m
      )
      mat_name <- paste0("maturity_", m)
      expect_equal(
        unname(result_batch$d_i[idx]),
        unname(result_single$d_i),
        info = paste("d_i mismatch for", mat_name)
      )
      expect_equal(
        result_batch$A_i[[idx]],
        result_single$A_i[[1]],
        info = paste("A_i mismatch for", mat_name)
      )
      expect_equal(
        result_batch$b_i[[idx]],
        result_single$b_i[[1]],
        info = paste("b_i mismatch for", mat_name)
      )
      expect_equal(
        unname(result_batch$c_i[idx]),
        unname(result_single$c_i),
        info = paste("c_i mismatch for", mat_name)
      )
    }
  }
)
