{
  I <- 2
  tau <- c(0.4, 0.6)
  L_i <- c(5, 7) # nolint: object_name_linter.
  V_i <- c(4, 6) # nolint: object_name_linter.
  Q_i <- list(c(1, 2), c(3, 4)) # nolint: object_name_linter.
  s_i_0 <- c(0.5, 0.8)
  s_i_1 <- list(c(0.1, 0.2), c(0.3, 0.4))
  s_i_2 <- list(
    matrix(c(1, 0.5, 0.5, 1), 2, 2),
    matrix(c(2, 1, 1, 2), 2, 2)
  )
  sigma_i_sq <- c(2, 3)

  result <- quadratic_from_components(
    tau, L_i, V_i, Q_i,
    s_i_0, s_i_1, s_i_2, sigma_i_sq,
    maturities = 1:I, n_components = I
  )

  # d_i is tau_i squared times V_i over sigma_i squared
  expect_equal(unname(result$d_i[1]), 0.32)
  expect_equal(unname(result$d_i[2]), 0.72)

  # A_i is the outer product of Q_i minus d_i times S_i^(2)
  expected_A_1 <- matrix( # nolint: object_name_linter.
    c(0.68, 1.84, 1.84, 3.68), 2, 2
  )
  expect_equal(result$A_i[[1]], expected_A_1)

  # b_i is negative two L_i Q_i plus two d_i S_i^(1)
  expect_equal(
    result$b_i[[1]],
    c(maturity_1 = -9.936, maturity_2 = -19.872)
  )

  # c_i is L_i squared minus d_i times S_i^(0)
  expect_equal(unname(result$c_i[1]), 24.84)
  expect_equal(unname(result$c_i[2]), 48.424)
}

{
  set.seed(42)
  I <- 6
  tau <- runif(I, 0.1, 0.9)
  L_i <- runif(I) # nolint: object_name_linter.
  V_i <- runif(I) # nolint: object_name_linter.
  Q_i <- lapply(seq_len(I), function(k) rnorm(I)) # nolint: object_name_linter.
  s_i_0 <- runif(I)
  s_i_1 <- lapply(seq_len(I), function(k) rnorm(I))
  s_i_2 <- lapply(seq_len(I), function(k) {
    M <- matrix(rnorm(I * I), I, I)
    M %*% t(M)
  })
  sigma_i_sq <- runif(I, 0.1, 1)

  maturities <- c(2, 4, 6)

  result_batch <- quadratic_from_components(
    tau,
    L_i[maturities], V_i[maturities],
    Q_i[maturities],
    s_i_0[maturities],
    s_i_1[maturities], s_i_2[maturities],
    sigma_i_sq[maturities],
    maturities = maturities, n_components = I
  )

  for (idx in seq_along(maturities)) {
    m <- maturities[idx]
    result_single <- quadratic_from_components(
      tau,
      L_i[m], V_i[m], Q_i[m],
      s_i_0[m],
      s_i_1[m], s_i_2[m],
      sigma_i_sq[m],
      maturities = m, n_components = I
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
