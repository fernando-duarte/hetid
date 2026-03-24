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
  "sentinel values verify tau uses maturity value, not position",
  {
    # Independent mathematical oracle using distinct sentinel
    # values for tau. This catches bugs that tautological
    # batch-vs-individual tests cannot: if tau were indexed by
    # position instead of maturity value, d_i would differ.
    #
    # Formula: d_i = tau_i^2 * V_i / sigma_i_sq
    # With maturities = c(2, 4, 6):
    #   tau[2]=2, tau[4]=4, tau[6]=6 (sentinel values)
    #   V_i = c(1, 1, 1), sigma_i_sq = c(1, 1, 1)
    # Expected: d_i = c(4, 16, 36)
    # Wrong (position-indexed): tau[1]=1, tau[2]=2, tau[3]=3
    #   would give d_i = c(1, 4, 9)

    J <- 2
    I <- 6
    maturities <- c(2, 4, 6)

    gamma <- diag(1, J, I)
    tau <- c(1, 2, 3, 4, 5, 6) # sentinel: tau[i] = i

    # Position-indexed inputs (length 3)
    L_i <- rep(0, 3)
    V_i <- rep(1, 3)
    Q_i <- lapply(1:3, function(k) rep(0, I))
    s_i_0 <- rep(0, 3)
    s_i_1 <- lapply(1:3, function(k) rep(0, I))
    s_i_2 <- lapply(1:3, function(k) matrix(0, I, I))
    sigma_i_sq <- rep(1, 3)

    result <- compute_identified_set_quadratic(
      gamma, tau, L_i, V_i, Q_i,
      s_i_0, s_i_1, s_i_2, sigma_i_sq,
      maturities = maturities
    )

    # Expected d_i: tau[c(2,4,6)]^2 times 1/1 gives c(4, 16, 36)
    expect_equal(
      unname(result$d_i), c(4, 16, 36),
      info = "tau must be indexed by maturity value, not position"
    )

    # With L_i=0, Q_i=0: A_i = -d_i * S_i^(2) = 0
    for (idx in seq_along(maturities)) {
      expect_equal(
        result$A_i[[idx]],
        matrix(0, I, I),
        info = paste("A_i should be zero for maturity", maturities[idx])
      )
    }

    # Expected b_i: all terms vanish because L_i and Q_i are zero
    for (idx in seq_along(maturities)) {
      expect_equal(
        unname(result$b_i[[idx]]),
        rep(0, I),
        info = paste("b_i should be zero for maturity", maturities[idx])
      )
    }

    # Expected c_i: vanishes because both L_i and s_i_0 are zero
    expect_equal(
      unname(result$c_i), rep(0, 3),
      info = "c_i should be zero with L_i=0 and s_i_0=0"
    )
  }
)

test_that(
  "components -> quadratic pipeline with non-contiguous maturities",
  {
    # End-to-end test chaining components() into quadratic()
    # with non-contiguous maturities.
    #
    # Verifies against an independent oracle for d_i and c_i

    set.seed(2024)
    J <- 3
    I <- 6
    maturities <- c(2, 4, 6)

    gamma <- matrix(rnorm(J * I), J, I)
    tau <- runif(I, 0.5, 2)

    T_obs <- 100
    w1 <- rnorm(T_obs)
    w2 <- matrix(rnorm(T_obs * I), nrow = T_obs, ncol = I)
    pcs <- matrix(rnorm(T_obs * J), nrow = T_obs, ncol = J)

    scalar_stats <- compute_scalar_statistics(w1, w2)
    vec_stats <- compute_vector_statistics(w1, w2, pcs)
    mat_stats <- compute_matrix_statistics(w1, w2)

    components <- compute_identified_set_components(
      gamma = gamma,
      r_i_0 = vec_stats$r_i_0,
      r_i_1 = vec_stats$r_i_1,
      p_i_0 = vec_stats$p_i_0,
      maturities = maturities
    )

    quad <- compute_identified_set_quadratic(
      gamma = gamma,
      tau = tau,
      L_i = components$L_i,
      V_i = components$V_i,
      Q_i = components$Q_i,
      s_i_0 = scalar_stats$s_i_0[maturities],
      s_i_1 = mat_stats$s_i_1[maturities],
      s_i_2 = mat_stats$s_i_2[maturities],
      sigma_i_sq = scalar_stats$sigma_i_sq[maturities],
      maturities = maturities
    )

    # Independent oracle for d_i using the formula directly
    expected_d_i <- tau[maturities]^2 *
      unname(components$V_i) /
      unname(scalar_stats$sigma_i_sq[maturities])
    expect_equal(
      unname(quad$d_i), expected_d_i,
      info = "d_i must use tau indexed by maturity value"
    )

    # Independent oracle for c_i using the formula directly
    expected_c_i <- unname(components$L_i)^2 -
      expected_d_i * unname(scalar_stats$s_i_0[maturities])
    expect_equal(
      unname(quad$c_i), expected_c_i,
      info = "c_i must match manual formula"
    )
  }
)

test_that(
  "omitting maturities with subsetted stats gives wrong tau",
  {
    # Negative test: if a caller forgets to pass maturities
    # when using subsetted stats, quadratic() defaults to
    # maturities = 1:3. Then tau[1:3] is used instead of
    # tau[c(2,4,6)], giving silently wrong d_i.
    #
    # This test documents the known API trap (L6).

    J <- 2
    I <- 6
    maturities <- c(2, 4, 6)
    tau <- c(1, 2, 3, 4, 5, 6)

    gamma <- diag(1, J, I)
    L_i <- rep(0, 3)
    V_i <- rep(1, 3)
    Q_i <- lapply(1:3, function(k) rep(0, I))
    s_i_0 <- rep(0, 3)
    s_i_1 <- lapply(1:3, function(k) rep(0, I))
    s_i_2 <- lapply(1:3, function(k) matrix(0, I, I))
    sigma_i_sq <- rep(1, 3)

    # Correct call: tau[c(2,4,6)]^2 = c(4, 16, 36)
    correct <- compute_identified_set_quadratic(
      gamma, tau, L_i, V_i, Q_i,
      s_i_0, s_i_1, s_i_2, sigma_i_sq,
      maturities = maturities
    )

    # Omitted maturities: defaults to 1:3,
    # so tau[1:3]^2 = c(1, 4, 9) -- WRONG values
    wrong <- compute_identified_set_quadratic(
      gamma, tau, L_i, V_i, Q_i,
      s_i_0, s_i_1, s_i_2, sigma_i_sq
      # maturities omitted!
    )

    # Confirm the results DIFFER -- proving the trap exists
    expect_false(
      identical(unname(correct$d_i), unname(wrong$d_i)),
      info = paste(
        "Omitting maturities silently uses tau[1:3]",
        "instead of tau[c(2,4,6)]"
      )
    )

    # Verify correct call used the right tau values
    expect_equal(unname(correct$d_i), c(4, 16, 36))
    # Verify wrong call used position-indexed tau
    expect_equal(unname(wrong$d_i), c(1, 4, 9))
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
