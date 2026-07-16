# Regression guard for the d/A/b/c arithmetic: hand-computed expectations hit
# the internal quadratic_from_components() with raw statistics, so not circular

test_that("A_i matrices are symmetric", {
  set.seed(456)
  I <- 5
  tau <- runif(I, 0.1, 0.9)
  L_i <- runif(I) # nolint: object_name_linter.
  V_i <- runif(I) # nolint: object_name_linter.
  Q_i <- lapply(1:I, function(i) rnorm(I)) # nolint: object_name_linter.
  s_i_0 <- runif(I)
  s_i_1 <- lapply(1:I, function(i) rnorm(I))
  s_i_2 <- lapply(1:I, function(i) {
    M <- matrix(rnorm(I * I), I, I)
    M %*% t(M)
  })
  sigma_i_sq <- runif(I, 0.1, 1)

  result <- quadratic_from_components(
    tau, L_i, V_i, Q_i,
    s_i_0, s_i_1, s_i_2, sigma_i_sq,
    maturities = 1:I, n_components = I
  )

  for (i in 1:I) {
    A_i_mat <- result$A_i[[i]] # nolint: object_name_linter.
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
  "quadratic_from_components computes values correctly",
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
)

test_that(
  "quadratic_from_components handles subset of maturities",
  {
    set.seed(789)
    I <- 6
    tau <- runif(I, 0.1, 0.9)
    L_i <- runif(I) # nolint: object_name_linter.
    V_i <- runif(I) # nolint: object_name_linter.
    Q_i <- lapply(1:I, function(i) rnorm(I)) # nolint: object_name_linter.
    s_i_0 <- runif(I)
    s_i_1 <- lapply(1:I, function(i) rnorm(I))
    s_i_2 <- lapply(1:I, function(i) {
      M <- matrix(rnorm(I * I), I, I)
      M %*% t(M)
    })
    sigma_i_sq <- runif(I, 0.1, 1)

    maturities <- c(1, 3, 5)
    result <- quadratic_from_components(
      tau,
      L_i[maturities], V_i[maturities],
      Q_i[maturities],
      s_i_0[maturities],
      s_i_1[maturities], s_i_2[maturities],
      sigma_i_sq[maturities],
      maturities = maturities, n_components = I
    )

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
    I <- 6
    tau <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
    maturities <- c(2, 4, 6)

    result <- quadratic_from_components(
      tau,
      L_i = rep(0, 3), V_i = rep(1, 3),
      Q_i = lapply(1:3, function(k) rep(0, I)),
      s_i_0 = rep(0, 3),
      s_i_1 = lapply(1:3, function(k) rep(0, I)),
      s_i_2 = lapply(1:3, function(k) matrix(0, I, I)),
      sigma_i_sq = rep(1, 3),
      maturities = maturities, n_components = I
    )

    # tau[c(2,4,6)]^2 times 1/1 gives c(0.04, 0.16, 0.36)
    expect_equal(
      unname(result$d_i), c(0.04, 0.16, 0.36),
      info = "tau must be indexed by maturity value"
    )
    expect_named(result$d_i, paste0("maturity_", maturities))
  }
)

test_that(
  "end-to-end pipeline matches the manual d_i formula",
  {
    set.seed(2024)
    J <- 3
    I <- 6
    maturities <- c(2, 4, 6)

    gamma <- matrix(rnorm(J * I), J, I)
    tau <- runif(I, 0.1, 0.9)

    n_obs <- 100
    w1 <- rnorm(n_obs)
    w2 <- matrix(
      rnorm(n_obs * I),
      nrow = n_obs, ncol = I
    )
    pcs <- matrix(
      rnorm(n_obs * J),
      nrow = n_obs, ncol = J
    )

    moments <- compute_identification_moments(
      w1, w2, pcs,
      maturities = maturities
    )
    components <- compute_identified_set_components(gamma, moments)
    quad <- compute_identified_set_quadratic(tau, components, moments)

    expected_d_i <- tau[maturities]^2 *
      unname(components$V_i) /
      unname(moments$sigma_i_sq)
    expect_equal(
      unname(quad$d_i), expected_d_i,
      info = "d_i must match manual formula"
    )
  }
)

test_that(
  "subset container results match the full-system constraints",
  {
    set.seed(31)
    J <- 3
    I <- 6
    maturities <- c(2, 4, 5)

    gamma <- matrix(rnorm(J * I), J, I)
    tau <- runif(I, 0.1, 0.9)

    n_obs <- 120
    w1 <- rnorm(n_obs)
    w2 <- matrix(rnorm(n_obs * I), nrow = n_obs, ncol = I)
    pcs <- matrix(rnorm(n_obs * J), nrow = n_obs, ncol = J)

    full_moments <- compute_identification_moments(w1, w2, pcs)
    full_quad <- compute_identified_set_quadratic(
      tau,
      compute_identified_set_components(gamma, full_moments),
      full_moments
    )

    subset_moments <- compute_identification_moments(
      w1, w2, pcs,
      maturities = maturities
    )
    subset_quad <- compute_identified_set_quadratic(
      tau,
      compute_identified_set_components(gamma, subset_moments),
      subset_moments
    )

    nms <- paste0("maturity_", maturities)
    expect_identical(subset_quad$d_i, full_quad$d_i[nms])
    expect_identical(subset_quad$A_i, full_quad$A_i[nms])
    expect_identical(subset_quad$b_i, full_quad$b_i[nms])
    expect_identical(subset_quad$c_i, full_quad$c_i[nms])
  }
)

test_that(
  "subset results match individual maturity computations",
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
)
