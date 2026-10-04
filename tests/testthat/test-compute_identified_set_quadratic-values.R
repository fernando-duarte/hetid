# Regression guard for the d/A/b/c arithmetic: hand-computed expectations hit
# the assembly kernel through the quadratic_from_components() test helper with
# raw statistics, so not circular

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
    expect_equal(
      A_i_mat, t(A_i_mat),
      tolerance = 1e-10,
      info = paste("A_i for maturity", i, "!= t(A_i)")
    )
  }
})

test_that("quadratic_from_components computes values correctly", {
  eval(
    parse("bodies/compute_identified_set_quadratic-values-contracts.R", encoding = "UTF-8")[[1]],
    environment()
  )
})

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
    quad <- build_quadratic_system(gamma, tau, moments)$quadratic

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
    full_quad <- build_quadratic_system(gamma, tau, full_moments)$quadratic

    subset_moments <- compute_identification_moments(
      w1, w2, pcs,
      maturities = maturities
    )
    subset_quad <- build_quadratic_system(gamma, tau, subset_moments)$quadratic

    nms <- paste0("maturity_", maturities)
    expect_identical(subset_quad$d_i, full_quad$d_i[nms])
    expect_identical(subset_quad$A_i, full_quad$A_i[nms])
    expect_identical(subset_quad$b_i, full_quad$b_i[nms])
    expect_identical(subset_quad$c_i, full_quad$c_i[nms])
  }
)

test_that("subset results match individual maturity computations", {
  eval(
    parse("bodies/compute_identified_set_quadratic-values-contracts.R", encoding = "UTF-8")[[2]],
    environment()
  )
})
