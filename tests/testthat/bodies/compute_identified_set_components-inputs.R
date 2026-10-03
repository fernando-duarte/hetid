{
  set.seed(99)
  n_obs <- 50
  I <- 6
  J <- 3
  maturities <- c(2, 4)

  w1 <- rnorm(n_obs)
  w2 <- matrix(rnorm(n_obs * I), n_obs, I)
  pcs <- matrix(rnorm(n_obs * J), n_obs, J)
  gamma <- matrix(rnorm(J * I), J, I)

  subset_moments <- compute_identification_moments(
    w1, w2, pcs,
    maturities = maturities
  )
  full_moments <- compute_identification_moments(w1, w2, pcs)

  subset_result <- compute_identified_set_components(gamma, subset_moments)
  full_result <- compute_identified_set_components(gamma, full_moments)

  expect_named(subset_result$L_i, paste0("maturity_", maturities))
  expect_identical(
    subset_result$L_i,
    full_result$L_i[paste0("maturity_", maturities)]
  )
  expect_identical(
    subset_result$V_i,
    full_result$V_i[paste0("maturity_", maturities)]
  )
  expect_identical(
    subset_result$Q_i,
    full_result$Q_i[paste0("maturity_", maturities)]
  )
}
