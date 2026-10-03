{
  set.seed(123)
  J <- 3
  I <- 4
  moments <- make_components_moments(
    r_i_0 = matrix(rnorm(J * I), J, I),
    r_i_1 = lapply(1:I, function(k) matrix(rnorm(J * I), J, I)),
    p_i_0 = matrix(rnorm(J * I), J, I),
    maturities = 1:I, n_components = I
  )

  expect_error(
    compute_identified_set_components(matrix(rnorm(J * I), J, I), "not moments"),
    "hetid_moments object",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_identified_set_components("not matrix", moments),
    "gamma must be a matrix",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_identified_set_components(matrix(rnorm(J * 3), J, 3), moments),
    "n_components",
    class = "hetid_error_dimension_mismatch"
  )
  expect_error(
    compute_identified_set_components(matrix(rnorm(2 * I), 2, I), moments),
    "same number of rows",
    class = "hetid_error_dimension_mismatch"
  )
}

{
  set.seed(123)
  J <- 3
  I <- 4
  moments <- make_components_moments(
    r_i_0 = matrix(rnorm(J * I), J, I),
    r_i_1 = lapply(1:I, function(k) matrix(rnorm(J * I), J, I)),
    p_i_0 = matrix(rnorm(J * I), J, I),
    maturities = 1:I, n_components = I
  )

  expect_error(
    compute_identified_set_components(matrix(NA_real_, J, I), moments),
    "gamma must not contain NA, NaN, or infinite values",
    class = "hetid_error_bad_argument"
  )

  gamma_inf <- matrix(rnorm(J * I), J, I)
  gamma_inf[2, 3] <- Inf
  expect_error(
    compute_identified_set_components(gamma_inf, moments),
    "gamma must not contain NA, NaN, or infinite values",
    class = "hetid_error_bad_argument"
  )

  expect_error(
    compute_identified_set_components(matrix(TRUE, J, I), moments),
    "gamma must contain only numeric values",
    class = "hetid_error_bad_argument"
  )
}

{
  set.seed(123)
  J <- 4 # number of PCs
  I <- 3 # number of maturities

  gamma <- matrix(rnorm(J * I), J, I)
  moments <- make_components_moments(
    r_i_0 = matrix(rnorm(J * I), J, I),
    r_i_1 = lapply(1:I, function(i) matrix(rnorm(J * I), J, I)),
    p_i_0 = matrix(rnorm(J * I), J, I),
    maturities = 1:I, n_components = I
  )

  result <- compute_identified_set_components(gamma, moments)

  expect_s3_class(result, "hetid_components")
  expect_named(result, c("L_i", "V_i", "Q_i"))
  expect_identical(attr(result, "maturities"), 1:I)
  expect_identical(attr(result, "n_components"), as.integer(I))

  expect_type(result$L_i, "double")
  expect_length(result$L_i, I)
  expect_named(result$L_i, paste0("maturity_", 1:I))

  expect_type(result$V_i, "double")
  expect_length(result$V_i, I)
  expect_named(result$V_i, paste0("maturity_", 1:I))

  expect_type(result$Q_i, "list")
  expect_length(result$Q_i, I)
  expect_named(result$Q_i, paste0("maturity_", 1:I))

  for (i in 1:I) {
    expect_type(result$Q_i[[i]], "double")
    expect_length(result$Q_i[[i]], I)
    expect_named(result$Q_i[[i]], paste0("maturity_", 1:I))
  }
}

{
  J <- 2
  I <- 2

  gamma <- matrix(c(1, 0, 0, 1), J, I) # Identity-like
  moments <- make_components_moments(
    r_i_0 = matrix(c(2, 3, 4, 5), J, I),
    r_i_1 = list(
      matrix(c(1, 2, 3, 4), J, I),
      matrix(c(5, 6, 7, 8), J, I)
    ),
    p_i_0 = matrix(c(1, 2, 3, 4), J, I),
    maturities = 1:I, n_components = I
  )

  result <- compute_identified_set_components(gamma, moments)

  # For maturity i, L_i is the inner product of gamma_i and r_i_0[,i]
  # gamma_1 = [1, 0], r_i_0[,1] = [2, 3] => L_1 = 2
  expect_equal(unname(result$L_i[1]), 2)

  # gamma_2 = [0, 1], r_i_0[,2] = [4, 5] => L_2 = 5
  expect_equal(unname(result$L_i[2]), 5)

  # For maturity i, V_i is gamma_i^T (p_i_0[,i] p_i_0[,i]^T) gamma_i
  # For i=1: p_i_0[,1] = [1, 2] => outer [[1, 2], [2, 4]] => V_1 = 1
  expect_equal(unname(result$V_i[1]), 1)

  # For i=2: p_i_0[,2] = [3, 4] => outer [[9, 12], [12, 16]] => V_2 = 16
  expect_equal(unname(result$V_i[2]), 16)

  # For maturity i, Q_i is gamma_i^T R_i^(1)
  # For i=1: gamma_1 = [1, 0], R_1^(1) = [[1, 3], [2, 4]] => Q_1 = [1, 3]
  expect_equal(result$Q_i[[1]], c(maturity_1 = 1, maturity_2 = 3))

  # For i=2: gamma_2 = [0, 1], R_2^(1) = [[5, 7], [6, 8]] => Q_2 = [6, 8]
  expect_equal(result$Q_i[[2]], c(maturity_1 = 6, maturity_2 = 8))
}

{
  set.seed(123)
  J <- 3
  I <- 4
  maturities <- c(2, 4)
  n_mat <- length(maturities)

  gamma <- matrix(rnorm(J * I), J, I)
  r_i_0 <- matrix(rnorm(J * n_mat), J, n_mat)
  p_i_0 <- matrix(rnorm(J * n_mat), J, n_mat)
  r_i_1 <- lapply(seq_len(n_mat), function(k) matrix(rnorm(J * I), J, I))

  moments <- make_components_moments(
    r_i_0, r_i_1, p_i_0,
    maturities = maturities, n_components = I
  )
  result <- compute_identified_set_components(gamma, moments)

  expect_length(result$L_i, n_mat)
  expect_named(result$L_i, paste0("maturity_", maturities))
  expect_identical(attr(result, "maturities"), c(2L, 4L))

  # Element k must use the gamma column for maturities[k], not position k
  for (k in seq_len(n_mat)) {
    expected <- as.numeric(
      crossprod(gamma[, maturities[k], drop = FALSE], moments$r_i_0[, k])
    )
    expect_equal(unname(result$L_i[k]), expected)
  }
}
