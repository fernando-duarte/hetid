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
      "not matrix", tau, L_i, V_i, Q_i,
      s_i_0, s_i_1, s_i_2, sigma_i_sq
    ),
    "gamma must be a matrix"
  )

  # Test tau validation
  expect_error(
    compute_identified_set_quadratic(
      gamma, "not numeric", L_i, V_i, Q_i,
      s_i_0, s_i_1, s_i_2, sigma_i_sq
    ),
    "tau must be a numeric vector"
  )

  # Test tau positive values
  expect_error(
    compute_identified_set_quadratic(
      gamma, c(1, -1, 1, 1), L_i, V_i, Q_i,
      s_i_0, s_i_1, s_i_2, sigma_i_sq
    ),
    "All elements of tau must be positive"
  )

  # Test tau length
  expect_error(
    compute_identified_set_quadratic(
      gamma, c(1, 1, 1), L_i, V_i, Q_i,
      s_i_0, s_i_1, s_i_2, sigma_i_sq
    ),
    "tau must have length I"
  )

  # Test L_i and V_i numeric
  expect_error(
    compute_identified_set_quadratic(
      gamma, tau, "not numeric", V_i, Q_i,
      s_i_0, s_i_1, s_i_2, sigma_i_sq
    ),
    "L_i and V_i must be numeric vectors"
  )

  # Test Q_i list
  expect_error(
    compute_identified_set_quadratic(
      gamma, tau, L_i, V_i, "not list",
      s_i_0, s_i_1, s_i_2, sigma_i_sq
    ),
    "Q_i must be a list"
  )

  # Test s_i_1 and s_i_2 lists
  expect_error(
    compute_identified_set_quadratic(
      gamma, tau, L_i, V_i, Q_i,
      s_i_0, "not list", s_i_2, sigma_i_sq
    ),
    "s_i_1 and s_i_2 must be lists"
  )
})

test_that(
  "compute_identified_set_quadratic returns correct structure",
  {
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
      M %*% t(M)
    })
    sigma_i_sq <- runif(I, 0.1, 1)

    result <- compute_identified_set_quadratic(
      gamma, tau, L_i, V_i, Q_i,
      s_i_0, s_i_1, s_i_2, sigma_i_sq
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
      expect_named(
        result$b_i[[i]], paste0("maturity_", 1:I)
      )
    }
  }
)

test_that("rejects mismatched input lengths with subset", {
  set.seed(999)
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

  maturities <- c(1, 3, 5)

  # Full-length s_i_1 with subsetted L_i should error
  expect_error(
    compute_identified_set_quadratic(
      gamma, tau,
      L_i[maturities], V_i[maturities],
      Q_i[maturities],
      s_i_0[maturities], s_i_1, s_i_2,
      sigma_i_sq[maturities],
      maturities = maturities
    ),
    "length matching maturities"
  )
})
