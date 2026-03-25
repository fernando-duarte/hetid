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

  # Test r_i_0 dimension mismatch
  expect_error(
    compute_identified_set_components(
      matrix(1:12, 3, 4),
      matrix(1:15, 3, 5),
      lapply(1:2, function(k) matrix(1:12, 3, 4)),
      matrix(1:6, 3, 2),
      maturities = c(1, 2)
    ),
    "r_i_0 must be J x length"
  )

  # Test r_i_1 length mismatch
  expect_error(
    compute_identified_set_components(
      matrix(1:12, 3, 4),
      matrix(1:9, 3, 3),
      lapply(1:2, function(k) matrix(1:12, 3, 4)),
      matrix(1:9, 3, 3),
      maturities = c(1, 2, 3)
    ),
    "r_i_1 must have length"
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

  # For maturity i, L_i is the inner product of gamma_i and r_i_0[,i]
  # gamma_1 = [1, 0], r_i_0[,1] = [2, 3] => L_1 = 2
  expect_equal(unname(result$L_i[1]), 2)

  # gamma_2 = [0, 1], r_i_0[,2] = [4, 5] => L_2 = 5
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

test_that(
  "components handles subset of maturities",
  {
    set.seed(123)
    J <- 3
    I <- 4
    maturities <- c(2, 4)
    n_mat <- length(maturities)

    gamma <- matrix(rnorm(J * I), J, I)

    r_i_0 <- matrix(rnorm(J * n_mat), J, n_mat)
    p_i_0 <- matrix(rnorm(J * n_mat), J, n_mat)
    r_i_1 <- lapply(seq_len(n_mat), function(k) {
      matrix(rnorm(J * I), J, I)
    })

    result <- compute_identified_set_components(
      gamma, r_i_0, r_i_1, p_i_0,
      maturities = maturities
    )

    expect_length(result$L_i, 2)
    expect_length(result$V_i, 2)
    expect_length(result$Q_i, 2)
    expect_named(
      result$L_i,
      paste0("maturity_", maturities)
    )
  }
)

test_that(
  "components accepts position-indexed statistics",
  {
    set.seed(42)
    J <- 3
    I <- 6
    maturities <- c(2, 4, 6)
    n_mat <- length(maturities)

    gamma <- matrix(rnorm(J * I), J, I)

    r_i_0 <- matrix(rnorm(J * n_mat), J, n_mat)
    colnames(r_i_0) <- paste0(
      "maturity_", maturities
    )
    p_i_0 <- matrix(rnorm(J * n_mat), J, n_mat)
    colnames(p_i_0) <- paste0(
      "maturity_", maturities
    )
    r_i_1 <- lapply(seq_len(n_mat), function(k) {
      matrix(rnorm(J * I), J, I)
    })
    names(r_i_1) <- paste0(
      "maturity_", maturities
    )

    result <- compute_identified_set_components(
      gamma, r_i_0, r_i_1, p_i_0,
      maturities = maturities
    )

    expect_length(result$L_i, n_mat)
    expect_named(
      result$L_i,
      paste0("maturity_", maturities)
    )
    expect_length(result$Q_i, n_mat)
    for (k in seq_len(n_mat)) {
      expect_length(result$Q_i[[k]], I)
    }
  }
)

test_that(
  "components infers maturities from named inputs",
  {
    set.seed(77)
    J <- 3
    I <- 6
    maturities <- c(2, 4, 6)
    n_mat <- length(maturities)
    mat_nms <- paste0("maturity_", maturities)

    gamma <- matrix(rnorm(J * I), J, I)

    r_i_0 <- matrix(rnorm(J * n_mat), J, n_mat)
    colnames(r_i_0) <- mat_nms
    p_i_0 <- matrix(rnorm(J * n_mat), J, n_mat)
    colnames(p_i_0) <- mat_nms
    r_i_1 <- setNames(
      lapply(seq_len(n_mat), function(k) {
        matrix(rnorm(J * I), J, I)
      }),
      mat_nms
    )

    result <- compute_identified_set_components(
      gamma, r_i_0, r_i_1, p_i_0
    )

    expect_named(
      result$L_i,
      paste0("maturity_", maturities)
    )
    expect_length(result$L_i, n_mat)
  }
)

test_that(
  "pipeline: stats -> components with subset",
  {
    set.seed(99)
    T_obs <- 50
    I <- 6
    J <- 3
    maturities <- c(2, 4)

    w1 <- rnorm(T_obs)
    w2 <- matrix(rnorm(T_obs * I), T_obs, I)
    pcs <- matrix(rnorm(T_obs * J), T_obs, J)
    gamma <- matrix(rnorm(J * I), J, I)

    vec <- compute_vector_statistics(
      w1, w2, pcs,
      maturities = maturities
    )

    result <- compute_identified_set_components(
      gamma, vec$r_i_0, vec$r_i_1, vec$p_i_0,
      maturities = maturities
    )

    expect_length(
      result$L_i, length(maturities)
    )
    expect_named(
      result$L_i,
      paste0("maturity_", maturities)
    )
  }
)
