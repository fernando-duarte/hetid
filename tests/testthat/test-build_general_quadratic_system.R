test_that("lambda list with wrong length is rejected", {
  sys <- make_general_test_system()
  bad <- list(matrix(1, 4, 1), matrix(1, 4, 1))
  expect_error(
    hetid:::as_lambda_list(bad, sys$moments),
    class = "hetid_error_bad_argument"
  )
})

test_that("lambda element with wrong row count is rejected", {
  sys <- make_general_test_system()
  bad <- list(matrix(1, 3, 1), matrix(1, 4, 1), matrix(1, 4, 1))
  expect_error(
    hetid:::as_lambda_list(bad, sys$moments),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("missing weights at a constrained column are rejected", {
  sys <- make_general_test_system(maturities = c(1, 3))
  bad <- list(matrix(1, 4, 2), NULL, NULL)
  expect_error(
    hetid:::as_lambda_list(bad, sys$moments),
    class = "hetid_error_bad_argument"
  )
})

test_that("ambiguous flat tau vectors are rejected", {
  sys <- make_general_test_system()
  lambda <- lapply(1:3, function(i) matrix(rnorm(8), 4, 2))
  expect_error(
    hetid:::as_tau_list(c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1), lambda, sys$moments),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("tau outside the unit interval is rejected in list form", {
  sys <- make_general_test_system()
  lambda <- lapply(1:3, function(i) matrix(rnorm(4), 4, 1))
  expect_error(
    hetid:::as_tau_list(list(0.2, 1.0, 0.2), lambda, sys$moments),
    class = "hetid_error_bad_argument"
  )
})

test_that("matrix-shaped tau is rejected", {
  sys <- make_general_test_system()
  lambda <- lapply(1:3, function(i) matrix(rnorm(4), 4, 1))
  expect_error(
    hetid:::as_tau_list(matrix(0.1, 2, 3), lambda, sys$moments),
    class = "hetid_error_bad_argument"
  )
})

test_that("constraint labels follow the combo naming convention", {
  sys <- make_general_test_system()
  lambda <- list(
    matrix(rnorm(4), 4, 1), matrix(rnorm(8), 4, 2), matrix(rnorm(4), 4, 1)
  )
  labels <- hetid:::general_constraint_labels(
    lambda, attr(sys$moments, "maturities")
  )
  expect_identical(
    labels$name,
    c(
      "maturity_1", "maturity_2_combo_1", "maturity_2_combo_2",
      "maturity_3"
    )
  )
  expect_identical(labels$constraint, seq_len(4L))
})

test_that("all-zero weight columns are rejected", {
  sys <- make_general_test_system()
  bad <- list(
    matrix(rnorm(4), 4, 1), matrix(0, 4, 1), matrix(rnorm(4), 4, 1)
  )
  expect_error(
    hetid:::as_lambda_list(bad, sys$moments),
    class = "hetid_error_bad_argument"
  )
  bad_mat <- matrix(rnorm(12), 4, 3)
  bad_mat[, 2] <- 0
  expect_error(
    hetid:::as_lambda_list(bad_mat, sys$moments),
    class = "hetid_error_bad_argument"
  )
})

test_that("weights at unconstrained system columns are rejected", {
  sys <- make_general_test_system(maturities = c(1, 3))
  bad <- list(
    matrix(rnorm(4), 4, 1), matrix(rnorm(4), 4, 1), matrix(rnorm(4), 4, 1)
  )
  expect_error(
    hetid:::as_lambda_list(bad, sys$moments),
    class = "hetid_error_bad_argument"
  )
})

test_that("slacks at unconstrained system columns are rejected", {
  sys <- make_general_test_system(maturities = c(1, 3))
  lambda <- list(matrix(rnorm(4), 4, 1), NULL, matrix(rnorm(4), 4, 1))
  expect_error(
    hetid:::as_tau_list(
      list(0.1, 0.1, 0.1), hetid:::as_lambda_list(lambda, sys$moments),
      sys$moments
    ),
    class = "hetid_error_bad_argument"
  )
})

test_that("matrix lambda reproduces the legacy quadratic system exactly", {
  sys <- make_general_test_system()
  gamma <- matrix(rnorm(12), nrow = 4, ncol = 3)
  tau <- c(0, 0.1, 0.3)

  legacy <- build_quadratic_system(gamma, tau, sys$moments)
  general <- build_general_quadratic_system(gamma, tau, sys$moments)

  expect_identical(general$quadratic, legacy$quadratic)
  expect_identical(general$components$L_i, legacy$components$L_i)
  expect_identical(general$components$V_i, legacy$components$V_i)
  expect_identical(general$components$Q_i, legacy$components$Q_i)
})

test_that("a list of single columns equals the matrix input bitwise", {
  sys <- make_general_test_system()
  gamma <- matrix(rnorm(12), nrow = 4, ncol = 3)
  tau <- c(0.05, 0.1, 0.2)
  as_list <- lapply(seq_len(3), function(i) gamma[, i, drop = FALSE])

  expect_identical(
    build_general_quadratic_system(as_list, tau, sys$moments),
    build_general_quadratic_system(gamma, tau, sys$moments)
  )
})

test_that("scalar tau equals the replicated list form", {
  sys <- make_general_test_system()
  lambda <- lapply(seq_len(3), function(i) matrix(rnorm(8), 4, 2))
  expect_identical(
    build_general_quadratic_system(lambda, 0.2, sys$moments),
    build_general_quadratic_system(
      lambda, list(rep(0.2, 2), rep(0.2, 2), rep(0.2, 2)), sys$moments
    )
  )
})

test_that("separate_instruments_lambda reproduces every basis-vector constraint", {
  sys <- make_general_test_system()
  j_dim <- 4
  i_dim <- 3
  tau_mat <- matrix(runif(j_dim * i_dim, 0, 0.5), j_dim, i_dim)

  lambda <- separate_instruments_lambda(sys$moments)
  tau_list <- lapply(seq_len(i_dim), function(i) tau_mat[, i])
  general <- build_general_quadratic_system(lambda, tau_list, sys$moments)

  for (j in seq_len(j_dim)) {
    basis_gamma <- matrix(0, j_dim, i_dim)
    basis_gamma[j, ] <- 1
    legacy_j <- build_quadratic_system(basis_gamma, tau_mat[j, ], sys$moments)
    for (i in seq_len(i_dim)) {
      pos <- general$labels$constraint[
        general$labels$maturity == i & general$labels$combo == j
      ]
      expect_identical(
        general$quadratic$A_i[[pos]], legacy_j$quadratic$A_i[[i]]
      )
      expect_identical(
        general$quadratic$b_i[[pos]], legacy_j$quadratic$b_i[[i]]
      )
      expect_identical(
        unname(general$quadratic$c_i[pos]),
        unname(legacy_j$quadratic$c_i[i])
      )
      expect_identical(
        unname(general$quadratic$d_i[pos]),
        unname(legacy_j$quadratic$d_i[i])
      )
    }
  }
})

test_that("scaling a weight column scales its constraint by the exact square", {
  sys <- make_general_test_system()
  lambda <- lapply(seq_len(3), function(i) matrix(rnorm(8), 4, 2))
  tau <- 0.2
  base <- build_general_quadratic_system(lambda, tau, sys$moments)

  lambda2 <- lambda
  lambda2[[2]][, 1] <- 2 * lambda2[[2]][, 1]
  scaled <- build_general_quadratic_system(lambda2, tau, sys$moments)

  pos <- base$labels$constraint[
    base$labels$maturity == 2 & base$labels$combo == 1
  ]
  expect_identical(scaled$quadratic$A_i[[pos]], 4 * base$quadratic$A_i[[pos]])
  expect_identical(scaled$quadratic$b_i[[pos]], 4 * base$quadratic$b_i[[pos]])
  expect_identical(
    unname(scaled$quadratic$c_i[pos]), unname(4 * base$quadratic$c_i[pos])
  )
  untouched <- base$labels$constraint[base$labels$maturity != 2]
  expect_identical(
    scaled$quadratic$A_i[untouched], base$quadratic$A_i[untouched]
  )
})

test_that("subset-maturity containers carry only the constrained components", {
  sys <- make_general_test_system(maturities = c(1, 3))
  lambda <- list(matrix(rnorm(8), 4, 2), NULL, matrix(rnorm(4), 4, 1))
  out <- build_general_quadratic_system(lambda, 0.1, sys$moments)
  expect_identical(out$labels$maturity, c(1L, 1L, 3L))
  expect_identical(length(out$quadratic$A_i), 3L)
})
