{
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
}
