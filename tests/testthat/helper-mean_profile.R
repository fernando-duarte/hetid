mean_profile_fixture <- function(seed = 3L) {
  with_rng_scope(
    {
      n <- 220L
      z <- rnorm(n)
      x <- stats::prcomp(matrix(rnorm(n * 5L), n, 5L))$x[, 1:3]
      colnames(x) <- paste0("expected_sdf_pc", 1:3)
      y2 <- stats::prcomp(sqrt(exp(0.4 + 0.9 * z)) * matrix(rnorm(n * 5L), n, 5L))$x[, 1:3]
      colnames(y2) <- paste0("sdf_news_pc", 1:3)
      e1 <- rnorm(n) + 0.4 * y2[, 1L] - 0.2 * y2[, 3L]
      y <- drop(0.3 + x %*% c(0.2, -0.1, 0.4) + y2 %*% c(0.5, -0.3, 0.2) + e1)
      z <- matrix(z, ncol = 1L, dimnames = list(NULL, "z"))
      compute_tau0_system(y, y2, x, z,
        impose_null = FALSE,
        gamma = matrix(1, 1, ncol(y2)), tol = 1e-8
      )
    },
    seed = seed,
    kind = c("Mersenne-Twister", "Inversion", "Rejection")
  )
}

mean_profile_ball <- function(dimension = 2L) {
  list(A_i = list(diag(dimension)), b_i = list(numeric(dimension)), c_i = -1)
}
