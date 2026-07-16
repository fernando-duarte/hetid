# Synthetic inputs for the general-instrument tests. Deterministic via
# an explicit seed so equivalence assertions are stable
make_general_test_system <- function(seed = 42, n_obs = 80,
                                     i_dim = 3, j_dim = 4,
                                     maturities = NULL) {
  set.seed(seed)
  w1 <- rnorm(n_obs)
  w2 <- matrix(rnorm(n_obs * i_dim), nrow = n_obs)
  z <- matrix(
    rnorm(n_obs * j_dim),
    nrow = n_obs,
    dimnames = list(NULL, paste0("z", seq_len(j_dim)))
  )
  list(
    w1 = w1, w2 = w2, z = z,
    moments = compute_identification_moments(
      w1, w2, z,
      maturities = maturities
    )
  )
}
