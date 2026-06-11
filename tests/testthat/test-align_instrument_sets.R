test_that("sets unite by name in first-appearance order", {
  t_obs <- 12
  z <- matrix(
    rnorm(t_obs * 3), t_obs,
    dimnames = list(NULL, c("a", "b", "c"))
  )
  out <- align_instrument_sets(
    list(z[, c("a", "b")], z[, c("b", "c")]),
    n_components = 2
  )
  expect_identical(colnames(out$instruments), c("a", "b", "c"))
  expect_identical(
    out$instruments,
    matrix(
      c(z[, "a"], z[, "b"], z[, "c"]), t_obs,
      dimnames = list(NULL, c("a", "b", "c"))
    )
  )
  expect_identical(out$support, list(c(1L, 2L), c(2L, 3L)))
})

test_that("conflicting same-name columns are rejected", {
  z_one <- matrix(
    as.numeric(seq_len(6)) / 2, 3, 2,
    dimnames = list(NULL, c("a", "b"))
  )
  z_two <- z_one[, "a", drop = FALSE]
  z_two[1, "a"] <- 99
  expect_error(
    align_instrument_sets(list(z_one, z_two), 2),
    class = "hetid_error_bad_argument"
  )
})

test_that("unnamed or duplicate-named columns are rejected", {
  expect_error(
    align_instrument_sets(list(matrix(rnorm(6), 3, 2)), 1),
    class = "hetid_error_bad_argument"
  )
  z_dup <- matrix(
    rnorm(6), 3, 2,
    dimnames = list(NULL, c("a", "a"))
  )
  expect_error(
    align_instrument_sets(list(z_dup), 1),
    class = "hetid_error_bad_argument"
  )
})

test_that("row-count mismatches across sets are rejected", {
  z_one <- matrix(rnorm(6), 3, 2, dimnames = list(NULL, c("a", "b")))
  z_two <- matrix(rnorm(8), 4, 2, dimnames = list(NULL, c("c", "d")))
  expect_error(
    align_instrument_sets(list(z_one, z_two), 2),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("maturities control which columns carry sets", {
  z_all <- matrix(
    rnorm(9), 3, 3,
    dimnames = list(NULL, c("a", "b", "c"))
  )
  out <- align_instrument_sets(
    list(z_all, NULL, z_all[, "c", drop = FALSE]),
    n_components = 3, maturities = c(1, 3)
  )
  expect_null(out$support[[2]])
  expect_identical(out$support[[3]], 3L)
  expect_error(
    align_instrument_sets(
      list(z_all, z_all, NULL),
      n_components = 3, maturities = c(1, 3)
    ),
    class = "hetid_error_bad_argument"
  )
})

test_that("invalid maturities and counts are rejected", {
  z_one <- matrix(rnorm(6), 3, 2, dimnames = list(NULL, c("a", "b")))
  expect_error(
    align_instrument_sets(list(z_one), 0),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    align_instrument_sets(list(z_one), 1, maturities = 2),
    class = "hetid_error_bad_argument"
  )
})

test_that("data frames are accepted and non-finite values rejected", {
  df <- data.frame(a = rnorm(5), b = rnorm(5))
  out <- align_instrument_sets(list(df), 1)
  expect_identical(colnames(out$instruments), c("a", "b"))
  df_bad <- df
  df_bad$a[2] <- NA
  expect_error(
    align_instrument_sets(list(df_bad), 1),
    class = "hetid_error_bad_argument"
  )
})

test_that("aligned output drives the full masked workflow", {
  sys <- make_general_test_system(i_dim = 2, j_dim = 4)
  aligned <- align_instrument_sets(
    list(sys$z[, c("z1", "z2")], sys$z[, c("z2", "z4")]),
    n_components = 2
  )
  expect_identical(
    colnames(aligned$instruments), c("z1", "z2", "z4")
  )
  moments <- compute_identification_moments(
    sys$w1, sys$w2, aligned$instruments
  )
  lambda <- lambda_from_support(
    aligned$support,
    list(matrix(c(1, 1), 2, 1), matrix(c(1, -1), 2, 1)),
    j_total = ncol(aligned$instruments)
  )
  qs <- build_general_quadratic_system(lambda, 0.2, moments)
  expect_identical(nrow(qs$labels), 2L)
})
