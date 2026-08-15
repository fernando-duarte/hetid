# Pins every identified-set search control, so a value cannot drift
# without a deliberate edit here.

test_that("identified-set controls hold their documented values", {
  expect_identical(IDENTIFIED_SET_CONTROL$N_GRID, 41L)
  expect_identical(IDENTIFIED_SET_CONTROL$MAX_GROWTH, 12L)
  expect_identical(IDENTIFIED_SET_CONTROL$N_DIR, 20000L)
  expect_identical(IDENTIFIED_SET_CONTROL$DIR_SEED, 20260815L)
  expect_identical(IDENTIFIED_SET_CONTROL$N_POINTS, 5L)
  expect_identical(IDENTIFIED_SET_CONTROL$FEAS_TOL, 1e-10)
  expect_identical(IDENTIFIED_SET_CONTROL$SEARCH_LIMIT, 4096)
})

test_that("identified-set controls carry no undocumented entries", {
  expect_named(
    IDENTIFIED_SET_CONTROL,
    c(
      "N_GRID", "MAX_GROWTH", "N_DIR", "DIR_SEED", "N_POINTS", "FEAS_TOL",
      "SEARCH_LIMIT"
    )
  )
})

test_that("the recession search leaves the caller's random stream alone", {
  qs <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
  set.seed(99)
  before <- .Random.seed
  invisible(recession_direction(qs, n_dir = 100L))
  expect_identical(.Random.seed, before)
})
