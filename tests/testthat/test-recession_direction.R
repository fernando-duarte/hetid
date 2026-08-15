# Recession-direction search: a witness proves the set unbounded, and a
# bounded set must yield none.

test_that("a bounded set has no recession direction", {
  # unit ball: A = I, so v'Av = 1 > 0 in every direction
  qs <- list(A_i = list(diag(3)), b_i = list(rep(0, 3)), c_i = -1)
  set.seed(42)
  expect_null(recession_direction(qs, n_dir = 2000L))
})

test_that("a witness satisfies the criterion it claims", {
  # A = -I sends every constraint to -Inf along any direction
  qs <- list(A_i = list(-diag(3)), b_i = list(rep(0, 3)), c_i = -1)
  set.seed(42)
  witness <- recession_direction(qs, n_dir = 2000L)
  expect_length(witness, 3L)
  expect_equal(sum(witness^2), 1, tolerance = 1e-12)
  expect_lt(drop(witness %*% qs$A_i[[1]] %*% witness), 0)
})

test_that("a direction must beat every constraint, not just one", {
  # one constraint is negative only along axis 1, the other only along
  # axis 2, so no single direction satisfies both
  a_one <- diag(c(-1, 1, 1)) # nolint: object_name_linter.
  a_two <- diag(c(1, -1, 1)) # nolint: object_name_linter.
  qs <- list(
    A_i = list(a_one, a_two),
    b_i = list(rep(0, 3), rep(0, 3)),
    c_i = c(-1, -1)
  )
  set.seed(42)
  expect_null(recession_direction(qs, n_dir = 5000L))
})

test_that("the witness is checked against every constraint", {
  # both constraints are negative-definite in the first two coordinates
  a_one <- diag(c(-1, -1, 1)) # nolint: object_name_linter.
  a_two <- diag(c(-2, -1, 1)) # nolint: object_name_linter.
  qs <- list(
    A_i = list(a_one, a_two),
    b_i = list(rep(0, 3), rep(0, 3)),
    c_i = c(-1, -1)
  )
  set.seed(42)
  witness <- recession_direction(qs, n_dir = 5000L)
  expect_false(is.null(witness))
  for (a_mat in qs$A_i) {
    expect_lt(drop(witness %*% a_mat %*% witness), 0)
  }
})
