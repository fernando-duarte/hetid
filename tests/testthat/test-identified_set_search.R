# The box-search kernel on hand-built quadratic systems: an ellipsoid whose
# extremes are known in closed form, a zero objective, a system with a
# recession direction, and the one-component case.

# one ellipsoid theta' A theta + b' theta + c <= 0 with A positive definite;
# the exact extremes of l' theta are l' mu +/- sqrt(r2 * l' A^{-1} l)
ellipsoid <- function() {
  a_mat <- matrix(c(2, 0.5, 0, 0.5, 1, 0.3, 0, 0.3, 3), 3, 3)
  b_vec <- c(0.4, -0.2, 0.1)
  mu <- drop(-solve(a_mat, b_vec)) / 2
  list(
    quadratic = list(A_i = list(a_mat), b_i = list(b_vec), c_i = -1),
    center = mu,
    radius2 = drop(crossprod(mu, a_mat %*% mu)) + 1,
    a_inv = solve(a_mat)
  )
}

exact_extremes <- function(shape, objectives) {
  spread <- sqrt(shape$radius2 * colSums(objectives * (shape$a_inv %*% objectives)))
  at_center <- drop(crossprod(objectives, shape$center))
  list(lower = at_center - spread, upper = at_center + spread)
}

test_that("the sweep recovers closed-form extremes on an ellipsoid", {
  shape <- ellipsoid()
  objectives <- cbind(diag(3), c(1, 1, 1) / sqrt(3), c(0, 0, 0))
  found <- apply_recession_bounds(
    identified_set_search(shape$center, diag(3), shape$quadratic, 41L, objectives),
    shape$quadratic, objectives
  )
  exact <- exact_extremes(shape, objectives)
  # inner approximation: never beyond the truth, and close at this grid
  expect_true(all(found$lower >= exact$lower - 1e-9))
  expect_true(all(found$upper <= exact$upper + 1e-9))
  expect_true(all(found$lower <= exact$lower + 2e-2))
  expect_true(all(found$upper >= exact$upper - 2e-2))
  # the zero objective is the point 0, attained at the centre
  expect_identical(found$lower[5L], 0)
  expect_identical(found$upper[5L], 0)
  expect_equal(found$arg_lower[5L, ], shape$center)
})

test_that("refining the grid tightens the ellipsoid extremes", {
  shape <- ellipsoid()
  objectives <- cbind(diag(3), c(1, -1, 0.5))
  coarse <- identified_set_search(shape$center, diag(3), shape$quadratic, 21L, objectives)
  fine <- identified_set_search(shape$center, diag(3), shape$quadratic, 81L, objectives)
  exact <- exact_extremes(shape, objectives)
  expect_true(all(fine$upper >= coarse$upper - 1e-12))
  expect_true(all(fine$lower <= coarse$lower + 1e-12))
  # the loss is second order in the grid spacing (0.05 here), so 1e-2 is loose
  expect_true(all(abs(fine$upper - exact$upper) < 1e-2))
})

test_that("a recession direction makes every non-zero objective infinite", {
  # theta' theta >= 1: the outside of the unit ball, unbounded in every
  # direction, with a zero objective that must stay the constant it is
  quadratic <- list(A_i = list(-diag(3)), b_i = list(rep(0, 3)), c_i = 1)
  center <- c(2, 0, 0)
  objectives <- cbind(diag(3), c(0, 1, 1), c(0, 0, 0))
  found <- apply_recession_bounds(
    identified_set_search(center, diag(3), quadratic, 11L, objectives),
    quadratic, objectives
  )
  expect_true(all(found$lower[1:4] == -Inf))
  expect_true(all(found$upper[1:4] == Inf))
  expect_true(all(is.na(found$arg_lower[1:4, ])))
  expect_identical(found$lower[5L], 0)
  expect_identical(found$upper[5L], 0)
  expect_equal(found$arg_upper[5L, ], center)
})

test_that("a single component needs no gridded plane", {
  # theta^2 <= 1 on the line: the free coordinate is solved exactly
  quadratic <- list(A_i = list(matrix(1, 1, 1)), b_i = list(0), c_i = -1)
  objectives <- matrix(c(1, 2, 0), 1, 3)
  found <- apply_recession_bounds(
    identified_set_search(0, matrix(1, 1, 1), quadratic, 3L, objectives),
    quadratic, objectives
  )
  expect_equal(found$lower, c(-1, -2, 0), tolerance = 1e-12)
  expect_equal(found$upper, c(1, 2, 0), tolerance = 1e-12)
  expect_identical(dim(found$arg_lower), c(3L, 1L))
})
