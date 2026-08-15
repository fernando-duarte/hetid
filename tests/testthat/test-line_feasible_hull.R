# Exact feasible hull along one line: the analytic case table, the
# multi-exclusion intersection a running-interval scheme gets wrong, and
# root stability under cancellation.

# One-dimensional systems, so center = 0 and dir = 1 make the univariate
# coefficients (a, beta, gamma) exactly the rows supplied here.
make_line_system <- function(rows) {
  rows <- rbind(rows)
  list(
    A_i = lapply(seq_len(nrow(rows)), function(i) {
      matrix(rows[i, 1], nrow = 1, ncol = 1)
    }),
    b_i = lapply(seq_len(nrow(rows)), function(i) rows[i, 2]),
    c_i = rows[, 3]
  )
}

hull_of <- function(rows, tol = 1e-10) {
  line_feasible_hull(0, 1, make_line_system(rows), tol)
}

test_that("a positive leading coefficient keeps the interval between roots", {
  expect_equal(hull_of(c(1, 0, -1)), c(-1, 1), tolerance = 1e-12)
})

test_that("a positive leading coefficient with no real root is infeasible", {
  expect_null(hull_of(c(1, 0, 1)))
})

test_that("a tangency point is dropped rather than reported", {
  # t^2 <= 0 touches zero at a single point; a zero-width cell has no
  # interior probe, and the loss is inside the caller's grid error
  expect_null(hull_of(c(1, 0, 0)))
})

test_that("a negative leading coefficient excludes the interval between roots", {
  expect_identical(hull_of(c(-1, 0, 1)), c(-Inf, Inf))
})

test_that("a concave constraint with no real root leaves the line feasible", {
  expect_identical(hull_of(c(-1, 0, -1)), c(-Inf, Inf))
})

test_that("a concave constraint touching zero leaves the line feasible", {
  expect_identical(hull_of(c(-1, 0, 0)), c(-Inf, Inf))
})

test_that("a vanishing leading coefficient gives the linear half-line", {
  expect_identical(hull_of(c(0, 1, -1)), c(-Inf, 1))
  expect_identical(hull_of(c(0, -1, -1)), c(-1, Inf))
})

test_that("a constant constraint is decided by its sign alone", {
  expect_identical(hull_of(c(0, 0, -1)), c(-Inf, Inf))
  expect_null(hull_of(c(0, 0, 1)))
})

test_that("two exclusions intersect correctly rather than by running trims", {
  # [0, 10] minus (3, 7) minus (-1, 4) is [7, 10]. A scheme that trims a
  # running [lower, upper] instead reports 4, which is infeasible.
  rows <- rbind(
    c(1, -10, 0), # t(t - 10) <= 0    keeps [0, 10]
    c(-1, 10, -21), # -(t-3)(t-7) <= 0 excludes (3, 7)
    c(-1, 3, 4) # -(t+1)(t-4) <= 0 excludes (-1, 4)
  )
  hull <- hull_of(rows)
  expect_equal(hull, c(7, 10), tolerance = 1e-12)
  expect_true(all(c(1, -10, 0) %*% c(hull[1]^2, hull[1], 1) <= 1e-10))
})

test_that("roots stay accurate when the discriminant nearly cancels", {
  # t^2 - 1e8 t + 1 <= 0 has roots about 1e-8 and 1e8; the textbook
  # formula loses the small one to cancellation and returns 0
  hull <- hull_of(c(1, -1e8, 1))
  expect_equal(hull[1], 2 / (1e8 + sqrt(1e16 - 4)), tolerance = 1e-18)
  expect_gt(hull[1], 0)
  expect_equal(hull[2], (1e8 + sqrt(1e16 - 4)) / 2, tolerance = 1e-6)
})

test_that("the hull matches a dense scan of the same constraints", {
  rows <- rbind(c(1, -10, 0), c(-1, 10, -21), c(-1, 3, 4))
  qs <- make_line_system(rows)
  grid <- seq(-5, 15, by = 0.001)
  values <- vapply(grid, function(t) {
    max(rows[, 1] * t^2 + rows[, 2] * t + rows[, 3])
  }, numeric(1))
  feasible <- grid[values <= 1e-10]
  hull <- line_feasible_hull(0, 1, qs, 1e-10)
  expect_lte(hull[1], min(feasible))
  expect_gte(hull[2], max(feasible))
  expect_equal(hull[1], min(feasible), tolerance = 1e-3)
  expect_equal(hull[2], max(feasible), tolerance = 1e-3)
})

test_that("a line through a two-dimensional system is solved exactly", {
  # unit disc: theta'theta - 1 <= 0, probed along the first axis
  qs <- list(
    A_i = list(diag(2)),
    b_i = list(c(0, 0)),
    c_i = -1
  )
  expect_equal(
    line_feasible_hull(c(0, 0), c(1, 0), qs, 1e-10),
    c(-1, 1),
    tolerance = 1e-12
  )
  # offset line: at theta_2 = 0.6 the disc gives |theta_1| <= 0.8
  expect_equal(
    line_feasible_hull(c(0, 0.6), c(1, 0), qs, 1e-10),
    c(-0.8, 0.8),
    tolerance = 1e-12
  )
})
