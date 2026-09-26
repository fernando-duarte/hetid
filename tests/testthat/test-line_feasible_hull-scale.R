# Analytic sets whose classification must not depend on constraint units
scale_hull <- function(rows) {
  rows <- rbind(rows)
  qs <- list(
    A_i = lapply(seq_len(nrow(rows)), function(i) matrix(rows[i, 1], 1L, 1L)),
    b_i = lapply(seq_len(nrow(rows)), function(i) rows[i, 2]),
    c_i = rows[, 3]
  )
  line_feasible_hull(0, 1, qs)
}

test_that("tiny positive curvature still gives finite endpoints", {
  expect_equal(scale_hull(c(1e-24, 0, -1)), c(-1e12, 1e12))
  expect_equal(scale_hull(c(1e-24, 0, -1e-14)), c(-1e5, 1e5))
})

test_that("a tiny violation cannot make an empty line unbounded", {
  expect_null(scale_hull(c(1, 0, 1e-11)))
  expect_null(scale_hull(c(0, 0, 1e-14)))
  expect_null(scale_hull(rbind(c(0, 1, -1), c(0, -1, 1 + 3e-10))))
})

test_that("linear roots beyond unit floating point spacing stay one sided", {
  expect_equal(scale_hull(c(0, 1e-30, -1)), c(-Inf, 1e30))
  expect_equal(scale_hull(c(0, -1e-30, -1)), c(-1e30, Inf))
})

test_that("true tails and free directions survive small coefficients", {
  expect_identical(scale_hull(c(-1e-24, 0, 1)), c(-Inf, Inf))
  expect_identical(scale_hull(c(0, 0, -1e-300)), c(-Inf, Inf))
  expect_identical(scale_hull(c(0, 0, 0)), c(-Inf, Inf))
  cylinder <- list(A_i = list(diag(c(1, 0))), b_i = list(c(0, 0)), c_i = -1)
  expect_identical(
    line_feasible_hull(c(0, 0), c(0, 1), cylinder), c(-Inf, Inf)
  )
})

test_that("positive constraint rescaling preserves bounded interval geometry", {
  exclusions <- rbind(c(1, -10, 0), c(-1, 10, -21), c(-1, 3, 4))
  for (s in c(1e-200, 1e-14, 1, 1e14, 1e200)) {
    expect_equal(scale_hull(s * c(1, 0, -1)), c(-1, 1), tolerance = 1e-12)
    expect_equal(scale_hull(s * exclusions), c(7, 10), tolerance = 1e-12)
    expect_null(scale_hull(s * c(1, 0, 0)))
    expect_null(scale_hull(s * c(1, -2, 1)))
    expect_null(scale_hull(s * c(1, 0, 1)))
  }
})

test_that("tiny curvature is not lost beside a large constant", {
  expect_equal(scale_hull(c(1e-200, 0, -1e200)), c(-1e200, 1e200))
  expect_equal(scale_hull(c(1e200, 0, -1e-200)), c(-1e-200, 1e-200),
    tolerance = 1e-210
  )
})

test_that("a finite root outside the numeric range is not an infinite bound", {
  expect_error(scale_hull(c(0, 1e-300, -1e300)), class = "hetid_error")
  expect_error(scale_hull(c(0, 1e300, -1e-300)), class = "hetid_error")
  expect_error(scale_hull(c(1e-320, 0, -1e308)), class = "hetid_error")
})

test_that("root arithmetic avoids unnecessary intermediate overflow", {
  roots <- line_quadratic_roots(matrix(c(1e-308, 2, 1e308), 1L))
  expect_true(all(is.finite(roots)))
  expect_equal(as.vector(roots) / 1e308, c(-1, -1), tolerance = 1e-7)
})

test_that("random systems preserve their hull under positive rescaling", {
  set.seed(123)
  for (i in seq_len(100L)) {
    rows <- matrix(rnorm(3L * sample(1:3, 1L)), ncol = 3L)
    reference <- scale_hull(rows)
    for (s in c(1e-200, 1e-12, 1e12, 1e200)) {
      actual <- scale_hull(s * rows)
      expect_identical(is.null(actual), is.null(reference))
      if (is.null(actual) || is.null(reference)) next
      expect_identical(is.infinite(actual), is.infinite(reference))
      finite <- is.finite(reference)
      expect_identical(actual[!finite], reference[!finite])
      expect_equal(actual[finite], reference[finite], tolerance = 1e-12)
    }
  }
})
