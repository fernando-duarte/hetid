outer_system <- function(a, b = rep(0, nrow(a)), constant = -1) {
  list(A_i = list(a), b_i = list(b), c_i = constant)
}

outer_mixed <- function() {
  list(
    A_i = list(
      diag(c(4, 1, 0.25)),
      rbind(c(1, 0.2, 0), c(0.2, -0.5, 0.1), c(0, 0.1, -0.3))
    ),
    b_i = list(c(-8, 0, 0), c(0, 0, 0)),
    c_i = c(3, -2)
  )
}

outer_fixture <- function(name) {
  folder <- test_path("fixtures")
  rows <- read.csv(file.path(folder, paste0(name, "-quadratic.csv")))
  n <- max(rows$row)
  ids <- sort(unique(rows$constraint))
  a <- lapply(ids, function(i) {
    out <- matrix(0, n, n)
    part <- rows[rows$constraint == i & rows$kind == "A", ]
    out[cbind(part$row, part$column)] <- part$value
    out
  })
  b <- lapply(ids, function(i) {
    part <- rows[rows$constraint == i & rows$kind == "b", ]
    out <- numeric(n)
    out[part$row] <- part$value
    out
  })
  c0 <- vapply(ids, function(i) rows$value[rows$constraint == i & rows$kind == "c"], 0)
  list(
    quadratic = list(A_i = a, b_i = b, c_i = c0),
    attained = read.csv(file.path(folder, paste0(name, "-attained.csv")))
  )
}

test_that("outer bounds are exact on the unit ball and keep zero loadings exact", {
  out <- compute_quadratic_set_evidence(outer_system(diag(3)), diag(3), n_dir = 0)
  bounds <- out$outer_bounds(cbind(diag(3), 0, c(1, 1, 0)))
  expect_true(all(bounds$lower[1:3] <= -1) && all(bounds$upper[1:3] >= 1))
  expect_equal(bounds$upper[1:3], rep(1, 3), tolerance = 1e-12)
  expect_identical(c(bounds$lower[4], bounds$upper[4]), c(0, 0))
  expect_equal(bounds$upper[5], sqrt(2), tolerance = 1e-12)
  expect_false(attr(bounds, "empty"))
})

test_that("outer bounds contain sampled members and refinement only tightens", {
  sys <- outer_mixed()
  out <- compute_quadratic_set_evidence(sys, diag(3), n_dir = 0)
  common <- out$outer_bounds(diag(3), refine = FALSE)
  refined <- out$outer_bounds(diag(3), refine = TRUE)
  expect_true(all(refined$lower >= common$lower) && all(refined$upper <= common$upper))
  set.seed(1)
  pts <- matrix(runif(3e5, -6, 6), ncol = 3)
  values <- sapply(seq_along(sys$A_i), function(i) {
    rowSums((pts %*% sys$A_i[[i]]) * pts) + drop(pts %*% sys$b_i[[i]]) + sys$c_i[i]
  })
  inside <- pts[apply(values <= 0, 1, all), , drop = FALSE]
  expect_gt(nrow(inside), 100)
  expect_true(all(sweep(inside, 2, refined$lower) >= 0))
  expect_true(all(sweep(inside, 2, refined$upper) <= 0))
})

test_that("row rescaling leaves bounds unchanged and objective scaling scales them", {
  sys <- outer_mixed()
  base <- compute_quadratic_set_evidence(sys, diag(3), n_dir = 0)$outer_bounds(diag(3))
  for (s in c(2^-600, 1e-150, 1e150, 2^600)) {
    scaled <- list(
      A_i = lapply(sys$A_i, `*`, s), b_i = lapply(sys$b_i, `*`, s),
      c_i = sys$c_i * s
    )
    got <- compute_quadratic_set_evidence(scaled, diag(3), n_dir = 0)$outer_bounds(diag(3))
    expect_equal(got$upper, base$upper, tolerance = 1e-9)
    expect_equal(got$lower, base$lower, tolerance = 1e-9)
  }
  out <- compute_quadratic_set_evidence(sys, diag(3), n_dir = 0)
  for (s in c(1e-200, 1e200)) {
    got <- out$outer_bounds(diag(3) * s)
    expect_equal(got$upper / s, base$upper, tolerance = 1e-9)
  }
  # x3 ranges over about [-2, 2], so its bound times 1e308 overflows: unknown,
  # never an infinite or wrapped value
  huge <- out$outer_bounds(diag(3) * 1e308)
  expect_false(any(is.infinite(c(huge$lower, huge$upper))))
  expect_true(is.na(huge$upper[3]) && is.na(huge$lower[3]))
  expect_true(is.finite(huge$upper[1]))
})

test_that("bounds are unknown without a verified certificate", {
  outside <- outer_system(-diag(2), constant = 1)
  out <- compute_quadratic_set_evidence(outside, diag(2), n_dir = 0)
  bounds <- out$outer_bounds(cbind(diag(2), 0))
  expect_true(all(is.na(bounds$lower[1:2])) && all(is.na(bounds$upper[1:2])))
  expect_identical(c(bounds$lower[3], bounds$upper[3]), c(0, 0))
  expect_match(attr(bounds, "reason"), "no positive definite certificate")
  lossy <- outer_system(diag(c(1e300, 1e-300)))
  bounder <- quadratic_outer_bounder(lossy,
    list(weights = 1, scales = 1e300, matrix = diag(2)),
    maxit = 10L
  )
  expect_match(attr(bounder(diag(2)), "reason"), "row rescaling")
})

test_that("verified emptiness is reported and conflicts with a checked point", {
  empty <- outer_system(diag(2), constant = 1)
  certificate <- list(weights = 1, scales = 1)
  bounds <- quadratic_outer_bounder(empty, certificate)(diag(2))
  expect_true(attr(bounds, "empty"))
  expect_true(all(is.na(bounds$lower)))
  expect_error(quadratic_outer_bounder(empty, certificate, nonempty = TRUE),
    class = "hetid_error"
  )
})

test_that("loadings, flags and pools are validated with structured errors", {
  out <- compute_quadratic_set_evidence(outer_system(diag(2)), diag(2), n_dir = 0)
  expect_error(out$outer_bounds(c(1, 0)), class = "hetid_error")
  expect_error(out$outer_bounds(matrix(1, 3, 1)), class = "hetid_error")
  expect_error(out$outer_bounds(matrix(c(1, NA), 2)), class = "hetid_error")
  expect_error(out$outer_bounds(matrix(c(1i, 0), 2)), class = "hetid_error")
  expect_error(out$outer_bounds(diag(2), refine = NA), class = "hetid_error")
  expect_error(out$outer_bounds(diag(2), pool = list("a")), class = "hetid_error")
  expect_error(out$outer_bounds(diag(2), pool = c(0.5, 0.5)), class = "hetid_error")
  expect_error(out$outer_bounds(diag(2), pool = list(c(1i, 0))), class = "hetid_error")
  expect_error(out$outer_bounds(matrix(1i, 2, 2)), class = "hetid_error")
  expect_error(out$outer_bounds(matrix(c(1, Inf), 2)), class = "hetid_error")
  expect_error(out$outer_bounds(matrix(numeric(), 2, 0)), class = "hetid_error")
})

test_that("caller pools are verified again, never trusted", {
  sys <- outer_mixed()
  out <- compute_quadratic_set_evidence(sys, diag(3), n_dir = 0)
  refined <- out$outer_bounds(diag(3), refine = TRUE)
  reused <- out$outer_bounds(diag(3), refine = FALSE, pool = attr(refined, "pool"))
  expect_equal(reused$upper, refined$upper)
  junk <- list(c(-1, 2), c(1, 0, 0), c(0, 1))
  guarded <- out$outer_bounds(diag(3), refine = FALSE, pool = junk)
  common <- out$outer_bounds(diag(3), refine = FALSE)
  expect_identical(guarded$upper, common$upper)
  huge <- out$outer_bounds(diag(3), refine = FALSE, pool = list(c(1e308, 1e308)))
  ordinary <- out$outer_bounds(diag(3), refine = FALSE, pool = list(c(1, 1)))
  expect_equal(huge$upper, ordinary$upper)
  expect_equal(huge$lower, ordinary$lower)
  expect_error(out$outer_bounds(diag(3), pool = list(matrix(c(1, 0), 1))),
    class = "hetid_error"
  )
})

test_that("the outer search consumes and restores no random numbers", {
  sys <- outer_mixed()
  out <- compute_quadratic_set_evidence(sys, diag(3), n_dir = 0)
  set.seed(7)
  before <- .Random.seed
  out$outer_bounds(diag(3), refine = TRUE)
  expect_identical(.Random.seed, before)
  rm(".Random.seed", envir = globalenv())
  out$outer_bounds(diag(3), refine = TRUE)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("real bounded systems keep checked points inside tight containing bounds", {
  for (name in c("fullB-tau020", "B1566-tau020")) {
    fixture <- outer_fixture(name)
    out <- compute_quadratic_set_evidence(fixture$quadratic, diag(3), n_dir = 0)
    expect_false(is.null(out$boundedness))
    bounds <- out$outer_bounds(diag(3), refine = TRUE)
    expect_true(all(is.finite(c(bounds$lower, bounds$upper))))
    for (i in seq_len(nrow(out$feasible_points))) {
      point <- out$feasible_points[i, ]
      expect_true(all(point >= bounds$lower & point <= bounds$upper))
    }
    width <- fixture$attained$upper - fixture$attained$lower
    slack <- 1e-7 * pmax(1, abs(c(fixture$attained$lower, fixture$attained$upper)))
    expect_true(all(bounds$lower <= fixture$attained$lower + slack[1:3]))
    expect_true(all(bounds$upper >= fixture$attained$upper - slack[4:6]))
    expect_true(all((bounds$upper - bounds$lower) / width <= 1.02))
  }
})
