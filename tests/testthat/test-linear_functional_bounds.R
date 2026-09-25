test_that("scalar functionals have analytic endpoints, offsets and witnesses", {
  d <- simulate_tau0_dgp()
  fit <- compute_tau0_system(d$y1, d$y2[, 1, drop = FALSE], d$x, d$z)
  obj <- matrix(c(1, -2, 0), 1, dimnames = list("news1", c("first", "negative", "flat")))
  out <- compute_linear_functional_bounds(fit, 0.01, obj, c(2, -1, 7), n_grid = 3)
  q <- out$quadratic
  a <- q$A_i[[1]][1, 1]
  b <- q$b_i[[1]][1]
  cc <- q$c_i[1]
  expect_gt(a, 0)
  roots <- sort((-b + c(-1, 1) * sqrt(b^2 - 4 * a * cc)) / (2 * a))
  expect_equal(out$bounds$lower, c(2 + roots[1], -1 - 2 * roots[2], 7))
  expect_equal(out$bounds$upper, c(2 + roots[2], -1 - 2 * roots[1], 7))
  expect_identical(dim(out$arg_lower), c(3L, 1L))
  expect_s3_class(out, "hetid_functional_bounds")
  expect_identical(out$objectives, obj)
  expect_identical(names(out$offsets), colnames(obj))
  expect_true(all(vapply(out$evidence_lower, is.null, logical(1))))
  expect_equal(out$arg_upper[3, ], out$center, ignore_attr = TRUE)
})

test_that("functional axes and fitted-system domain fail explicitly", {
  fit <- box_fit()
  obj <- matrix(1, 3, 1, dimnames = list(colnames(fit$w2), "sum"))
  expect_error(compute_linear_functional_bounds(fit, 0.05, obj[3:1, , drop = FALSE]),
    "theta axis",
    class = "hetid_error_bad_argument"
  )
  expect_error(compute_linear_functional_bounds(fit, 0.05, unname(obj)), "column names")
  expect_error(compute_linear_functional_bounds(fit, 0.05, obj, c(1, 2)), "offsets")
  expect_error(compute_linear_functional_bounds(fit, 0.05, obj, c(other = 1)), "offset names")
  expect_error(compute_linear_functional_bounds(fit, 0, obj), "strictly positive")
  expect_error(compute_linear_functional_bounds(fit, 1, obj), class = "hetid_error")
  expect_error(compute_linear_functional_bounds(fit, 0.05, obj, n_grid = 4), "odd")
  expect_error(compute_linear_functional_bounds(fit, 0.05, obj, max_growth = 0),
    class = "hetid_error"
  )
  expect_error(compute_linear_functional_bounds(fit, 0.05, obj, search_limit = 1),
    class = "hetid_error"
  )
  expect_error(compute_linear_functional_bounds(fit, 0.05, obj, center = matrix(0, 3)),
    "numeric vector",
    class = "hetid_error"
  )
  fit$point <- NULL
  fit["beta1"] <- list(NULL)
  expect_error(compute_linear_functional_bounds(fit, 0.05, obj), "supply center")
})

test_that("finite joint-objective values are attained without snapping small loadings", {
  fit <- box_fit()
  obj <- cbind(sum = c(1, 1, 1), cancel = c(1, -1, 0), tiny = c(1e-20, 0, 0))
  out <- compute_linear_functional_bounds(fit, 0.05, obj, n_grid = 7)
  for (side in c("lower", "upper")) {
    points <- out[[paste0("arg_", side)]]
    values <- rowSums(t(obj) * points)
    expect_equal(unname(values), out$bounds[[side]])
    expect_identical(rownames(points), colnames(obj))
    for (theta in split(points, row(points))) {
      q <- out$quadratic
      scale <- vapply(seq_along(q$c_i), function(i) {
        sum(abs(outer(theta, theta) * q$A_i[[i]])) +
          sum(abs(q$b_i[[i]] * theta)) + abs(q$c_i[i])
      }, numeric(1))
      expect_true(all(make_system_checker(q)(theta) <= IDENTIFIED_SET_CONTROL$FEAS_TOL * scale))
    }
  }
  expect_gt(out$bounds$upper[3] - out$bounds$lower[3], 0)
  expect_true(length(out$search$phases) >= 1L)
})

test_that("unbounded fitted sets retain evidence and exact constant objectives", {
  fit <- box_fit(collinear = TRUE)
  obj <- cbind(sum = c(1, 1, 1), flat = c(0, 0, 0))
  out <- compute_linear_functional_bounds(fit, 0.3, obj, c(0, 8), n_grid = 3)
  expect_identical(out$bounds$lower, c(-Inf, 8))
  expect_identical(out$bounds$upper, c(Inf, 8))
  expect_true(all(is.na(out$arg_lower[1, ])))
  expect_true(is.list(out$evidence_lower$sum))
  expect_null(out$evidence_lower$flat)
  v <- out$evidence_lower$sum$direction
  expect_true(all(vapply(out$quadratic$A_i, function(a) {
    drop(crossprod(v, a %*% v)) < 0
  }, logical(1))))
})

test_that("common response units preserve functional values and evidence classification", {
  d <- simulate_tau0_dgp()
  obj <- cbind(sum = c(1, 1), difference = c(1, -1))
  run <- function(s) {
    compute_linear_functional_bounds(
      compute_tau0_system(s * d$y1, s * d$y2, d$x, d$z), 0.05, obj,
      n_grid = 7
    )
  }
  reference <- run(1)
  for (s in c(0.001, 100)) {
    actual <- run(s)
    expect_equal(actual$bounds, reference$bounds, tolerance = 1e-9)
    expect_true(all(is.finite(actual$bounds$lower)))
  }
})

test_that("public witnesses and both evidence kinds carry the theta axis", {
  fit <- box_fit(collinear = TRUE)
  obj <- cbind(sum = c(1, 1, 1), flat = c(0, 0, 0))
  center <- stats::setNames(fit$point$theta, colnames(fit$w2))
  out <- compute_linear_functional_bounds(fit, 0.3, obj, center = center, n_grid = 3)
  expect_identical(names(out$center), colnames(fit$w2))
  expect_identical(rownames(out$basis), colnames(fit$w2))
  for (side in c("lower", "upper")) {
    proofs <- out[[paste0("evidence_", side)]]
    infinite <- is.infinite(out$bounds[[side]])
    expect_identical(
      !vapply(proofs, is.null, logical(1)),
      stats::setNames(infinite, colnames(obj))
    )
    expect_identical(names(proofs$sum$direction), colnames(fit$w2))
    expect_true(all(is.na(out[[paste0("arg_", side)]][infinite, , drop = FALSE])))
  }
  q <- list(A_i = list(matrix(0)), b_i = list(1), c_i = -1)
  found <- identified_set_search(0, matrix(1), q, 3, matrix(1), evidence = TRUE)
  proof <- linear_bounds_evidence(found$tail_lower, matrix(1, dimnames = list(NULL, "x")), "news")
  expect_identical(names(proof$x$origin), "news")
  expect_identical(names(proof$x$direction), "news")
})
