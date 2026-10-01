test_that("uncertain curvature never supplies accepted bounds", {
  objectives <- cbind(diag(2), zero = 0)
  for (offset in c(-1e-14, 1e-14)) {
    label <- paste("offset", offset)
    a <- matrix(c(1, 1, 1, 1 + offset), 2)
    system <- list(A_i = list(a), b_i = list(c(0, 0)), c_i = -1)
    values <- eigen(a, symmetric = TRUE, only.values = TRUE)$values
    error <- HETID_CONSTANTS$QUADRATIC_SIGN_FACTOR * .Machine$double.eps *
      nrow(a) * sum(abs(a))
    expect_identical(sign(min(values)), sign(offset), info = label)
    expect_true(abs(min(values)) < error, info = label)
    for (search in c(FALSE, TRUE)) {
      out <- if (search) {
        compute_quadratic_set_evidence(system, objectives)
      } else {
        compute_quadratic_set_evidence(system, objectives, n_dir = 0L, maxit = 0L)
      }
      expect_true(out$nonempty, info = label)
      expect_true(out$check_point(c(0, 0)), info = label)
      expect_null(out$boundedness)
      expect_null(out$strict_direction)
      expect_length(out$directional, 0L)
      expect_identical(unname(lengths(out$tails)), rep(0L, 3), info = label)
      expect_identical(out$summary$lower_state,
        c("unresolved", "unresolved", "bounded"),
        info = label
      )
      expect_identical(out$summary$upper_state, out$summary$lower_state, info = label)
      bounds <- out$outer_bounds(objectives, refine = search)
      expect_true(all(is.na(bounds$lower[1:2])), info = label)
      expect_true(all(is.na(bounds$upper[1:2])), info = label)
      expect_identical(c(bounds$lower[3], bounds$upper[3]), c(0, 0), info = label)
      expect_length(attr(bounds, "pool"), 0L)
      expect_false(attr(bounds, "empty"), info = label)
      expect_match(attr(bounds, "reason"), "no positive definite certificate", fixed = TRUE)
    }
  }
})

test_that("curvature outside the uncertainty margin resolves both sides", {
  positive <- matrix(c(1, 1, 1, 1 + 1e-6), 2)
  system <- list(A_i = list(positive), b_i = list(c(0, 0)), c_i = -1)
  objectives <- diag(2)
  out <- compute_quadratic_set_evidence(system, objectives, n_dir = 0L, maxit = 0L)
  expect_true(out$nonempty)
  expect_false(is.null(out$boundedness))
  expect_identical(out$summary$lower_state, rep("bounded", 2))
  expect_identical(out$summary$upper_state, rep("bounded", 2))
  bounds <- out$outer_bounds(objectives, refine = FALSE)
  expect_true(all(is.finite(c(bounds$lower, bounds$upper))))
  exact <- sqrt(diag(solve(positive)))
  expect_true(all(bounds$lower <= -exact))
  expect_true(all(bounds$upper >= exact))

  negative <- matrix(c(1, 1, 1, 1 - 1e-6), 2)
  system$A_i <- list(negative)
  out <- compute_quadratic_set_evidence(system, objectives, n_dir = 0L, maxit = 0L)
  expect_true(out$nonempty)
  expect_null(out$boundedness)
  expect_false(is.null(out$strict_direction))
  expect_lt(drop(crossprod(out$strict_direction, negative %*% out$strict_direction)), 0)
  expect_identical(out$summary$lower_state, rep("unbounded", 2))
  expect_identical(out$summary$upper_state, rep("unbounded", 2))
  bounds <- out$outer_bounds(objectives, refine = FALSE)
  expect_true(all(is.na(c(bounds$lower, bounds$upper))))
})
