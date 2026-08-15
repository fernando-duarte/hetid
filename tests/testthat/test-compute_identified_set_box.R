# The identified-set box at a positive slack: the properties that make an
# inner approximation trustworthy (attained by feasible points, contains
# the tau = 0 point, nests in tau, never shrinks under refinement) and the
# rejections that keep it honest.

box_fit <- function(collinear = FALSE) {
  d <- simulate_box_dgp(collinear = collinear)
  compute_tau0_system(d$y1, d$y2, d$x, d$z)
}

test_that("tau = 0 is rejected rather than searched", {
  expect_error(
    compute_identified_set_box(box_fit(), tau = 0),
    "strictly positive",
    class = "hetid_error_bad_argument"
  )
})

test_that("tau outside the unit interval is rejected", {
  expect_error(
    compute_identified_set_box(box_fit(), tau = 1),
    class = "hetid_error_bad_argument"
  )
})

test_that("a fit without a tau = 0 point demands an explicit centre", {
  fit <- box_fit()
  fit$point <- NULL
  fit$beta1 <- NULL
  expect_error(
    compute_identified_set_box(fit, tau = 0.05, n_grid = 11L),
    "supply center",
    class = "hetid_error_bad_argument"
  )
})

test_that("an infeasible centre is refused", {
  expect_error(
    compute_identified_set_box(
      box_fit(),
      tau = 0.05, n_grid = 11L, center = c(50, 50, 50)
    ),
    "not strictly inside",
    class = "hetid_error_bad_argument"
  )
})

test_that("every finite bound is attained by a feasible theta", {
  box <- compute_identified_set_box(box_fit(), tau = 0.05, n_grid = 15L)
  checker <- make_system_checker(box$quadratic)
  for (k in seq_len(nrow(box$bounds))) {
    expect_lte(max(checker(box$arg_lower[k, ])), IDENTIFIED_SET_CONTROL$FEAS_TOL)
    expect_lte(max(checker(box$arg_upper[k, ])), IDENTIFIED_SET_CONTROL$FEAS_TOL)
    # the witness must actually attain the bound it is stored for
    expect_equal(box$arg_lower[k, k], box$bounds$lower[k], tolerance = 1e-10)
    expect_equal(box$arg_upper[k, k], box$bounds$upper[k], tolerance = 1e-10)
  }
})

test_that("a box corner is not itself a member of the set", {
  # the guard behind the previous test: bounds alone cannot stand in for a
  # witness, because the corner of a box around a non-convex set is
  # generally outside it
  box <- compute_identified_set_box(box_fit(), tau = 0.05, n_grid = 15L)
  checker <- make_system_checker(box$quadratic)
  expect_gt(max(checker(box$bounds$lower)), IDENTIFIED_SET_CONTROL$FEAS_TOL)
})

test_that("the box contains the tau = 0 point at every slack", {
  fit <- box_fit()
  point <- fit$point$theta
  for (tau in c(0.02, 0.05, 0.1)) {
    box <- compute_identified_set_box(fit, tau = tau, n_grid = 15L)
    expect_true(all(point >= box$bounds$lower))
    expect_true(all(point <= box$bounds$upper))
  }
})

test_that("boxes nest as the slack grows", {
  fit <- box_fit()
  tight <- compute_identified_set_box(fit, tau = 0.02, n_grid = 15L)
  loose <- compute_identified_set_box(fit, tau = 0.1, n_grid = 15L)
  expect_true(all(loose$bounds$lower <= tight$bounds$lower + 1e-9))
  expect_true(all(loose$bounds$upper >= tight$bounds$upper - 1e-9))
})

test_that("refining a nested grid never shrinks the box", {
  # 21 -> 41 -> 81 keeps every coarse node, so the finer search sees at
  # least what the coarser one did. Arbitrary sizes would not nest and the
  # comparison would be meaningless.
  fit <- box_fit()
  coarse <- compute_identified_set_box(fit, tau = 0.05, n_grid = 21L)
  medium <- compute_identified_set_box(fit, tau = 0.05, n_grid = 41L)
  fine <- compute_identified_set_box(fit, tau = 0.05, n_grid = 81L)
  expect_true(all(medium$bounds$lower <= coarse$bounds$lower + 1e-9))
  expect_true(all(medium$bounds$upper >= coarse$bounds$upper - 1e-9))
  expect_true(all(fine$bounds$lower <= medium$bounds$lower + 1e-9))
  expect_true(all(fine$bounds$upper >= medium$bounds$upper - 1e-9))
})

test_that("an unbounded set reports infinite sides, not a large box", {
  # the collinear DGP is above its boundedness frontier at this slack, so
  # a recession direction exists and the honest answer is infinite
  fit <- box_fit(collinear = TRUE)
  box <- compute_identified_set_box(fit, tau = 0.3, n_grid = 11L)
  expect_true(any(!is.finite(box$bounds$lower)))
  expect_true(any(!is.finite(box$bounds$upper)))
  expect_true(all(is.na(box$arg_lower[!is.finite(box$bounds$lower), ])))
})

test_that("the box carries the pieces it was built from", {
  fit <- box_fit()
  box <- compute_identified_set_box(fit, tau = 0.05, n_grid = 11L)
  expect_identical(box$w1, fit$w1)
  expect_identical(box$w2, fit$w2)
  expect_identical(attr(box, "tau"), 0.05)
  expect_identical(attr(box, "n_components"), 3L)
  expect_identical(box$bounds$coef, colnames(fit$w2))
})

test_that("the boundary returns the box visibly", {
  fit <- box_fit()
  expect_true(withVisible(
    compute_identified_set_box(fit, tau = 0.05, n_grid = 11L)
  )$visible)
})
