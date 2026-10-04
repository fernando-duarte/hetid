test_that("geometry and numerical endpoints remain separate", {
  qs <- mean_profile_ball()
  ev <- profile_evidence(qs, diag(2), matrix(0, 1, 2))
  high <- profile_linear_bound(qs, c(1, 0), "max", ev, 1L, QUADRATIC_PROFILE_CONTROL)
  expect_true(high$bounded && high$valid)
  expect_equal(high$bound, 1, tolerance = 1e-6)
  expect_true(ev$check_point(high$theta))
  enclosure <- profile_containing_bounds(ev, 2)
  expect_lte(high$bound, enclosure$outer_upper[1])
  saddle <- list(A_i = list(diag(c(1, -1))), b_i = list(c(0, 0)), c_i = -1)
  ev <- profile_evidence(saddle, diag(2), matrix(0, 1, 2))
  unbounded_side <- profile_linear_bound(saddle, c(0, 1), "min", ev, 2L, QUADRATIC_PROFILE_CONTROL)
  expect_identical(unbounded_side, list(bound = -Inf, bounded = FALSE, valid = TRUE))
  ev$summary$upper_state[1] <- "unresolved"
  unknown <- profile_linear_bound(
    saddle, c(1, 0), "max", ev, 1L,
    QUADRATIC_PROFILE_CONTROL
  )
  expect_identical(unknown, list(bound = NA_real_, bounded = FALSE, valid = FALSE))
})

test_that("boundary repair respects a checked anchor and displacement cap", {
  qs <- mean_profile_ball()
  ev <- profile_evidence(qs, diag(2), matrix(0, 1, 2))
  initial <- c(1 + 1e-8, 0)
  repaired <- profile_checked_candidate(ev, initial, QUADRATIC_PROFILE_CONTROL)
  expect_true(ev$check_point(repaired$theta))
  expect_lte(max(abs(repaired$theta / max(1, abs(initial)) -
    initial / max(1, abs(initial)))), QUADRATIC_PROFILE_CONTROL$CANDIDATE_CORRECTION_RTOL)
  expect_null(profile_checked_candidate(ev, c(2, 0), QUADRATIC_PROFILE_CONTROL))
  expect_null(profile_checked_candidate(ev, c(NA_real_, 0), QUADRATIC_PROFILE_CONTROL))
  ev$feasible_points <- matrix(numeric(), 0, 2)
  expect_null(profile_checked_candidate(ev, initial, QUADRATIC_PROFILE_CONTROL))
})

test_that("box growth preserves the donor elongated-set regression", {
  bounds <- function(eps, boxes) {
    qs <- list(A_i = list(diag(c(1, eps))), b_i = list(c(0, 0)), c_i = -1)
    ev <- profile_evidence(qs, diag(2), matrix(0, 1, 2))
    control <- QUADRATIC_PROFILE_CONTROL
    control$SOLVER_BOXES <- boxes
    profile_linear_bound(qs, c(0, 1), "max", ev, 2L, control)
  }
  boxes <- QUADRATIC_PROFILE_CONTROL$SOLVER_BOXES
  repeated <- rep(boxes[1], 3)
  expect_equal(bounds(1e-13, boxes)$bound, 3162277.6601655032, tolerance = 1e-12)
  expect_false(bounds(1e-13, repeated)$valid)
  expect_equal(bounds(1e-12, repeated)$bound, 999999.99999909056, tolerance = 1e-12)
  expect_equal(bounds(1e-12, boxes)$bound, 999999.99994357873, tolerance = 1e-12)
})

test_that("a containing box never substitutes attained endpoints", {
  tab <- data.frame(
    set_lower = -0.8, set_upper = 0.8, status = "bounded",
    outer_lower = -1, outer_upper = 1
  )
  expect_identical(profile_containing_box(tab), list(lower = -1, upper = 1))
  tab$set_upper <- 1.1
  expect_error(profile_containing_box(tab), "outside", class = "hetid_error")
  tab$outer_upper <- NA_real_
  expect_error(profile_containing_box(tab), "lacks finite", class = "hetid_error")
})

test_that("segment repair reaches a checked point beside a nearly tangent anchor", {
  ev <- profile_evidence(mean_profile_ball(), diag(2), matrix(c(1 - 1e-10, 1e-5), 1L))
  ev$feasible_points <- matrix(c(1 - 1e-10, 1e-5), 1L)
  initial <- c(1 + 1e-8, 0)
  control <- QUADRATIC_PROFILE_CONTROL
  expect_false(ev$check_point(initial))
  for (normalized in list(NULL, c(1, 0))) {
    repaired <- profile_checked_candidate(ev, initial, control, normalized)
    expect_true(ev$check_point(repaired$theta))
    movement <- max(abs(repaired$theta - initial))
    expect_gt(movement, control$CANDIDATE_CORRECTION_RTOL)
    expect_lte(movement, HETID_CONSTANTS$PROFILE_SEGMENT_RTOL)
    expect_lte(
      abs(repaired$theta[1] - initial[1]), HETID_CONSTANTS$PROFILE_SEGMENT_OBJECTIVE_RTOL
    )
  }
  expect_null(profile_checked_candidate(ev, c(1 + 1e-3, 0), control))
})

test_that("segment repair checks every active constraint at a corner", {
  corner <- list(
    A_i = rep(list(matrix(0, 2, 2)), 3L),
    b_i = list(c(1, 0), c(-0.8, 0.6), c(-0.8, 0.6)), c_i = rep(0, 3L)
  )
  ev <- list(
    check_point = quadratic_point_verifier(corner), feasible_points = matrix(c(-1, -2), 1L)
  )
  repaired <- profile_segment_candidate(ev, c(1e-8, 1e-8))
  expect_true(ev$check_point(repaired$theta))
  expect_lte(max(abs(repaired$theta - 1e-8)), HETID_CONSTANTS$PROFILE_SEGMENT_RTOL)
})
