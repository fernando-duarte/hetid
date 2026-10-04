test_that("profile point matrices keep only finite points of the right length", {
  points <- list("a", 1, c(NA_real_, 0), c(1, 2), c(Inf, 0), c(3, 4))
  expect_identical(profile_point_matrix(points, 2L), rbind(c(1, 2), c(3, 4)))
  expect_identical(dim(profile_point_matrix(points[1:3], 2L)), c(0L, 2L))
})

test_that("an objective whose direction underflows is invalid", {
  expect_null(profile_objective_direction(c(1e300, 1e-30)))
  expect_equal(profile_objective_direction(c(3, 4)), c(0.6, 0.8))
})

test_that("the analytic Jacobian guard rejects asymmetric constraint matrices", {
  expect_invisible(profile_assert_symmetric(
    list(A_i = list(diag(2))), QUADRATIC_PROFILE_CONTROL
  ))
  expect_error(
    profile_assert_symmetric(
      list(A_i = list(matrix(c(1, 0, 1, 1), 2L))), QUADRATIC_PROFILE_CONTROL
    ),
    class = "hetid_error_bad_argument"
  )
})

test_that("certificate centers that overflow are discarded", {
  center <- function(quadratic) {
    certificate <- quadratic_boundedness_search(quadratic, maxit = 0L)$certificate
    expect_false(is.null(certificate))
    quadratic_certificate_center(quadratic, certificate)
  }
  # the weighted linear term overflows
  expect_null(center(list(A_i = list(matrix(1e-300)), b_i = list(1e100), c_i = -1)))
  # the linear term is finite but dividing by a tiny eigenvalue overflows
  expect_null(center(list(
    A_i = list(diag(c(1, 1e-12))), b_i = list(c(0, 1e298)), c_i = -1
  )))
  expect_identical(
    center(list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)), c(0, 0)
  )
})

test_that("containing bounds refuse a set that is both nonempty and empty", {
  evidence <- list(
    nonempty = TRUE, boundedness = list(),
    outer_bounds = function(objectives, refine) {
      structure(data.frame(lower = -1, upper = 1), empty = TRUE)
    }
  )
  expect_error(profile_containing_bounds(evidence, 1L),
    "Geometry conflict",
    class = "hetid_error"
  )
})

test_that("non-finite or unchecked solver candidates leave the endpoint invalid", {
  quadratic <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -1)
  evidence <- profile_evidence(quadratic, matrix(1), matrix(0))
  checked_candidate <- profile_checked_candidate
  phi <- Inf
  reject <- FALSE
  candidate_calls <- 0L
  testthat::local_mocked_bindings(
    profile_solve_checked = function(...) list(phi = phi),
    profile_checked_candidate = function(...) {
      candidate_calls <<- candidate_calls + 1L
      if (reject) NULL else checked_candidate(...)
    },
    .package = "hetid"
  )
  invalid <- list(bound = NA_real_, bounded = FALSE, valid = FALSE)
  bound <- function() {
    profile_linear_bound(quadratic, 1, "min", evidence, 1L, QUADRATIC_PROFILE_CONTROL)
  }
  expect_identical(bound(), invalid)
  expect_identical(candidate_calls, 0L)
  phi <- -1
  reject <- TRUE
  expect_identical(bound(), invalid)
  expect_identical(candidate_calls, length(QUADRATIC_PROFILE_CONTROL$SOLVER_BOXES))
  reject <- FALSE
  valid <- bound()
  expect_true(valid$valid)
  expect_true(valid$bounded)
  expect_equal(valid$bound, -1, tolerance = 1e-10)
})
