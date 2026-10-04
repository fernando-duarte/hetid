test_that("precomputed line forms preserve interior, boundary and uncertain certificates", {
  interval <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -1)
  gap <- list(A_i = list(matrix(1), matrix(-1)), b_i = list(0, 0), c_i = c(-4, 1))
  isolated <- list(A_i = list(matrix(1), matrix(-1)), b_i = list(-2, 1), c_i = c(0, 0))
  cases <- list(
    list(q = interval, root = 0), list(q = interval, root = 1),
    list(q = gap, root = 0), list(q = gap, root = 1.5),
    list(q = isolated, root = 0), list(q = interval, root = 1 + 1e-9),
    list(q = interval, root = .1)
  )
  for (value in cases) {
    for (constant in c(2, 1e-10)) {
      w1 <- c(value$root, constant)
      w2 <- matrix(c(1, 0), ncol = 1L)
      for (direction in c(1, .1)) {
        forms <- lv_log_line_forms(value$q, w1, w2, 0, direction)
        expect_identical(forms$constraints, lapply(seq_along(value$q$c_i), function(i) {
          lv_log_line_constraint(
            value$q$A_i[[i]], value$q$b_i[[i]],
            value$q$c_i[i], 0, direction
          )
        }))
        expect_identical(forms$residuals, lapply(seq_along(w1), function(i) {
          lv_log_line_residual(w1[i], w2[i, ], 0, direction)
        }))
        expect_identical(
          lv_set_line_approach(value$q, w1, w2, 1L, 0, direction, function() forms),
          lv_set_line_approach(value$q, w1, w2, 1L, 0, direction)
        )
      }
    }
  }
  flat <- list(
    A_i = list(diag(c(1, 0)), diag(c(0, 1))),
    b_i = list(c(0, 0), c(0, 0)), c_i = c(-1, 0)
  )
  w2 <- rbind(c(1, 0), c(0, 0))
  forms <- lv_log_line_forms(flat, c(0, 2), w2, c(0, 0), c(1, 0))
  expect_true(lv_set_line_approach(
    flat, c(0, 2), w2, 1L, c(0, 0), c(1, 0),
    function() forms
  ))
})

test_that("coordinate forms are lazy and remain local to each census", {
  form_builder <- lv_log_line_forms
  approach <- lv_set_line_approach
  built <- list()
  directions <- list()
  local_mocked_bindings(
    lv_log_line_forms = function(quadratic, w1, w2, origin, direction) {
      built[[length(built) + 1L]] <<- list(quadratic, w1, w2, origin, direction)
      form_builder(quadratic, w1, w2, origin, direction)
    },
    lv_set_line_approach = function(quadratic, w1, w2, rows, origin, direction,
                                    line_forms = NULL) {
      directions[[length(directions) + 1L]] <<- direction
      approach(quadratic, w1, w2, rows, origin, direction, line_forms)
    }
  )
  q <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -100)
  w1 <- c(-2, 0, 2)
  w2 <- matrix(1, 3L, 2L)
  run <- function(q, w1, w2, groups = NULL) {
    lv_set_crossing_census(q, c(-3, -3), c(3, 3), w1, w2, groups = groups)
  }
  expected <- list(cross = 1:3, unresolved = integer(0), zero_rows = integer(0))
  expect_identical(run(q, w1, w2), expected)
  expect_length(built, 1L)
  expect_identical(directions, rep(list(c(1, 0)), 3L))
  changed <- q
  changed$c_i <- -64
  expect_identical(run(changed, w1, w2), expected)
  expect_length(built, 2L)
  expect_identical(built[[2L]][[1L]], changed)
  shifted <- c(-1, .5, 2)
  expect_identical(run(q, shifted, w2), expected)
  expect_length(built, 3L)
  expect_identical(built[[3L]][[2L]], shifted)
  groups <- list(rows = as.list(3:1), ambiguous = integer(0))
  expect_identical(run(q, w1, w2, groups), expected)
  expect_length(built, 4L)
  w2[, 1L] <- 0
  expect_identical(run(q, w1, w2), expected)
  expect_length(built, 5L)
  expect_identical(built[[5L]][[5L]], c(0, 1))
})

test_that("root and derivative rejections do not request invariant forms", {
  forms <- function() stop_bad_argument("forms requested before root admission", "line_forms")
  q <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -1)
  expect_false(lv_set_line_approach(q, 1, matrix(0), 1L, 0, 1, forms))
  expect_false(lv_set_line_approach(q, 1, matrix(1), 1L, 0, 0, forms))
  expect_false(lv_set_line_approach(q, 1, matrix(1), 1L, Inf, 1, forms))
  expect_false(lv_set_line_approach(
    q, .Machine$double.xmax, matrix(2^-1022),
    1L, 0, 1, forms
  ))
})
