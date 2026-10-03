lv_test_log_domain <- function(w1, w2, x = cbind(pc1 = seq_along(w1)),
                               constant = -.01, radius = .1, seed = NULL, cold = FALSE) {
  ids <- seq_along(w1)
  prepared <- prepare_log_variance_search(w1, cbind(news = w2), x, ids, ids)
  map <- make_log_variance_map(prepared, "logols")
  quadratic <- list(A_i = list(matrix(1)), b_i = list(0), c_i = constant)
  bounds_table <- data.frame(
    coef = "news", status = "bounded",
    outer_lower = -radius, outer_upper = radius
  )
  control <- log_variance_search_control()
  control$search$grid_n <- 5L
  control$search$grid_floor <- 3L
  control$search$primary_starts_per_side <- 1L
  result <- search_log_variance_map(map, quadratic, bounds_table,
    seed = seed, max_grid_points = 5L, max_fit_evals = 100L,
    cold_start_check = cold, control = control
  )
  list(sample = prepared, map = map, result = result)
}

test_that("constant nonzero residuals and outside roots do not imply infinity", {
  cases <- list(
    list(
      w1 = c(1e-10, -1e-10, 2, -2, 3, -3), w2 = c(0, 0, 1, -1, 1, -1),
      constant = -.01, radius = .1
    ),
    list(
      w1 = c(1 + 1e-9, -1 - 1e-9, 2, -2, 3, -3),
      w2 = c(1, -1, 0, 0, 0, 0), constant = -1, radius = 1
    )
  )
  for (input in cases) {
    found <- do.call(lv_test_log_domain, input)
    schema <- found$result$schema
    expected <- vapply(c(-input$radius, input$radius), function(b) {
      stats::lm.fit(found$sample$x_mat, 2 * log(abs(input$w1 - input$w2 * b)))$coef
    }, numeric(2))
    expect_true(all(is.finite(c(schema$lower, schema$upper))))
    expect_false(any(c(schema$lower_status, schema$upper_status) == "unbounded"))
    expect_equal(schema$lower, unname(apply(expected, 1L, min)), tolerance = 1e-6)
    expect_equal(schema$upper, unname(apply(expected, 1L, max)), tolerance = 1e-6)
  }
})

test_that("an everywhere zero residual closes the empty log domain before polishing", {
  found <- lv_test_log_domain(c(0, 0, 2, -2, 3, -3), c(0, 0, 1, -1, 1, -1))
  for (b in c(-.1, 0, .1)) {
    expect_identical(found$map$fit_at_b(b)$fit_status, "domain_failure")
  }
  schema <- found$result$schema
  expect_true(all(is.na(c(schema$lower, schema$upper))))
  expect_true(all(c(schema$lower_status, schema$upper_status) == "unreliable"))
  expect_identical(found$result$diagnostics$closure_reason, "empty_log_domain")
  expect_identical(found$result$diagnostics$zero_rows, 1:2)
  expect_identical(found$result$diagnostics$n_evaluated, 0L)
})

test_that("local approach witnesses respect components and structural boundaries", {
  interval <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -1)
  disconnected <- list(
    A_i = list(matrix(1), matrix(-1)),
    b_i = list(0, 0), c_i = c(-4, 1)
  )
  isolated <- list(
    A_i = list(matrix(1), matrix(-1)),
    b_i = list(-2, 1), c_i = c(0, 0)
  )
  witness <- function(q, root, u = 0, v = 1) {
    lv_set_line_approach(q, root, matrix(1), 1L, u, v)
  }
  expect_true(witness(interval, 0))
  expect_true(witness(interval, 1))
  expect_true(witness(disconnected, 1.5))
  expect_false(witness(disconnected, 0))
  expect_false(witness(isolated, 0))
  expect_false(witness(interval, 1 + 1e-9))
  flat <- list(
    A_i = list(diag(c(1, 0)), diag(c(0, 1))),
    b_i = list(c(0, 0), c(0, 0)), c_i = c(-1, 0)
  )
  expect_true(lv_set_line_approach(
    flat, 0, matrix(c(1, 0), 1L),
    1L, c(0, 0), c(1, 0)
  ))
  negative <- list(A_i = list(matrix(1), matrix(-1)), b_i = list(0, 0), c_i = c(-1, 0))
  expect_true(witness(negative, 0))
})

test_that("a scan with no valid log point supplies no attaining starts", {
  prepared <- prepare_log_variance_search(
    c(-1, 1, -2, 2),
    cbind(news = c(-1, 1, -2, 2)), matrix(numeric(), 4L, 0L), 1:4, 1:4
  )
  map <- make_log_variance_map(prepared, "logols")
  scan <- map$scan_grid(matrix(1, 1L))
  expect_null(scan$min)
  expect_identical(scan$n_failed, 0L)
  expect_identical(scan$n_domain, 1L)
  scan <- map$scan_grid(matrix(c(0, 1), ncol = 1L))
  expect_true(all(is.finite(c(scan$min, scan$max))))
  expect_equal(scan$arg_min, matrix(0, 1L))
})

test_that("public crossing decisions distinguish an attainable boundary from a gap", {
  boundary <- lv_test_log_domain(c(1, -1, 2, -2, 3, -3), c(1, -1, 0, 0, 0, 0),
    constant = -1, radius = 1
  )
  expect_identical(boundary$result$schema$lower_status[1L], "unbounded")
  expect_identical(boundary$result$schema$upper_status[2L], "unbounded")
  expect_identical(boundary$result$schema$lower[1L], -Inf)
  expect_identical(boundary$result$schema$upper[2L], Inf)
  prepared <- prepare_log_variance_search(
    c(0, 0, 3, -3, 4, -4),
    cbind(news = c(1, -1, 0, 0, 0, 0)), cbind(pc1 = 1:6), 1:6, 1:6
  )
  quadratic <- list(A_i = list(matrix(1), matrix(-1)), b_i = list(0, 0), c_i = c(-4, 1))
  bounds_table <- data.frame(coef = "news", status = "bounded", outer_lower = -2, outer_upper = 2)
  result <- search_log_variance_map(make_log_variance_map(prepared, "logols"), quadratic,
    bounds_table,
    max_grid_points = 5L, max_fit_evals = 100L
  )
  expect_true(all(c(result$schema$lower_status, result$schema$upper_status) == "unreliable"))
  expect_true(all(is.na(c(result$schema$lower, result$schema$upper))))
})

test_that("excluded log points from every start source stay outside refinement", {
  found <- lv_test_log_domain(rep(c(0, 2, -2), each = 2), rep(c(1, -.5, -.5), each = 2),
    x = cbind(pc1 = rep(c(-1, 1), 3))
  )
  ends <- new.env(parent = emptyenv())
  ends$labels <- c("intercept", "pc1")
  ends$lower_bad <- ends$upper_bad <- c(FALSE, FALSE)
  scan <- list(arg_min = matrix(0, 2L, 1L), arg_min_pool = list(list(0)))
  hit <- new.env(parent = emptyenv())
  hit$condition <- NULL
  objective <- found$map$coef_objective(1L)
  objective$fn <- function(b) stop_bad_argument("excluded start reached refinement", "b")
  record <- lv_set_polish_side(
    ends, 1L, "min", scan, 0, list(0),
    NULL, 1, objective, hit, log_variance_search_control()
  )
  expect_identical(record$n_trials, 0L)
  expect_false(record$accepted)
  expect_null(hit$condition)
})

test_that("public divergence retains a crossing in a disconnected positive component", {
  prepared <- prepare_log_variance_search(
    c(1.5, -1.5, 3, -3, 4, -4),
    cbind(news = c(1, -1, 0, 0, 0, 0)), cbind(pc1 = 1:6), 1:6, 1:6
  )
  map <- make_log_variance_map(prepared, "logols")
  quadratic <- list(A_i = list(matrix(1), matrix(-1)), b_i = list(0, 0), c_i = c(-4, 1))
  table <- data.frame(coef = "news", status = "bounded", outer_lower = -2, outer_upper = 2)
  expect_true(lv_set_line_approach(quadratic, prepared$w1, prepared$w2, 1:2, 0, 1))
  expect_identical(map$precheck(quadratic, table)$cross, 1:2)
  target <- c(1 / 3, -8 / 35)
  groups <- lv_set_logols_groups(prepared$prep)
  group <- which(vapply(groups$rows, identical, logical(1), 1:2))
  expect_equal(unname(groups$weights[, group]), target, tolerance = 1e-12)
  expect_identical(unname(groups$signs[, group]), sign(target))
  control <- log_variance_search_control()
  control$search$grid_n <- 5L
  control$search$grid_floor <- 3L
  control$search$primary_starts_per_side <- 1L
  result <- search_log_variance_map(map, quadratic, table,
    max_grid_points = 5L,
    max_fit_evals = 100L, cold_start_check = FALSE, control = control
  )
  expect_identical(result$diagnostics$domain$crossing, 1:2)
  expect_identical(result$schema$lower_status[1L], "unbounded")
  expect_identical(result$schema$upper_status[2L], "unbounded")
  expect_identical(result$schema$lower[1L], -Inf)
  expect_identical(result$schema$upper[2L], Inf)
})
