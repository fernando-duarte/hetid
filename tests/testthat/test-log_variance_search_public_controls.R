test_that("public controls support custom and built-in searches without donor fields", {
  control <- log_variance_search_control()
  expect_identical(control$sets$grid_points_limit, 2e6)
  expect_identical(control$sets[names(QUADRATIC_PROFILE_CONTROL)], QUADRATIC_PROFILE_CONTROL)
  expect_null(QUADRATIC_PROFILE_CONTROL$grid_points_limit)
  control$search$grid_n <- 7L
  control$search$grid_floor <- 3L
  map <- lv_test_linear()
  map$coef_labels <- "f"
  map$fit_at_b <- function(b, start = NULL, phase = NULL) {
    lv_set_fit_result(c(f = b[[1L]]), "ok", TRUE)
  }
  map$jacobian_at_b <- function(b, fit = NULL) matrix(1, 1L, 1L)
  quadratic <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -0.1^2)
  table <- data.frame(coef = "news", status = "bounded", outer_lower = -0.1, outer_upper = 0.1)
  result <- search_log_variance_map(map, quadratic, table, control = control)
  expect_true(all(result$schema$lower_status == "bounded"))
  expect_true(all(result$schema$upper_status == "bounded"))
  expect_equal(c(result$schema$lower, result$schema$upper), c(-0.1, 0.1), tolerance = 1e-6)
})

test_that("public PPML and Harvey controls preserve direct and aggregate donor searches", {
  input <- lv_test_oracle()$inputs
  oracle <- lv_test_path_oracle()
  sample <- lv_test_sample()
  control <- log_variance_search_control()
  control$search$grid_n <- 7L
  control$search$grid_floor <- 3L
  control$search$primary_grid_cap <- 30L
  control$search$coverage_grid_cap <- 30L
  control$search$primary_fit_budget <- 1000L
  control$search$sensitivity_fit_budget <- 1000L
  control$search$coverage_fit_budget <- 1000L
  expect_identical(control$search, oracle$control$search)
  expect_identical(control$sets, oracle$control$sets[names(control$sets)])
  point <- c(0, 0)
  ppml <- make_log_variance_map(sample, "ppml", point)
  start <- stats::lm.fit(sample$x_mat, log(sample$ols_residuals^2))$coefficients
  harvey <- make_log_variance_map(sample, "harvey", point, ppml = ppml, logols_coef = start)
  for (method in c("ppml", "harvey")) {
    map <- if (method == "ppml") ppml else harvey
    direct <- search_log_variance_map(map, input$quadratic, input$table,
      seed = point, max_grid_points = 30L, max_fit_evals = 1000L, control = control
    )
    matched <- search_log_variance_map(map, input$quadratic, input$table,
      seed = point, max_grid_points = 30L, max_fit_evals = 1000L, control = oracle$control
    )
    expect_identical(direct$schema, matched$schema)
    expect_identical(direct$diagnostics, matched$diagnostics)
    expect_true(all(direct$schema$lower_status == "bounded"))
    expect_true(all(direct$schema$upper_status == "bounded"))
    expect_true(all(is.finite(c(direct$schema$lower, direct$schema$upper))))
    sets <- profile_log_variance_map(
      sample, oracle$quadratics,
      oracle$theta_tables, oracle$taus, method, point, control
    )
    expected <- oracle[[method]]
    keep <- names(expected)[!is.na(names(expected))]
    expect_identical(lv_test_core(sets[keep]), lv_test_core(expected[keep]))
    for (result in sets$results) {
      expect_true(all(result$schema$lower_status == "bounded"))
      expect_true(all(result$schema$upper_status == "bounded"))
      expect_true(all(is.finite(c(result$schema$lower, result$schema$upper))))
    }
  }
})

test_that("public raw lattice caps reject malformed and oversized work before fitting", {
  input <- lv_test_oracle()$inputs
  map <- lv_test_linear()
  calls <- 0L
  map$fit_at_b <- function(...) calls <<- calls + 1L
  invalid <- list(NULL, NA_real_, NaN, Inf, "2e6", c(1, 2), matrix(2e6), 0, -1, 1.5)
  for (limit in invalid) {
    control <- log_variance_search_control()
    control$sets$grid_points_limit <- limit
    cache <- new.env(parent = emptyenv())
    budget <- lv_set_budget()
    before <- as.list(budget)
    expect_error(search_log_variance_map(map, input$quadratic, input$table,
      cache = cache, budget = budget, control = control
    ), class = "hetid_error_bad_argument")
    expect_length(ls(cache, all.names = TRUE), 0L)
    expect_identical(as.list(budget), before)
  }
  control <- log_variance_search_control()
  control$search$grid_n <- 3L
  control$sets$grid_points_limit <- 8L
  expect_error(search_log_variance_map(map, input$quadratic, input$table, control = control),
    "before allocation",
    class = "hetid_error_bad_argument"
  )
  control$sets$grid_points_limit <- 9L
  expect_error(search_log_variance_map(map, input$quadratic, input$table, control = control),
    "before allocation",
    class = "hetid_error_bad_argument"
  )
  logols_control <- log_variance_map_control("logols")
  logols_control$full_grid_safety_cap <- 8L
  logols <- make_log_variance_map(lv_test_sample(), "logols", control = logols_control)
  expect_error(search_log_variance_map(logols, input$quadratic, input$table, control = control),
    "before allocation",
    class = "hetid_error_bad_argument"
  )
  expect_identical(calls, 0L)
})
