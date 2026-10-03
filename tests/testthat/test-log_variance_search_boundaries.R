test_that("every raw lattice is checked before construction", {
  input <- lv_test_oracle()$inputs
  control <- input$control
  control$sets$grid_points_limit <- 1L
  expect_error(search_log_variance_map(lv_test_linear(), input$quadratic,
    input$table,
    control = control
  ), class = "hetid_error_bad_argument")
  expect_error(lv_set_check_lattice(41L, 6L, 2e6), "before allocation",
    class = "hetid_error_bad_argument"
  )
  sample <- prepare_log_variance_search(
    c(-2, 2, -3, 3), cbind(b1 = c(-1, 1, -1, 1)),
    matrix(numeric(), 4L, 0L), 1:4, 1:4
  )
  map_control <- log_variance_map_control("logols")
  map_control$full_grid_safety_cap <- 1L
  map <- make_log_variance_map(sample, "logols", control = map_control)
  qs <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -0.0625)
  tab <- data.frame(coef = "b1", status = "bounded", outer_lower = -0.25, outer_upper = 0.25)
  expect_error(search_log_variance_map(map, qs, tab, seed = 0, cold_start_check = FALSE),
    "before allocation",
    class = "hetid_error_bad_argument"
  )
  control <- log_variance_search_control()
  control$search$grid_n <- 3L
  control$search$grid_floor <- 100L
  map$full_grid_safety_cap <- 3L
  expect_error(search_log_variance_map(map, qs, tab, control = control),
    "before allocation",
    class = "hetid_error_bad_argument"
  )
  oracle <- lv_test_path_oracle()
  control <- oracle$control
  control$sets$grid_points_limit <- 1L
  for (method in c("ppml", "harvey", "logols")) {
    expect_error(
      profile_log_variance_map(
        lv_test_sample(), oracle$quadratics,
        oracle$theta_tables, oracle$taus, method, c(0, 0), control
      ),
      "before allocation",
      class = "hetid_error_bad_argument"
    )
  }
})

test_that("mean-domain closure cannot be mistaken for mapped divergence", {
  input <- lv_test_oracle()$inputs
  qs <- list(A_i = list(diag(c(1, 0))), b_i = list(c(0, 0)), c_i = -1)
  tab <- input$table
  tab$status[[2L]] <- "unbounded"
  tab$outer_lower[[2L]] <- -Inf
  tab$outer_upper[[2L]] <- Inf
  calls <- 0L
  map <- lv_test_linear()
  map$fit_at_b <- function(b, start = NULL, phase = NULL) {
    calls <<- calls + 1L
    lv_set_fit_result(c(first = 1, second = 2), "ok", TRUE)
  }
  result <- search_log_variance_map(map, qs, tab, control = input$control)
  expect_identical(result$diagnostics$closure_reason, "mean_domain_unbounded")
  expect_identical(result$diagnostics$mean_status, tab$status)
  expect_true(all(result$schema$lower_status == "unbounded"))
  expect_true(all(is.na(result$schema$lower)))
  expect_true(all(is.na(result$schema$upper)))
  expect_identical(calls, 0L)
})

test_that("aggregate reuse binds systems, tau requests, point and controls", {
  oracle <- lv_test_path_oracle()
  sample <- lv_test_sample()
  map <- profile_log_variance_map(
    sample, oracle$quadratics, oracle$theta_tables,
    oracle$taus, "ppml", c(0, 0), oracle$control
  )
  expect_identical(profile_log_variance_map(sample, oracle$quadratics, oracle$theta_tables,
    oracle$taus, "ppml", c(0, 0), oracle$control,
    ppml = map
  ), map)
  expect_error(
    profile_log_variance_map(sample, oracle$quadratics, oracle$theta_tables,
      oracle$taus[2L], "ppml", c(0, 0), oracle$control,
      ppml = map
    ),
    class = "hetid_error_bad_argument"
  )
  smaller <- oracle$quadratics
  smaller[[1L]]$c_i <- -0.0001
  expect_error(
    profile_log_variance_map(sample, smaller, oracle$theta_tables,
      oracle$taus, "ppml", c(0, 0), oracle$control,
      ppml = map
    ),
    class = "hetid_error_bad_argument"
  )
  expect_error(profile_log_variance_path(
    map, smaller, oracle$theta_tables,
    oracle$taus, oracle$control
  ), class = "hetid_error_bad_argument")
  tab <- oracle$theta_tables
  tab[[1L]]$outer_upper <- c(0.01, 0.01)
  expect_error(profile_log_variance_path(
    map, oracle$quadratics, tab,
    oracle$taus, oracle$control
  ), class = "hetid_error_bad_argument")
  changed <- oracle$control
  changed$search$cold_start_check <- FALSE
  expect_error(profile_log_variance_map(sample, oracle$quadratics, oracle$theta_tables,
    oracle$taus, "ppml", c(0, 0), changed,
    ppml = map
  ), class = "hetid_error_bad_argument")
  expect_error(
    profile_log_variance_map(sample, oracle$quadratics, oracle$theta_tables,
      oracle$taus, "ppml", c(0.01, 0), oracle$control,
      ppml = map
    ),
    class = "hetid_error_bad_argument"
  )
  harvey <- profile_log_variance_map(sample, oracle$quadratics, oracle$theta_tables,
    oracle$taus[2L], "harvey", c(0, 0), oracle$control,
    ppml = map
  )
  expect_identical(harvey$taus, oracle$taus[2L])
})
