lv_test_empty_path <- function() {
  sample_data <- prepare_log_variance_search(
    c(0, 0, 2, -2, 3, -3), cbind(news = c(0, 0, 1, -1, 1, -1)),
    cbind(pc1 = 1:6), 1:6, 1:6
  )
  taus <- c(0.05, 0.1, 0.15)
  keys <- sprintf("%.17g", taus)
  quadratic <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -0.01)
  bounds_table <- data.frame(
    coef = "news", status = "bounded", outer_lower = -0.1, outer_upper = 0.1
  )
  quadratics <- stats::setNames(rep(list(quadratic), length(keys)), keys)
  tables <- stats::setNames(rep(list(bounds_table), length(keys)), keys)
  quadratics[[3L]] <- list(A_i = list(matrix(0)), b_i = list(0), c_i = -1)
  tables[[3L]]$status <- "unbounded"
  tables[[3L]]$outer_lower <- -Inf
  tables[[3L]]$outer_upper <- Inf
  control <- log_variance_search_control()
  sets <- profile_log_variance_map(sample_data, quadratics, tables, taus[1L], "logols",
    control = control
  )
  list(sets = sets, quadratics = quadratics, tables = tables, taus = taus, control = control)
}

test_that("empty log domains retain unavailable grid and display paths without fitting", {
  input <- lv_test_empty_path()
  keys <- names(input$quadratics)[1:2]
  display <- input$sets$results
  diagnostics <- display[[1L]]$diagnostics
  expect_identical(diagnostics$closure_reason, "empty_log_domain")
  expect_identical(diagnostics$zero_rows, 1:2)
  expect_identical(diagnostics$n_raw_feasible, NA_integer_)
  expect_true(all(c(
    diagnostics$n_attempted, diagnostics$n_evaluated,
    diagnostics$n_cached, diagnostics$n_failed, diagnostics$counters
  ) == 0L))
  path <- profile_log_variance_path(
    input$sets, input$quadratics, input$tables, input$taus[1:2], input$control
  )
  expect_true(all(is.na(c(path$rows$lower, path$rows$upper))))
  expect_true(all(c(path$rows$lower_status, path$rows$upper_status) == "unreliable"))
  expect_identical(unique(path$rows$source), c("grid", "display"))
  expect_identical(path$diagnostics$raw_feasible, stats::setNames(rep(NA_integer_, 2L), keys))
  reason <- list(closure_reason = "empty_log_domain", zero_rows = 1:2)
  expect_identical(
    path$diagnostics$pre_grid_closures, stats::setNames(rep(list(reason), 2L), keys)
  )
  expect_identical(
    path$diagnostics$display_pre_grid_closures, stats::setNames(list(reason), keys[1L])
  )
  expect_identical(path$diagnostics$thin_lattice, numeric(0))
  expect_equal(path$diagnostics$n_evaluated, 0)
  expect_equal(path$diagnostics$cache_hits, 0)
  expect_length(ls(input$sets$cache$store, all.names = TRUE), 0L)
  expect_identical(input$sets$results, display)
})

test_that("distinct empty display and grid closures coexist with a mean-domain closure", {
  input <- lv_test_empty_path()
  keys <- names(input$quadratics)
  path <- profile_log_variance_path(
    input$sets, input$quadratics, input$tables, input$taus[2:3], input$control
  )
  empty <- list(closure_reason = "empty_log_domain", zero_rows = 1:2)
  mean <- list(closure_reason = "mean_domain_unbounded", mean_status = "unbounded")
  expect_identical(
    path$diagnostics$pre_grid_closures, stats::setNames(list(empty, mean), keys[2:3])
  )
  expect_identical(
    path$diagnostics$display_pre_grid_closures, stats::setNames(list(empty), keys[1L])
  )
  expect_identical(
    path$diagnostics$raw_feasible, stats::setNames(rep(NA_integer_, 2L), keys[2:3])
  )
  statuses <- ifelse(path$rows$tau == input$taus[3L], "unbounded", "unreliable")
  expect_identical(path$rows$lower_status, statuses)
  expect_identical(path$rows$upper_status, statuses)
  expect_true(all(is.na(c(path$rows$lower, path$rows$upper))))
  expect_identical(unique(path$rows$source), c("grid", "display"))
  expect_identical(path$diagnostics$thin_lattice, numeric(0))
  expect_equal(path$diagnostics$n_evaluated, 0)
  expect_equal(path$diagnostics$cache_hits, 0)
})

test_that("an empty-domain label cannot excuse missing counts without valid row evidence", {
  result <- lv_test_empty_path()$sets$results[[1L]]
  expect_identical(lv_set_path_raw_count(result), NA_integer_)
  for (rows in list(NULL, integer(0), NA_integer_, 0L, -1L, 1.5, "1", matrix(1L))) {
    damaged <- result
    damaged$diagnostics$zero_rows <- rows
    expect_error(lv_set_path_raw_count(damaged), class = "hetid_error_bad_argument")
  }
  damaged <- result
  damaged$diagnostics$closure_reason <- "unknown"
  expect_error(lv_set_path_raw_count(damaged), class = "hetid_error_bad_argument")
})
