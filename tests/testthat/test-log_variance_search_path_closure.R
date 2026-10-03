lv_test_strip_path <- function() {
  oracle <- lv_test_path_oracle()
  tab <- oracle$theta_tables[[1L]]
  for (name in c("set_lower", "outer_lower")) tab[[name]] <- c(-1, -Inf)
  for (name in c("set_upper", "outer_upper")) tab[[name]] <- c(1, Inf)
  for (name in c("status", "lower_status", "upper_status")) {
    tab[[name]] <- c("bounded", "unbounded")
  }
  keys <- names(oracle$quadratics)
  strip <- list(A_i = list(diag(c(1, 0))), b_i = list(c(0, 0)), c_i = -1)
  list(
    oracle = oracle, sample = lv_test_sample(),
    quadratics = stats::setNames(rep(list(strip), length(keys)), keys),
    tables = stats::setNames(rep(list(tab), length(keys)), keys)
  )
}

test_that("unavailable display and grid paths preserve their mean-domain reason", {
  input <- lv_test_strip_path()
  oracle <- input$oracle
  sets <- profile_log_variance_map(input$sample, input$quadratics, input$tables,
    oracle$taus, "logols",
    control = oracle$control
  )
  path <- profile_log_variance_path(
    sets, input$quadratics, input$tables,
    oracle$taus, oracle$control
  )
  expect_true(all(is.na(path$rows$lower)) && all(is.na(path$rows$upper)))
  expect_true(all(path$rows$lower_status == "unbounded"))
  expect_true(all(path$rows$upper_status == "unbounded"))
  expect_identical(unique(path$rows$source), c("grid", "display"))
  expect_identical(
    path$diagnostics$raw_feasible,
    stats::setNames(rep(NA_integer_, 2L), names(input$quadratics))
  )
  expect_identical(path$diagnostics$thin_lattice, numeric(0))
  expect_equal(path$diagnostics$n_evaluated, 0)
  for (closed in path$diagnostics$pre_grid_closures) {
    expect_identical(closed$closure_reason, "mean_domain_unbounded")
    expect_identical(closed$mean_status, c("bounded", "unbounded"))
  }
})

test_that("new unavailable grids coexist with retained bounded display requests", {
  input <- lv_test_strip_path()
  oracle <- input$oracle
  sets <- profile_log_variance_map(
    input$sample, oracle$quadratics, oracle$theta_tables,
    oracle$taus, "logols", c(0, 0), oracle$control
  )
  key <- sprintf("%.17g", 0.2)
  quadratics <- oracle$quadratics
  tables <- oracle$theta_tables
  quadratics[[key]] <- input$quadratics[[1L]]
  tables[[key]] <- input$tables[[1L]]
  closed <- profile_log_variance_path(sets, quadratics, tables, 0.2, oracle$control)
  mixed <- profile_log_variance_path(
    sets, quadratics, tables,
    c(oracle$taus, 0.2), oracle$control
  )
  for (path in list(closed, mixed)) {
    unavailable <- path$rows[path$rows$source == "grid" & path$rows$tau == 0.2, ]
    expect_true(all(is.na(unavailable$lower)) && all(is.na(unavailable$upper)))
    expect_true(all(unavailable$lower_status == "unbounded"))
    expect_true(all(unavailable$upper_status == "unbounded"))
    expect_identical(names(path$diagnostics$pre_grid_closures), key)
    expect_identical(
      path$diagnostics$pre_grid_closures[[key]]$closure_reason,
      "mean_domain_unbounded"
    )
    expect_false(0.2 %in% path$diagnostics$thin_lattice)
    display <- path$rows[path$rows$source == "display", ]
    expect_identical(unique(display$tau), oracle$taus)
  }
  expect_identical(closed$diagnostics$raw_feasible, stats::setNames(NA_integer_, key))
  expect_true(all(!is.na(mixed$diagnostics$raw_feasible[1:2])))
  expect_true(is.na(mixed$diagnostics$raw_feasible[[key]]))
})

test_that("path count integrity and thin-lattice demotions remain enforced", {
  input <- lv_test_oracle()$inputs
  completed <- lv_test_search()
  expect_identical(lv_set_path_raw_count(completed), completed$diagnostics$n_raw_feasible)
  for (count in list(NULL, NA_integer_, NaN, -1L, 1.5, Inf)) {
    damaged <- completed
    damaged$diagnostics$n_raw_feasible <- count
    expect_error(lv_set_path_raw_count(damaged), class = "hetid_error_bad_argument")
  }
  map <- lv_test_linear()
  map$precheck <- function(quadratic, theta_table) list(unresolved = 1L)
  pending <- search_log_variance_map(map, input$quadratic, input$table, control = input$control)
  expect_identical(lv_set_path_raw_count(pending), NA_integer_)
  oracle <- lv_test_path_oracle()
  control <- oracle$control
  control$search$grid_floor <- 1000L
  sets <- profile_log_variance_map(
    lv_test_sample(), oracle$quadratics,
    oracle$theta_tables, oracle$taus, "ppml", c(0, 0), control
  )
  path <- profile_log_variance_path(
    sets, oracle$quadratics, oracle$theta_tables,
    oracle$taus, control
  )
  grid <- path$rows[path$rows$source == "grid", ]
  expect_identical(path$diagnostics$thin_lattice, oracle$taus)
  expect_true(all(path$diagnostics$raw_feasible > 0L &
    path$diagnostics$raw_feasible < control$search$grid_floor))
  expect_false(any(grid$lower_status == "bounded" | grid$upper_status == "bounded"))
})

test_that("log-OLS chunk controls fail at construction when malformed", {
  sample <- lv_test_sample()
  for (chunk in list(NA_integer_, NaN, 0L, -1L, 1.5, Inf, NULL, "1", TRUE, c(1L, 2L))) {
    control <- log_variance_map_control("logols")
    control["scan_chunk_size"] <- list(chunk)
    expect_error(make_log_variance_map(sample, "logols", control = control),
      "scan_chunk_size",
      class = "hetid_error_bad_argument"
    )
  }
  control <- log_variance_map_control("logols")
  control$scan_chunk_size <- 1L
  small <- make_log_variance_map(sample, "logols", control = control)
  usual <- make_log_variance_map(sample, "logols")
  grid <- lv_test_oracle()$inputs$points
  expect_oracle_equal(small$scan_grid(grid), usual$scan_grid(grid), ORACLE_TOLERANCE[["direct"]])
})
