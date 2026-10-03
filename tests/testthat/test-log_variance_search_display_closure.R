test_that("display-only mean closures keep their source and do not rerun", {
  oracle <- lv_test_path_oracle()
  quadratics <- oracle$quadratics
  tables <- oracle$theta_tables
  quadratics[[2L]] <- list(A_i = list(diag(c(1, 0))), b_i = list(c(0, 0)), c_i = -1)
  for (name in c("set_lower", "outer_lower")) tables[[2L]][[name]] <- c(-1, -Inf)
  for (name in c("set_upper", "outer_upper")) tables[[2L]][[name]] <- c(1, Inf)
  for (name in c("status", "lower_status", "upper_status")) {
    tables[[2L]][[name]] <- c("bounded", "unbounded")
  }
  sets <- profile_log_variance_map(
    lv_test_sample(), quadratics, tables,
    oracle$taus, "logols", c(0, 0), oracle$control
  )
  expected <- sets$results[[2L]]$diagnostics[c("closure_reason", "mean_status")]
  calls <- numeric()
  precheck <- sets$estimator$precheck
  sets$estimator$precheck <- function(quadratic, theta_table) {
    calls <<- c(calls, quadratic$c_i)
    precheck(quadratic, theta_table)
  }
  path <- profile_log_variance_path(sets, quadratics, tables, oracle$taus[[1L]], oracle$control)
  key <- names(quadratics)[[2L]]
  expect_null(path$diagnostics$pre_grid_closures)
  expect_identical(names(path$diagnostics$display_pre_grid_closures), key)
  expect_identical(path$diagnostics$display_pre_grid_closures[[key]], expected)
  expect_identical(names(path$diagnostics$raw_feasible), names(quadratics)[[1L]])
  expect_true(all(calls == quadratics[[1L]]$c_i))
  rows <- path$rows[path$rows$source == "display" & path$rows$tau == oracle$taus[[2L]], ]
  expect_true(all(is.na(rows$lower)) && all(is.na(rows$upper)))
  expect_true(all(rows$lower_status == "unbounded" & rows$upper_status == "unbounded"))
})

test_that("display unresolved prechecks preserve their original recorded evidence", {
  oracle <- lv_test_path_oracle()
  sets <- profile_log_variance_map(
    lv_test_sample(), oracle$quadratics, oracle$theta_tables,
    oracle$taus, "logols", c(0, 0), oracle$control
  )
  map <- sets$estimator
  map$precheck <- function(quadratic, theta_table) list(unresolved = c(2L, 5L))
  pending <- search_log_variance_map(map, oracle$quadratics[[2L]], oracle$theta_tables[[2L]],
    tau = oracle$taus[[2L]], control = oracle$control
  )
  sets$results[[2L]] <- pending
  path <- profile_log_variance_path(
    sets, oracle$quadratics, oracle$theta_tables,
    oracle$taus[[1L]], oracle$control
  )
  key <- names(oracle$quadratics)[[2L]]
  expect_identical(
    path$diagnostics$display_pre_grid_closures[[key]],
    list(precheck_failed = c(2L, 5L))
  )
  expect_null(path$diagnostics$pre_grid_closures)
  rows <- path$rows[path$rows$source == "display" & path$rows$tau == oracle$taus[[2L]], ]
  expect_true(all(is.na(rows$lower)) && all(is.na(rows$upper)))
  expect_true(all(rows$lower_status == "unreliable" & rows$upper_status == "unreliable"))
})
