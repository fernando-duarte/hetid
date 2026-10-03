test_that("control and metadata containers reject before fitting or binding mutable state", {
  input <- lv_test_oracle()$inputs
  calls <- 0L
  map <- lv_test_linear()
  map$fit_at_b <- function(...) calls <<- calls + 1L
  run <- function(estimator = map, control = input$control) {
    cache <- new.env(parent = emptyenv())
    budget <- lv_set_budget()
    before <- as.list(budget)
    expect_error(search_log_variance_map(estimator, input$quadratic, input$table,
      cache = cache, budget = budget, control = control
    ), class = "hetid_error_bad_argument")
    expect_length(ls(cache, all.names = TRUE), 0L)
    expect_identical(as.list(budget), before)
  }
  for (bad in list(NULL, 1, "bad")) {
    run(control = bad)
    for (field in c("sets", "search")) {
      control <- input$control
      control[field] <- list(bad)
      run(control = control)
    }
    malformed <- map
    malformed["metadata"] <- list(bad)
    run(estimator = malformed)
  }
  expect_identical(calls, 0L)
})

test_that("path aggregate containers reject before reusing fits or cache state", {
  oracle <- lv_test_path_oracle()
  sets <- profile_log_variance_map(
    lv_test_sample(), oracle$quadratics,
    oracle$theta_tables, oracle$taus, "logols", c(0, 0), oracle$control
  )
  calls <- 0L
  sets$estimator$fit_at_b <- function(...) calls <<- calls + 1L
  store <- as.list(sets$cache$store)
  run <- function(value, tables = oracle$theta_tables) {
    expect_error(profile_log_variance_path(
      value, oracle$quadratics,
      tables, oracle$taus, oracle$control
    ), class = "hetid_error_bad_argument")
    expect_identical(calls, 0L)
    expect_identical(as.list(sets$cache$store), store)
  }
  for (bad in list(NULL, 1, "bad")) {
    run(bad)
    for (field in c("request", "results", "estimator")) {
      malformed <- sets
      malformed[field] <- list(bad)
      run(malformed)
    }
    malformed <- sets
    malformed$estimator["metadata"] <- list(bad)
    run(malformed)
    malformed <- sets
    malformed$results[1L] <- list(bad)
    run(malformed)
    for (field in c("schema", "diagnostics")) {
      malformed <- sets
      malformed$results[[1L]][field] <- list(bad)
      run(malformed)
    }
    tables <- oracle$theta_tables
    tables[1L] <- list(bad)
    run(sets, tables)
  }
})

test_that("PPML reuse and Harvey start metadata require their declared containers", {
  oracle <- lv_test_path_oracle()
  sample <- lv_test_sample()
  sets <- profile_log_variance_map(
    sample, oracle$quadratics, oracle$theta_tables,
    oracle$taus, "ppml", c(0, 0), oracle$control
  )
  calls <- 0L
  sets$estimator$fit_at_b <- function(...) calls <<- calls + 1L
  store <- as.list(sets$cache$store)
  start <- stats::lm.fit(sample$x_mat, log(sample$ols_residuals^2))$coefficients
  for (bad in list(NULL, 1, "bad")) {
    for (field in c("estimator", "metadata")) {
      malformed <- sets
      if (field == "estimator") {
        malformed["estimator"] <- list(bad)
      } else {
        malformed$estimator["metadata"] <- list(bad)
      }
      for (method in c("ppml", "harvey")) {
        expect_error(profile_log_variance_map(sample, oracle$quadratics,
          oracle$theta_tables, oracle$taus, method, c(0, 0), oracle$control,
          ppml = malformed
        ), class = "hetid_error_bad_argument")
      }
    }
    for (field in c("request", "results")) {
      malformed <- sets
      malformed[field] <- list(bad)
      expect_error(profile_log_variance_map(sample, oracle$quadratics,
        oracle$theta_tables, oracle$taus, "ppml", c(0, 0), oracle$control,
        ppml = malformed
      ), class = "hetid_error_bad_argument")
    }
    malformed <- sets$estimator
    malformed["metadata"] <- list(bad)
    expect_error(make_log_variance_map(sample, "harvey", c(0, 0),
      ppml = malformed, logols_coef = start
    ), class = "hetid_error_bad_argument")
  }
  expect_identical(calls, 0L)
  expect_identical(as.list(sets$cache$store), store)
})
