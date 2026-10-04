test_that("source projection preserves the independent donor boundary", {
  oracle <- fv_test_boundary()
  source <- fv_test_source()
  cache <- new.env(parent = emptyenv())
  adapter <- fitted_volatility_adapter(source$map, oracle$design, oracle$labels, cache)
  fit <- adapter$fit_at_b(0.25, start = c(2, 3, 4), phase = "scan")
  expect_identical(fit, oracle$fit)
  expect_identical(adapter$jacobian_at_b(0.25, fit), oracle$jacobian)
  expect_identical(adapter$fit_at_b(0.25, c(5, 6, 7), "polish"), fit)
  expect_identical(adapter$fit_at_b(0.25, c(8, 9, 10), "cold_start"), fit)
  failed <- adapter$fit_at_b(-1, phase = "extra_start")
  expect_identical(failed, oracle$failed)
  expect_identical(adapter$fit_at_b(-1), failed)
  expect_identical(source$calls$fits, oracle$phases)
  expect_identical(as.list(adapter$source_budget), oracle$counts)
  expect_identical(adapter$precheck(NULL, NULL), oracle$precheck)
  expect_identical(source$calls$jacobians[[1L]]$coef, fit$source_coef)
  foreign <- source$map
  foreign$metadata$sample_id <- "sample-b"
  expect_error(fitted_volatility_adapter(foreign, oracle$design, oracle$labels, cache),
    class = "hetid_error"
  )
  expect_length(ls(cache$store), 2L)
})

test_that("cold source fits neither read nor replace warm cached fits", {
  oracle <- fv_test_boundary()
  source <- fv_test_source(cold = TRUE)
  cache <- new.env(parent = emptyenv())
  adapter <- fitted_volatility_adapter(source$map, oracle$design, oracle$labels, cache)
  warm <- adapter$fit_at_b(0.25)
  cold <- adapter$fit_at_b(0.25, start = c(1, 2, 3), phase = "cold_start")
  expect_false(identical(cold$source_coef, warm$source_coef))
  expect_null(source$calls$fits[[2L]]$start)
  expect_identical(adapter$fit_at_b(0.25), warm)
  expect_identical(adapter$source_budget$counters[["cold_start"]], 1L)
  expect_identical(adapter$source_budget$n_cached, 1L)
})

test_that("NULL Jacobians retain the outer dated missing-gradient policy", {
  oracle <- fv_test_boundary()
  source <- fv_test_source(jacobian = function() NULL)
  adapter <- fitted_volatility_adapter(
    source$map, oracle$design, oracle$labels,
    new.env(parent = emptyenv())
  )
  fit <- adapter$fit_at_b(0.25)
  expect_null(adapter$jacobian_at_b(0.25, fit))
  expect_length(source$calls$jacobians, 1L)
  expect_identical(source$calls$jacobians[[1L]]$coef, fit$source_coef)
  out <- lv_set_checked_jacobian(adapter, 0.25, fit)
  expect_identical(dim(out), c(3L, 1L))
  expect_true(all(is.nan(out)))
  expect_length(source$calls$jacobians, 2L)
})

test_that("Jacobian axes are checked once before dated multiplication", {
  oracle <- fv_test_boundary()
  good <- matrix(c(0, 1, 2), 3L, 1L,
    dimnames = list(c(HETID_CONSTANTS$INTERCEPT_LABEL, "pc1", "pc2"), "news")
  )
  bad <- list(
    good[3:1, , drop = FALSE], matrix(1, 2L, 1L),
    matrix("x", 3L, 1L), structure(good, dimnames = list(rownames(good), "foreign"))
  )
  for (value in bad) {
    source <- fv_test_source(jacobian = function() value)
    adapter <- fitted_volatility_adapter(
      source$map, oracle$design, oracle$labels,
      new.env(parent = emptyenv())
    )
    fit <- adapter$fit_at_b(0.25)
    expect_error(adapter$jacobian_at_b(0.25, fit), class = "hetid_error_bad_argument")
    expect_length(source$calls$jacobians, 1L)
  }
  source <- fv_test_source(jacobian = function() unname(good))
  adapter <- fitted_volatility_adapter(
    source$map, oracle$design, oracle$labels,
    new.env(parent = emptyenv())
  )
  expect_identical(
    unname(adapter$jacobian_at_b(0.25, adapter$fit_at_b(0.25))),
    unname(oracle$design %*% good)
  )
})

test_that("design, side and cache boundaries reject before fitting", {
  oracle <- fv_test_boundary()
  source <- fv_test_source()
  for (field in c("sides", "analyze_domain")) {
    map <- source$map
    map[[field]] <- function(...) NULL
    expect_error(fitted_volatility_adapter(
      map, oracle$design, oracle$labels,
      new.env(parent = emptyenv())
    ), class = "hetid_error_bad_argument")
  }
  expect_error(fitted_volatility_adapter(
    source$map, oracle$design[, 3:1], oracle$labels,
    new.env(parent = emptyenv())
  ), class = "hetid_error_bad_argument")
  expect_error(fitted_volatility_adapter(
    source$map, oracle$design, rep("same", 3L),
    new.env(parent = emptyenv())
  ), class = "hetid_error_bad_argument")
  expect_error(fitted_volatility_adapter(
    source$map, oracle$design, oracle$labels,
    emptyenv()
  ), class = "hetid_error_bad_argument")
  expect_length(source$calls$fits, 0L)
})

test_that("nonfinite projected values retain source coefficients and failure status", {
  oracle <- fv_test_boundary()
  source <- fv_test_source()
  source$map$fit_at_b <- function(...) {
    lv_set_fit_result(
      stats::setNames(c(0, 1e308, 1e308), source$map$coef_labels), "ok", TRUE
    )
  }
  adapter <- fitted_volatility_adapter(
    source$map, oracle$design, oracle$labels,
    new.env(parent = emptyenv())
  )
  fit <- adapter$fit_at_b(0)
  expect_identical(fit$fit_status, "nonfinite_fitted_log_variance")
  expect_false(fit$converged)
  expect_true(all(is.na(fit$coef)))
  expect_identical(names(fit$coef), oracle$labels)
  expect_true(all(is.finite(fit$source_coef)))
})

test_that("cold disagreement demotes dated engine sides with raw evidence retained", {
  oracle <- fv_test_boundary()
  map <- fv_test_source()$map
  map$precheck <- NULL
  map$fit_at_b <- function(b, start = NULL, phase = NULL) {
    value <- c(100, b, 2 * b)
    if (identical(phase, "cold_start")) value <- value + 0.1
    lv_set_fit_result(stats::setNames(value, map$coef_labels), "ok", TRUE,
      warm_start = value
    )
  }
  adapter <- fitted_volatility_adapter(
    map, oracle$design, oracle$labels,
    new.env(parent = emptyenv())
  )
  geometry <- fv_test_system()
  result <- search_log_variance_map(adapter, geometry$quadratic, geometry$table,
    seed = c(news = 0), max_grid_points = 7L, max_fit_evals = 1000L,
    control = fv_test_control()
  )
  expect_true(all(result$schema$lower_status == "unreliable"))
  expect_true(all(result$schema$upper_status == "unreliable"))
  expect_true(all(is.finite(result$schema$lower)))
  expect_length(result$diagnostics$cold_start, 6L)
  expect_identical(adapter$source_budget$counters[["cold_start"]], 6L)
})

test_that("wrong fit axes fail without promoting a source fit", {
  oracle <- fv_test_boundary()
  map <- fv_test_source()$map
  map$fit_at_b <- function(...) {
    lv_set_fit_result(
      c(pc2 = 0, pc1 = 1, foreign = 2),
      "ok", TRUE
    )
  }
  adapter <- fitted_volatility_adapter(
    map, oracle$design, oracle$labels,
    new.env(parent = emptyenv())
  )
  expect_error(adapter$fit_at_b(0.25), class = "hetid_error_bad_argument")
})
