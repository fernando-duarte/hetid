test_that("dated extrema search the joint set rather than coefficient marginals", {
  source <- fv_test_source()$map
  source$theta_labels <- c("b1", "b2")
  source$precheck <- NULL
  loading <- rbind(c(0, 0), c(1, 1), c(1, 1))
  dimnames(loading) <- list(source$coef_labels, source$theta_labels)
  source$fit_at_b <- function(b, start = NULL, phase = NULL) {
    value <- drop(loading %*% b)
    lv_set_fit_result(value, "ok", TRUE, warm_start = value)
  }
  source$jacobian_at_b <- function(b, fit = NULL) loading
  design <- rbind(c(0, 1, -1), c(0, 1, 1), c(0, 1, 0))
  colnames(design) <- source$coef_labels
  adapter <- fitted_volatility_adapter(
    source, design, sprintf("date_%04d", 1:3),
    new.env(parent = emptyenv())
  )
  geometry <- fv_test_system(1, source$theta_labels)
  result <- search_log_variance_map(adapter, geometry$quadratic, geometry$table,
    seed = c(b1 = 0, b2 = 0), max_grid_points = 30L,
    max_fit_evals = 1000L, control = fv_test_control()
  )
  expected <- c(0, 2 * sqrt(2), sqrt(2))
  expect_equal(result$schema$lower, -expected, tolerance = 1e-6)
  expect_equal(result$schema$upper, expected, tolerance = 1e-6)
  expect_equal(result$schema$lower[1L], 0, tolerance = 1e-10)
  expect_equal(result$schema$upper[1L], 0, tolerance = 1e-10)
  expect_true(all(result$schema$lower_status == "bounded"))
  expect_equal(fitted_volatility_level(result$schema$upper, 0.5), exp(0.5 * expected),
    tolerance = 1e-6
  )
  b <- c(b1 = 0.2, b2 = -0.1)
  step <- 1e-6
  difference <- sapply(seq_along(b), function(j) {
    plus <- minus <- b
    plus[j] <- plus[j] + step
    minus[j] <- minus[j] - step
    (adapter$fit_at_b(plus)$coef - adapter$fit_at_b(minus)$coef) / (2 * step)
  })
  expect_equal(unname(adapter$jacobian_at_b(b, adapter$fit_at_b(b))),
    unname(difference),
    tolerance = 1e-5
  )
})

test_that("exponentials preserve the frozen extended-real and underflow policy", {
  oracle <- fv_test_boundary()
  for (i in seq_along(oracle$levels)) {
    expect_identical(fitted_volatility_level(oracle$eta, c(1, 0.5)[i]), oracle$levels[[i]])
  }
})

test_that("overflow demotion preserves raw evidence and containment errors carry dates", {
  sets <- fv_test_sets()$sets
  design <- sets$sample$x_mat
  design[, LOG_VARIANCE_INTERCEPT_LABEL] <- 0
  adapter <- fitted_volatility_adapter(
    sets$estimator, design,
    sprintf("date_%04d", 1:12), sets$cache
  )
  schema <- data.frame(
    coef = adapter$coef_labels, lower = 0, upper = 2,
    lower_status = "bounded", upper_status = "bounded", lower_source = "polish",
    upper_source = "polish"
  )
  schema$upper[1L] <- 1000
  result <- list(schema = schema, diagnostics = list(n_raw_feasible = 3L))
  envelope <- fitted_volatility_result(
    sets, adapter, result, 0.05, rep(1, 12), "ok",
    fv_test_control()
  )
  expect_identical(envelope$schema, schema)
  expect_identical(envelope$data$upper_status[1L], "unreliable")
  expect_true(is.na(envelope$data$variance_upper[1L]))
  expect_true(is.finite(envelope$data$volatility_upper[1L]))
  expect_identical(envelope$data$lower_status, rep("bounded", 12))
  expect_identical(envelope$metadata$response_date, sets$sample$response_date)
  expect_identical(envelope$metadata$predictor_date, sets$sample$date)
  expect_false(envelope$metadata$include_intercept)
  point <- rep(1, 12)
  point[2L] <- 3
  error <- tryCatch(fitted_volatility_result(
    sets, adapter, result, 0.05, point, "ok",
    fv_test_control()
  ), hetid_error = identity)
  expect_s3_class(error, "hetid_error_numerical")
  expect_identical(error$tau, 0.05)
  expect_identical(error$date, sets$sample$response_date[2L])
})

test_that("display and dated closures retain separate axes and exact origins", {
  sets <- fv_test_sets()$sets
  display <- list(closure_reason = "mean_domain_unbounded", mean_status = "unbounded")
  target <- list(precheck_failed = c("witness-a", "witness-b"))
  sets$results[[1L]]$diagnostics <- c(list(n_raw_feasible = NA_integer_), display)
  display_only <- profile_tau_key(0.2)
  sets$results[[display_only]] <- sets$results[[1L]]
  key <- profile_tau_key(0.05)
  results <- stats::setNames(list(list(diagnostics = c(
    list(n_raw_feasible = NA_integer_), target
  ))), key)
  results[[profile_tau_key(0.1)]] <- list(diagnostics = list(n_raw_feasible = 3L))
  diagnostics <- fitted_volatility_closures(sets, results)
  expect_identical(diagnostics$pre_grid_closures[[key]], target)
  expect_identical(diagnostics$display_pre_grid_closures[[key]], display)
  expect_identical(diagnostics$display_pre_grid_closures[[display_only]], display)
  expect_identical(names(diagnostics$pre_grid_closures), key)
  expect_identical(
    diagnostics$closure_axes$pre_grid_closures$response_date,
    sets$sample$response_date
  )
  expect_identical(
    diagnostics$closure_axes$pre_grid_closures$coef_labels,
    sprintf("date_%04d", 1:12)
  )
  expect_identical(
    diagnostics$closure_axes$display_pre_grid_closures$coef_labels,
    sets$estimator$coef_labels
  )
  expect_identical(fitted_volatility_closures(fv_test_sets()$sets), list())
})

test_that("unavailable mean domains and point failures remain distinct", {
  setup <- fv_test_sets()
  geometry <- fv_test_system()
  geometry$table$status <- "unbounded"
  envelope <- profile_fitted_volatility(setup$sets, geometry$quadratic, geometry$table, 0.05,
    point = NULL, control = fv_test_control()
  )
  expect_true(all(is.na(envelope$data$variance_lower)))
  expect_true(all(is.na(envelope$data$volatility_upper)))
  expect_identical(envelope$point_status, "not_in_set")
  expect_identical(envelope$diagnostics$engine$closure_reason, "mean_domain_unbounded")
  expect_length(setup$calls$fits, 0L)
  for (point in list(c(news = 1), c(news = -0.05), c(news = 0))) {
    result <- profile_fitted_volatility(setup$sets, geometry$quadratic, geometry$table, 0.05,
      point = point, control = fv_test_control()
    )
    expected <- if (point == 1) "not_in_set" else if (point < 0) "domain_failure" else "ok"
    expect_identical(result$point_status, expected)
  }
})

test_that("public and internal budget routes preserve independent point accounting", {
  setup <- fv_test_sets()
  geometry <- fv_test_system()
  control <- fv_test_control()
  for (limit in c(0, Inf)) {
    control$search$envelope_fit_budget <- limit
    expect_error(profile_fitted_volatility(setup$sets, geometry$quadratic, geometry$table,
      0.05,
      control = control
    ), class = "hetid_error_bad_argument")
    expect_length(ls(setup$sets$cache), 0L)
    expect_length(setup$calls$fits, 0L)
  }
  control$search$envelope_fit_budget <- 1L
  result <- profile_fitted_volatility(setup$sets, geometry$quadratic, geometry$table,
    0.05,
    control = control
  )
  expect_true(result$diagnostics$engine$budget_exhausted)
  expect_identical(result$point_status, "ok")
  expect_identical(result$diagnostics$engine$n_evaluated, 1L)
  expect_identical(result$diagnostics$source$n_attempted, 2L)
  design <- setup$sets$sample$x_mat
  design[, LOG_VARIANCE_INTERCEPT_LABEL] <- 0
  adapter <- fitted_volatility_adapter(
    setup$sets$estimator, design,
    sprintf("date_%04d", 1:12), setup$sets$cache
  )
  budget <- lv_set_budget(0L)
  lv_set_validate_budget(budget)
  direct <- search_log_variance_map(adapter, geometry$quadratic, geometry$table,
    budget = budget, max_grid_points = 7L, control = fv_test_control()
  )
  expect_identical(direct$diagnostics$n_evaluated, 0L)
  expect_true(direct$diagnostics$budget_exhausted)
  expect_true(all(direct$schema$lower_status == "unreliable"))
})
