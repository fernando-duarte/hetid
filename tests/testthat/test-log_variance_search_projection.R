test_that("log-OLS uses the existing stable package evaluator", {
  sample <- lv_test_sample()
  map <- make_log_variance_map(sample, "logols")
  oracle <- lv_test_oracle()
  for (i in seq_len(nrow(oracle$inputs$points))) {
    b <- oracle$inputs$points[i, ]
    direct <- evaluate_log_projection(sample$prep, b, "log")
    expect_identical(map$fit_at_b(b)$coef, direct$coef)
    expect_identical(map$jacobian_at_b(b), direct$jacobian)
    expect_equal(direct$coef, oracle$logols[[i]]$fit$coef, tolerance = 1e-10)
    expect_equal(direct$jacobian, oracle$logols[[i]]$jacobian, tolerance = 1e-10)
    objective <- map$coef_objective(2L)
    step <- 1e-6
    finite_difference <- vapply(seq_along(b), function(j) {
      change <- replace(numeric(length(b)), j, step)
      (objective$fn(b + change) - objective$fn(b - change)) / (2 * step)
    }, numeric(1))
    names(finite_difference) <- colnames(sample$w2)
    expect_equal(objective$gr(b), finite_difference, tolerance = 1e-5)
  }
  batch <- map$scan_grid(oracle$inputs$points)
  expect_equal(batch$min, oracle$logols_scan$min, tolerance = 1e-10)
  expect_equal(batch$max, oracle$logols_scan$max, tolerance = 1e-10)
  expect_identical(batch$cross_grid, oracle$logols_scan$cross_grid)
})

test_that("zero residuals and numerical failures stay distinct", {
  w1 <- c(-2, 2, -3, 3)
  w2 <- cbind(b1 = c(-1, 1, -1, 1))
  sample <- prepare_log_variance_search(w1, w2, matrix(numeric(), 4L, 0L), 1:4, 1:4)
  map <- make_log_variance_map(sample, "logols")
  expect_identical(map$fit_at_b(2)$fit_status, "domain_failure")
  expect_false(log_variance_fit_ok(map$fit_at_b(2)))
  huge <- prepare_log_variance_search(w1, w2 * 2, matrix(numeric(), 4L, 0L), 1:4, 1:4)
  expect_identical(
    make_log_variance_map(huge, "logols")$fit_at_b(.Machine$double.xmax)$fit_status,
    "nonfinite_fitted_log_variance"
  )
})

test_that("crossing census uses containing bounds and checked attained ranges", {
  qs <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
  w2 <- rbind(c(1, 0), c(0, 2), c(3, 4), c(0, 0), c(0, 0), c(0.6, 0.8))
  w1 <- c(0.5, 2.5, -5 + 1e-12, 1, 0, 1 + 1e-5)
  result <- lv_set_fixed_rng(lv_set_crossing_census(
    qs, c(-1, -1), c(1, 1),
    w1, w2, lv_set_logols_control()
  ))
  expect_identical(result$cross, integer())
  expect_identical(result$zero_rows, 5L)
  expect_identical(result$unresolved, integer())
  strip <- list(A_i = list(diag(c(1, 0))), b_i = list(c(0, 0)), c_i = -1)
  pending <- lv_set_fixed_rng(lv_set_crossing_census(
    strip, c(-1, -50), c(1, 50),
    c(0.5, 30), diag(2), lv_set_logols_control()
  ))
  expect_true(all(1:2 %in% c(pending$cross, pending$unresolved)))
})
