test_that("PPML and Harvey fits and Jacobians retain exact donor arithmetic", {
  oracle <- lv_test_oracle()
  sample <- lv_test_sample()
  point <- oracle$inputs$points[1L, ]
  ppml <- make_log_variance_map(sample, "ppml", point, point, "tau_zero_point")
  coefficients <- stats::lm.fit(sample$x_mat, log(sample$ols_residuals^2))$coefficients
  harvey <- make_log_variance_map(sample, "harvey", point,
    ppml = ppml, logols_coef = coefficients
  )
  for (method in c("ppml", "harvey")) {
    map <- if (method == "ppml") ppml else harvey
    for (i in seq_len(nrow(oracle$inputs$points))) {
      b <- oracle$inputs$points[i, ]
      fit <- map$fit_at_b(b)
      expect_identical(fit, oracle[[method]][[i]]$fit)
      expect_identical(map$jacobian_at_b(b, fit), oracle[[method]][[i]]$jacobian)
      expect_true(log_variance_fit_ok(fit))
    }
  }
  expect_equal(harvey$point_fit$coef, oracle$harvey[[1L]]$fit$coef, tolerance = 1e-12)
  expect_true(log_variance_fit_ok(harvey$point_fit))
  expect_true(is.list(harvey$point_fit$diagnostics$per_start_criteria))
})

test_that("response scaling and starts preserve original coefficient units", {
  sample <- lv_test_sample()
  point <- c(0, 0)
  ordinary <- make_log_variance_map(sample, "ppml", point, point)
  scaled <- make_log_variance_map(sample, "ppml", point, point, response_scale = 10)
  expect_equal(scaled$fit_at_b(point)$coef, ordinary$fit_at_b(point)$coef,
    tolerance = 1e-6
  )
  expect_equal(scaled$start_bundle$coef_scaled[[1L]],
    scaled$start_bundle$coef_original[[1L]] - log(10),
    tolerance = 1e-12
  )
  expect_false(identical(ordinary$metadata$spec_id, scaled$metadata$spec_id))
  expect_identical(scaled$metadata$response_scale_value, 10)
})

test_that("PPML pilot and Morton selector agree with independent donor outputs", {
  oracle <- lv_test_oracle()
  sample <- lv_test_sample()
  points <- oracle$inputs$points
  expect_identical(
    lv_set_ppml_pilot(sample, points[1L, ], points[-1L, , drop = FALSE]),
    oracle$pilot
  )
  grid <- as.matrix(expand.grid(-2:2, -2:2))
  expect_identical(lv_set_morton_select(grid, 7L), oracle$morton)
  expect_error(lv_set_morton_select(matrix(0, 2L, 4L), 1L), class = "hetid_error")
})

test_that("Harvey recession classifications require explicit witness checks", {
  oracle <- lv_test_oracle()
  design <- cbind("(Intercept)" = 1, pc1 = c(-1, 0, 1))
  cases <- list(c(1, 2, 3), c(0, 0, 0), c(0, 1, 0))
  for (i in seq_along(cases)) {
    expect_identical(
      lv_set_recession(cases[[i]], design, lv_set_harvey_control()),
      oracle$recession[[i]]
    )
  }
  expect_identical(lv_set_recession_self_test(design, lv_set_harvey_control()), character())
  fit <- lv_set_harvey_fitter(design)(c(0, 0, 0))
  expect_identical(fit$fit_status, "nonexistence")
  expect_false(log_variance_fit_ok(fit))
})
