test_that("a triggered PPML pilot rescales the response by its positive median", {
  oracle <- lv_test_oracle()
  sample <- lv_test_sample()
  points <- oracle$inputs$points
  anchor <- points[1L, ]
  control <- lv_set_ppml_control()
  control$PILOT_CONDITION_LIMIT <- 0
  pilot <- lv_set_ppml_pilot(sample, anchor, points[-1L, , drop = FALSE], control)
  response <- drop(sample$w1 - sample$w2 %*% anchor)^2
  expect_identical(pilot$n_triggered, pilot$n_fits)
  expect_identical(pilot$response_scale, stats::median(response[response > 0]))
  expect_false(pilot$response_scale == 1)
  # an anchor that fits w1 exactly leaves no positive response to scale by
  exact <- sample
  exact$w1 <- drop(sample$w2 %*% anchor)
  expect_error(lv_set_ppml_pilot(exact, anchor, NULL, control),
    "needs a scale",
    class = "hetid_error"
  )
})

test_that("rejected PPML Jacobians reach the search as NaN matrices", {
  oracle <- lv_test_oracle()
  sample <- lv_test_sample()
  b <- oracle$inputs$points[1L, ]
  control <- log_variance_map_control("ppml")
  control$JACOBIAN_RCOND_TOL <- 2
  map <- make_log_variance_map(sample, "ppml", b, b, control = control)
  fit <- map$fit_at_b(b)
  expect_true(log_variance_fit_ok(fit))
  expect_null(map$jacobian_at_b(b, fit))
  jacobian <- lv_set_checked_jacobian(map, b, fit)
  expect_identical(dim(jacobian), c(length(map$coef_labels), length(b)))
  expect_true(all(is.nan(jacobian)))
})

# The degenerate-norm and chol arms stay untested: the polish path asks for
# a Jacobian only after log_variance_fit_ok, ppml_acceptance() already rejects
# the same information norms for an accepted fit on the same design, and the
# chol failure is a floating-point backstop with no constructed input.
test_that("a failed PPML fit passed to the public map has no Jacobian", {
  w2 <- cbind(news = c(-1, 1, -1, 1))
  w1 <- drop(w2)
  sample <- prepare_log_variance_search(
    w1, w2, matrix(numeric(), 4L, 0L), 1:4, 1:4,
    ols_residuals = w1
  )
  map <- make_log_variance_map(sample, "ppml", point = 0, anchor = 0)
  failed <- map$fit_at_b(1)
  expect_identical(failed$fit_status, "nonconvergence")
  expect_null(map$jacobian_at_b(1, failed))
  expect_true(is.matrix(map$jacobian_at_b(0, map$fit_at_b(0))))
})
