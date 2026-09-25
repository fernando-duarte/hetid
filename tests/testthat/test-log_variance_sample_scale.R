test_that("constraint scaling cannot admit points in a disconnected set's gap", {
  parts <- sample_box_fixture()
  box <- parts$box
  box$w1[] <- 3
  box$bounds$lower <- -2
  box$bounds$upper <- 2
  box$arg_lower[, ] <- -2
  box$arg_upper[, ] <- 2
  colnames(box$arg_lower) <- colnames(box$arg_upper) <- colnames(box$w2)
  q <- list(A_i = list(matrix(-1), matrix(1)), b_i = list(0, 0), c_i = c(1, -4))
  for (scale in c(1e-20, 1, 1e20)) {
    box$quadratic <- list(
      A_i = lapply(q$A_i, function(a) a * scale),
      b_i = lapply(q$b_i, function(b) b * scale), c_i = q$c_i * scale
    )
    sampled <- sample_log_variance_set(box, parts$x, n_points = 4)
    expect_equal(sort(drop(unname(sampled$candidates))), c(-2, -1.5, -1, 1, 1.5, 2))
    expect_identical(sampled$bounds, profile_log_variance_set(box, parts$x, n_points = 4))
    predicted <- predict(sampled)
    expect_true(all(abs(predicted$arg_lower) >= 1 & abs(predicted$arg_lower) <= 2))
    expect_true(all(abs(predicted$arg_upper) >= 1 & abs(predicted$arg_upper) <= 2))
  }
})

test_that("response rescaling retains boundary candidates and log-variance widths", {
  d <- simulate_box_dgp()
  get_sample <- function(scale) {
    fit <- compute_tau0_system(scale * d$y1, scale * d$y2, d$x, d$z)
    region <- compute_identified_set_box(fit, 0.05, n_grid = 7)
    # Tighten IRLS accuracy to isolate membership from scale-dependent stopping.
    sample_log_variance_set(region, d$x_var,
      n_points = 3,
      control = list(GLM_EPSILON = 1e-14)
    )
  }
  baseline <- get_sample(1)
  for (scale in c(1e-4, 100, 1000)) {
    sampled <- get_sample(scale)
    expect_equal(sampled$candidates, baseline$candidates, tolerance = 1e-10)
    lower <- predict(sampled)$bounds$lower - 2 * log(scale)
    upper <- predict(sampled)$bounds$upper - 2 * log(scale)
    expect_equal(lower, predict(baseline)$bounds$lower, tolerance = 1e-7)
    expect_equal(upper, predict(baseline)$bounds$upper, tolerance = 1e-7)
  }
})

test_that("nonfinite membership arithmetic fails rather than retaining a point", {
  parts <- sample_box_fixture()
  parts$box$arg_lower[, ] <- -1e308
  parts$box$arg_upper[, ] <- 1e308
  expect_error(sample_log_variance_set(parts$box, parts$x),
    "numeric range",
    class = "hetid_error"
  )
  expect_error(profile_log_variance_set(parts$box, parts$x),
    "numeric range",
    class = "hetid_error"
  )
})


test_that("scalar profile candidates do not use row labels as coefficient names", {
  parts <- sample_box_fixture()
  for (estimator in c("ppml", "harvey")) {
    sampled <- sample_log_variance_set(parts$box, parts$x, estimator, n_points = 2)
    profile <- profile_log_variance_set(parts$box, parts$x, estimator, n_points = 2)
    expect_identical(profile, sampled$bounds)
    expect_true(all(is.finite(profile$lower)))
  }
})
