test_that("successful callback coefficients keep their declared axis", {
  for (bad in list(c(second = 1, first = 2), 1, matrix(1:2, 2))) {
    map <- lv_test_linear()
    map$fit_at_b <- function(b, start = NULL, phase = NULL) {
      lv_set_fit_result(bad, "ok", TRUE)
    }
    expect_error(lv_test_search(map), class = "hetid_error_bad_argument")
  }
  map <- lv_test_linear()
  map$fit_at_b <- function(b, start = NULL, phase = NULL) {
    lv_set_fit_result(unname(drop(lv_test_oracle()$inputs$loading %*% b)), "ok", TRUE)
  }
  expect_true(all(lv_test_search(map)$schema$lower_status == "bounded"))
})

test_that("Jacobian dimensions and named axes are checked before acceptance", {
  loading <- lv_test_oracle()$inputs$loading
  bad <- list(
    matrix(1, 1, 2), loading[2:1, ],
    matrix(1, 2, 2, dimnames = list(NULL, c("b2", "b1")))
  )
  for (jac in bad) {
    map <- lv_test_linear()
    map$jacobian_at_b <- function(b, fit = NULL) jac
    expect_error(lv_test_search(map), class = "hetid_error_bad_argument")
  }
  map <- lv_test_linear()
  map$jacobian_at_b <- function(b, fit = NULL) unname(loading)
  expect_true(all(lv_test_search(map)$schema$lower_status == "bounded"))
})

test_that("all built-in maps preserve the existing optional theta-name contract", {
  sample <- lv_test_sample()
  point <- c(b1 = 0, b2 = 0)
  wrong <- c(b2 = 0.1, b1 = -0.1)
  ppml <- make_log_variance_map(sample, "ppml", point)
  start <- stats::lm.fit(sample$x_mat, log(sample$ols_residuals^2))$coefficients
  maps <- list(ppml, make_log_variance_map(sample, "harvey", point,
    ppml = ppml, logols_coef = start
  ), make_log_variance_map(sample, "logols"))
  for (map in maps) {
    expect_error(map$fit_at_b(wrong), class = "hetid_error_bad_argument")
    expect_error(map$jacobian_at_b(wrong), class = "hetid_error_bad_argument")
    expect_identical(map$fit_at_b(point)$coef, map$fit_at_b(unname(point))$coef)
  }
  expect_error(make_log_variance_map(sample, "ppml", wrong), class = "hetid_error_bad_argument")
  expect_error(make_log_variance_map(sample, "ppml", point, anchor = wrong),
    class = "hetid_error_bad_argument"
  )
  expect_error(make_log_variance_map(sample, "harvey", wrong, ppml = ppml, logols_coef = start),
    class = "hetid_error_bad_argument"
  )
  input <- lv_test_oracle()$inputs
  expect_error(search_log_variance_map(lv_test_linear(), input$quadratic,
    input$table,
    seed = wrong, control = input$control
  ), class = "hetid_error_bad_argument")
  expect_error(lv_test_search(extra_starts = list(wrong)), class = "hetid_error_bad_argument")
  expect_identical(
    vapply(maps, function(map) map$metadata$target_functional, character(1)),
    c("theta_var", "theta_var_gaussian", "theta_log")
  )
})
