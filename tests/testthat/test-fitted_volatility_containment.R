test_that("point overflow respects bounded containment and preserves other diagnostics", {
  for (scenario in c("isolated", "mixed", "non_bounded", "non_bounded_mixed")) {
    sets <- fv_test_sets()$sets
    geometry <- fv_test_system()
    source_coef <- stats::setNames(c(0, 1e4, 0), sets$estimator$coef_labels)
    source_fit <- lv_set_fit_result(source_coef, "ok", TRUE)
    expect_true(log_variance_fit_ok(source_fit))
    expect_true(all(is.finite(source_coef)))
    sets$estimator$fit_at_b <- function(b, start = NULL, phase = NULL) source_fit
    sets$estimator$jacobian_at_b <- function(...) matrix(0, 3, 1)
    design <- sets$sample$x_mat
    design[, HETID_CONSTANTS$INTERCEPT_LABEL] <- 0
    point_eta <- unname(drop(design %*% source_coef))
    overflow <- point_eta / 2 > log(.Machine$double.xmax)
    expect_true(all(is.finite(point_eta)))
    expect_true(all(overflow | point_eta < 0))
    status <- switch(scenario,
      isolated = c("bounded", rep("unreliable", 11L)),
      mixed = rep("bounded", 12L),
      non_bounded = rep("unreliable", 12L),
      non_bounded_mixed = ifelse(overflow, "unreliable", "bounded")
    )
    # An injected missed endpoint exercises the observable containment diagnostic.
    injected <- list(
      schema = data.frame(
        coef = sprintf("date_%04d", seq_along(sets$sample$response_date)),
        lower = 0, upper = 1, lower_status = status, upper_status = status,
        lower_source = "polish", upper_source = "polish"
      ),
      diagnostics = list(n_evaluated = 1L, n_raw_feasible = 7L)
    )
    original <- injected
    local_mocked_bindings(search_log_variance_map = function(...) injected)
    expected_dates <- sets$sample$response_date[status == "bounded"]
    point <- c(news = 0)
    expect_true(lv_set_point_feasible(geometry$quadratic, point))
    error <- tryCatch(profile_fitted_volatility(
      sets, geometry$quadratic, geometry$table, 0.05,
      point = point, control = fv_test_control()
    ), error = identity)
    if (scenario == "non_bounded") {
      expect_type(error, "list")
      expect_identical(error$data$log_variance_point, point_eta)
      expect_true(all(is.na(error$data$volatility_point[overflow])))
      expect_identical(error$data$volatility_point[!overflow], exp(0.5 * point_eta[!overflow]))
      expect_identical(error$data$lower_status, status)
      expect_identical(error$data$upper_status, status)
      expect_identical(error$point_status, "ok")
    } else {
      expect_gt(length(expected_dates), 0L)
      expect_s3_class(error, "hetid_error_numerical")
      expect_match(conditionMessage(error), "lies outside its fitted volatility band",
        fixed = TRUE
      )
      expect_identical(error$tau, 0.05)
      expect_identical(error$date, expected_dates)
      expect_false(anyNA(error$date))
    }
    expect_identical(injected, original)
    expect_identical(source_fit$coef, source_coef)
  }
})
