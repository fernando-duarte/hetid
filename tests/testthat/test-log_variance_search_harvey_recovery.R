test_that("Harvey aggregates allow native recovery from a tentative Fisher failure", {
  v <- rep(seq(-1, 1, length.out = 15L), each = 2L)
  polarity <- rep(c(-1, 1), length.out = length(v))
  response <- exp(0.2 * v)
  small <- exp(-10 + 0.2 * v)
  sample_data <- prepare_log_variance_search(
    polarity * sqrt(small),
    cbind(news = polarity * (sqrt(small) - sqrt(response))), cbind(v = v),
    seq_along(v), seq_along(v),
    ols_residuals = polarity * sqrt(response)
  )
  tau <- 0.05
  key <- sprintf("%.17g", tau)
  radius <- 1e-4
  systems_at <- function(center) {
    stats::setNames(list(list(
      A_i = list(matrix(1, 1L, 1L)), b_i = list(-2 * center),
      c_i = center^2 - radius^2
    )), key)
  }
  tables_at <- function(center) {
    stats::setNames(list(data.frame(
      coef = "news", status = "bounded",
      outer_lower = center - radius, outer_upper = center + radius
    )), key)
  }
  controls <- list(
    search = log_variance_search_control(), map = log_variance_map_control("harvey"),
    fit = LOG_VARIANCE_HARVEY_CONTROL
  )
  ppml <- profile_log_variance_map(sample_data, systems_at(0), tables_at(0),
    tau, "ppml",
    point = c(news = 0)
  )
  start <- ppml$estimator$start_bundle$coef_original
  expect_equal(unname(start), c(-10, 0.2), tolerance = 1e-6)
  expect_identical(precheck_harvey_starts(
    list(anchor_ppml = list(y = response, start = start)), sample_data$x_mat
  ), c(anchor_ppml = "proposal_nonfinite"))
  native <- fit_log_variance(response, sample_data$pcr, "harvey",
    start = start,
    control = list(AUTO_INTERCEPT = FALSE)
  )
  expect_true(log_variance_fit_ok(native))
  expect_equal(unname(native$coef), c(0, 0.2), tolerance = 1e-6)
  expect_length(native$diagnostics$start_attempts, 1L)
  notices <- character()
  result <- with_rng_scope({
    set.seed(71L)
    before <- .Random.seed
    answer <- withCallingHandlers(
      profile_log_variance_map(sample_data, systems_at(1), tables_at(1),
        tau, "harvey",
        point = c(news = 1), ppml = ppml
      ),
      message = function(condition) {
        notices <<- c(notices, conditionMessage(condition))
        invokeRestart("muffleMessage")
      }
    )
    expect_identical(.Random.seed, before)
    answer
  })
  expect_identical(result$stability_precheck, list(
    passed = FALSE,
    reasons = c(
      ref_intercept = NA_character_, anchor_ppml = "proposal_nonfinite",
      anchor_intercept = NA_character_
    )
  ))
  expect_true(any(grepl("anchor_ppml proposal_nonfinite", notices, fixed = TRUE)))
  expect_true(any(grepl("existing fitter", notices, fixed = TRUE)))
  expect_true(log_variance_fit_ok(result$estimator$point_fit))
  expect_equal(unname(result$estimator$point_fit$coef), c(0, 0.2), tolerance = 1e-6)
  expect_identical(names(result$results), key)
  expect_gt(result$results[[key]]$n_feasible, 0)
  expect_identical(controls, list(
    search = log_variance_search_control(), map = log_variance_map_control("harvey"),
    fit = LOG_VARIANCE_HARVEY_CONTROL
  ))
})

test_that("tentative Harvey reasons preserve fatal and required-reference gates", {
  sample_data <- lv_test_sample()
  point <- c(0, 0)
  arguments <- list(
    sample = sample_data, context = list(point = point, anchor = point, seed = point),
    path = list(), bounds = list(theta = list()), tau_control = list(display = 0.05),
    control = log_variance_search_control(),
    ppml = list(estimator = make_log_variance_map(sample_data, "ppml", point))
  )
  fitter_calls <- search_calls <- 0L
  reasons <- c(anchor_ppml = NA_character_)
  testthat::local_mocked_bindings(
    precheck_harvey_starts = function(...) reasons,
    lv_set_harvey_fitter = function(...) {
      fitter_calls <<- fitter_calls + 1L
      function(...) lv_set_failure("nonconvergence", "reference_unavailable")
    },
    lv_set_display_map = function(...) {
      search_calls <<- search_calls + 1L
      stop("unexpected search")
    }, .package = "hetid"
  )
  for (reason in c(
    "nonfinite_start_eval", "nonpositive_mu", "nonfinite_info", "proposal_nonfinite"
  )) {
    reasons <- c(anchor_ppml = reason)
    fitter_calls <- 0L
    expect_message(
      expect_error(do.call(lv_set_harvey_sets, arguments), "reference_unavailable",
        class = "hetid_error"
      ), reason
    )
    expect_gt(fitter_calls, 0L)
    expect_identical(search_calls, 0L)
  }
  for (reasons in list(
    c(reference = "invalid_response"), c(anchor_ppml = "invalid_start"),
    c(anchor_ppml = "unknown_reason"),
    c(anchor_ppml = "proposal_nonfinite", reference = "invalid_response")
  )) {
    fitter_calls <- 0L
    expect_error(do.call(lv_set_harvey_sets, arguments), tail(reasons, 1L),
      class = "hetid_error"
    )
    expect_identical(fitter_calls, 0L)
    expect_identical(search_calls, 0L)
  }
})

test_that("Harvey aggregate recovery preserves all-zero and recession refusals", {
  sample_data <- lv_test_sample()
  ppml <- make_log_variance_map(sample_data, "ppml", c(0, 0))
  sample_data$ols_residuals[] <- 0
  searched <- FALSE
  testthat::local_mocked_bindings(lv_set_display_map = function(...) {
    searched <<- TRUE
    stop("unexpected search")
  }, .package = "hetid")
  expect_error(lv_set_harvey_sets(
    sample_data,
    list(point = c(0, 0), anchor = c(0, 0)), list(), list(theta = list()),
    list(display = 0.05), log_variance_search_control(), list(estimator = ppml)
  ), "ref_intercept invalid_start", class = "hetid_error")
  expect_false(searched)
  design <- cbind("(Intercept)" = 1, v = c(-1, 0, 1))
  for (response in list(c(0, 0, 0), c(0, 1, 0))) {
    fit <- lv_set_harvey_fitter(design)(response)
    expect_false(log_variance_fit_ok(fit))
    expect_null(fit$coef)
    expect_true(fit$fit_status %in% c("nonexistence", "nonconvergence"))
  }
})

test_that("all-pass Harvey aggregates retain their previous numerical evidence", {
  oracle <- lv_test_path_oracle()
  sample_data <- lv_test_sample()
  ppml <- profile_log_variance_map(
    sample_data, oracle$quadratics, oracle$theta_tables,
    oracle$taus, "ppml", c(0, 0), oracle$control
  )
  notices <- character()
  result <- withCallingHandlers(
    profile_log_variance_map(sample_data, oracle$quadratics, oracle$theta_tables,
      oracle$taus, "harvey", c(0, 0), oracle$control,
      ppml = ppml
    ),
    message = function(condition) {
      notices <<- c(notices, conditionMessage(condition))
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(notices, character())
  expect_identical(result$stability_precheck, list(
    passed = TRUE,
    reasons = c(
      ref_intercept = NA_character_, anchor_ppml = NA_character_,
      anchor_intercept = NA_character_
    )
  ))
  expected <- oracle$harvey
  keep <- names(expected)[!is.na(names(expected))]
  expect_identical(lv_test_core(result[keep]), lv_test_core(expected[keep]))
  expect_identical(result$estimator$point_fit, oracle$harvey_point_fit)
})
