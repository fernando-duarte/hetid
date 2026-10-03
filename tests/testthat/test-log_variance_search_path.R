test_that("audited PPML and Harvey paths preserve the independent donor results", {
  oracle <- lv_test_path_oracle()
  sample <- lv_test_sample()
  maps <- list()
  for (method in c("ppml", "harvey")) {
    maps[[method]] <- profile_log_variance_map(sample, oracle$quadratics,
      oracle$theta_tables, oracle$taus, method, c(0, 0), oracle$control,
      ppml = maps$ppml
    )
    expected <- oracle[[method]]
    keep <- names(expected)[!is.na(names(expected))]
    expect_identical(lv_test_core(maps[[method]][keep]), lv_test_core(expected[keep]))
    for (result in maps[[method]]$results) {
      expect_identical(result$schema$sample_id, rep(sample$sample_id, 3L))
    }
  }
  expect_identical(maps$harvey$estimator$point_fit, oracle$harvey_point_fit)
  expect_identical(maps$ppml$selector_provenance$status, "verified")
  expect_identical(maps$ppml$selector_provenance$traversal, "as_selected")
  expect_identical(maps$ppml$selector_provenance$n_verified, 2L)
  path <- profile_log_variance_path(
    maps$ppml, oracle$quadratics,
    oracle$theta_tables, oracle$taus, oracle$control
  )
  expect_identical(path, oracle$ppml_path)
})

test_that("the existing log projection preserves donor endpoint semantics", {
  oracle <- lv_test_path_oracle()
  sample <- lv_test_sample()
  result <- profile_log_variance_map(
    sample, oracle$quadratics, oracle$theta_tables,
    oracle$taus, "logols", c(0, 0), oracle$control
  )
  expect_identical(names(result$results), names(oracle$logols$results))
  for (key in names(result$results)) {
    actual <- result$results[[key]]$schema
    expected <- oracle$logols$results[[key]]$schema
    fields <- c("coef", "lower_status", "upper_status", "lower_source", "upper_source", "tau")
    expect_identical(actual[fields], expected[fields])
    for (side in c("lower", "upper")) {
      expect_identical(is.na(actual[[side]]), is.na(expected[[side]]))
      expect_identical(is.infinite(actual[[side]]), is.infinite(expected[[side]]))
      finite <- is.finite(expected[[side]])
      expect_identical(actual[[side]][!finite], expected[[side]][!finite])
      expect_true(all(abs(actual[[side]][finite] - expected[[side]][finite]) <= 1e-6))
    }
  }
})
