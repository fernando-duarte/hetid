test_that("the engine preserves the independent donor baseline", {
  oracle <- lv_test_oracle()
  result <- lv_test_search()
  expect_oracle_equal(
    lv_drop_route(result[names(oracle$engine)]), lv_drop_route(oracle$engine),
    ORACLE_TOLERANCE[["solver"]]
  )
  expect_effort_consistent(result)
  expected <- 0.5 * sqrt(rowSums(oracle$inputs$loading^2))
  expect_equal(result$schema$lower, -unname(expected), tolerance = 1e-6)
  expect_equal(result$schema$upper, unname(expected), tolerance = 1e-6)
  expect_true(all(result$schema$lower_status == "bounded"))
  expect_true(all(result$schema$upper_status == "bounded"))
  expect_lte(
    max(result$schema$lower_constraint_residual),
    oracle$inputs$control$sets$FEASIBILITY_TOLERANCE
  )
})

test_that("budget, failed fits and cold disagreement have distinct evidence", {
  empty <- lv_test_search(budget = 0L)
  expect_true(empty$diagnostics$budget_exhausted)
  expect_identical(empty$diagnostics$n_evaluated, 0L)
  starved <- lv_test_search(budget = 2L)
  expect_true(starved$diagnostics$budget_exhausted)
  expect_identical(starved$diagnostics$n_evaluated, 2L)
  expect_true(all(is.na(starved$schema$lower)))
  interrupted <- lv_test_search(lv_test_linear(fail = TRUE), budget = 1L)
  expect_identical(interrupted$diagnostics$n_failed, 1L)
  expect_true(all(interrupted$schema$fit_failure_count == 2L))
  expect_identical(interrupted$diagnostics$n_cached, 1L)
  cold <- lv_test_search(lv_test_linear(cold = TRUE))
  expect_true(all(cold$schema$lower_status == "unreliable"))
  expect_true(all(is.finite(cold$schema$lower)))
  expect_length(cold$diagnostics$cold_start, 4L)
})

test_that("warm caches are bound and cold fits bypass cache state", {
  first <- lv_test_search()
  keys <- ls(first$cache$store, all.names = TRUE)
  cached <- as.list(first$cache$store, all.names = TRUE)
  second <- lv_test_search(cache = first$cache)
  expect_gt(second$diagnostics$n_cached, 0L)
  expect_identical(ls(first$cache$store, all.names = TRUE), keys)
  expect_identical(as.list(first$cache$store, all.names = TRUE), cached)
  expect_equal(second$schema, first$schema)
  changed <- lv_test_linear()
  changed$metadata$spec_id <- "another"
  expect_error(lv_test_search(changed, cache = first$cache), class = "hetid_error")
})

test_that("unresolved domain endpoints outrank divergence", {
  map <- lv_test_linear(sides = function(scan, precheck) {
    list(
      lower_unbounded = c(TRUE, FALSE), upper_unbounded = c(FALSE, TRUE),
      unresolved_endpoints = "first:min", info = list(reason = "unresolved-test")
    )
  })
  result <- lv_test_search(map)
  expect_identical(result$schema$lower_status, c("unreliable", "bounded"))
  expect_identical(result$schema$upper_status, c("bounded", "unbounded"))
  expect_identical(result$diagnostics$domain$info$reason, "unresolved-test")
  expect_identical(result$schema$upper[[2L]], Inf)
})

test_that("derived overflow is numerical and a containing box is not an endpoint", {
  input <- lv_test_oracle()$inputs
  control <- input$control
  control$sets$SOLVER_BOXES <- rep(.Machine$double.xmax, 3L)
  qs <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -4)
  polished <- lv_set_polish(qs, "min", 0, 1, identity, function(x) 1, control)
  expect_null(polished$bound)
  box <- input$table
  box$set_lower <- box$outer_lower / 2
  box$set_upper <- box$outer_upper / 2
  expect_identical(profile_containing_box(box)$lower, input$table$outer_lower)
})
