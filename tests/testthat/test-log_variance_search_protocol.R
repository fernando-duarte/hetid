test_that("coverage provenance is owned by completed engine diagnostics", {
  result <- lv_test_search()
  none <- lv_set_selector_provenance(list(a = list(ok = TRUE, result = result)))
  expect_identical(none$status, "not_applicable")
  failed <- lv_set_selector_provenance(
    list(a = list(ok = FALSE, error = "failed")),
    "morton-v1"
  )
  expect_identical(failed$status, "all_failed")
  expect_identical(failed$n_verified, 0L)
  closed <- list(a = list(ok = TRUE, result = lv_test_search(budget = 0L)))
  closed$a$result$diagnostics$n_raw_feasible <- NA_integer_
  expect_identical(lv_set_selector_provenance(closed, "morton-v1")$status, "all_failed")
  selected <- lv_test_search(grid_selector = lv_set_morton_select)
  audit <- list(a = list(ok = TRUE, result = selected))
  expect_identical(lv_set_selector_provenance(audit, "morton-v1")$status, "verified")
  audit$a$result$diagnostics$selector$traversal <- "engine_default"
  expect_error(lv_set_selector_provenance(audit, "morton-v1"), class = "hetid_error")
  expect_error(lv_set_selector_provenance(
    list(a = list(ok = TRUE, result = result)),
    "morton-v1"
  ), class = "hetid_error")
})

test_that("audit disagreement cannot upgrade endpoints", {
  primary <- list(a = lv_test_search())
  audit <- list(a = list(ok = TRUE, result = primary$a))
  audit$a$result$schema$lower[[1L]] <- primary$a$schema$lower[[1L]] - 1
  applied <- lv_set_audit_apply(primary, audit)
  expect_identical(applied$results$a$schema$lower_status[[1L]], "unreliable")
  expect_identical(
    applied$results$a$schema$lower[[1L]],
    audit$a$result$schema$lower[[1L]]
  )
  expect_identical(applied$audit$reason[[1L]], "endpoint_moved")
  audit$a <- list(ok = FALSE, error = "deliberate")
  expect_warning(failed <- lv_set_audit_apply(primary, audit), "deliberate")
  expect_true(all(failed$results$a$schema$lower_status == "unreliable"))
})

test_that("sample identity, named alignment and full mean sample are retained", {
  oracle <- lv_test_oracle()
  input <- oracle$inputs
  n <- length(input$sample$w1)
  rows <- c(10:20, 2:8)
  sample <- prepare_log_variance_search(
    input$sample$w1, input$sample$w2,
    input$raw[rows, ], paste0("id", 1:n), paste0("id", rows)
  )
  expect_identical(sample$prep$w1_mean, input$sample$w1)
  expect_identical(sample$w1, input$sample$w1[rows])
  expect_identical(sample$date, paste0("id", rows))
  changed <- sample
  changed$w1[[1L]] <- changed$w1[[1L]] + 1
  expect_error(make_log_variance_map(changed, "logols"), class = "hetid_error")
  expect_error(prepare_log_variance_search(
    input$sample$w1, input$sample$w2,
    input$raw[rows, ], 1:n, rep(1L, length(rows))
  ), class = "hetid_error")
})

test_that("the search restores caller RNG and rejects unsupported hooks", {
  with_rng_scope({
    RNGkind("L'Ecuyer-CMRG")
    set.seed(104L)
    before <- .Random.seed
    lv_test_search()
    expect_identical(.Random.seed, before)
    expect_error(log_variance_map_control("unknown"), class = "hetid_error")
    map <- lv_test_linear()
    map$analyze_domain <- list(sides = function(...) NULL)
    expect_error(lv_test_search(map), class = "hetid_error")
    expect_identical(.Random.seed, before)
  })
})
