test_that("direct cold flags and tau labels are validated before fitting", {
  calls <- 0L
  map <- lv_test_linear(cold = TRUE)
  underlying <- map$fit_at_b
  map$fit_at_b <- function(...) {
    calls <<- calls + 1L
    underlying(...)
  }
  input <- lv_test_oracle()$inputs
  for (flag in list(NA, c(TRUE, FALSE), "TRUE", 1)) {
    expect_error(lv_test_search(map, cold_start_check = flag), class = "hetid_error_bad_argument")
  }
  for (tau in list(c(0.1, 0.2), numeric(), Inf, NaN, "0.1")) {
    expect_error(search_log_variance_map(map, input$quadratic, input$table,
      tau = tau, control = input$control
    ), class = "hetid_error_bad_argument")
  }
  expect_identical(calls, 0L)
  expect_true(all(lv_test_search(map, cold_start_check = FALSE)$schema$lower_status == "bounded"))
  checked <- lv_test_search(map, cold_start_check = TRUE)
  expect_true(all(checked$schema$lower_status == "unreliable"))
  expect_true(all(is.na(search_log_variance_map(lv_test_linear(), input$quadratic,
    input$table,
    control = input$control
  )$schema$tau)))
})

test_that("cache and shared budgets require valid mutable state", {
  expect_error(lv_test_search(cache = list()), class = "hetid_error_bad_argument")
  expect_error(lv_test_search(cache = emptyenv()), class = "hetid_error_bad_argument")
  cache <- new.env(parent = emptyenv())
  cache$sample_id <- "partial"
  expect_error(lv_test_search(cache = cache), class = "hetid_error_bad_argument")
  input <- lv_test_oracle()$inputs
  run <- function(budget) {
    search_log_variance_map(lv_test_linear(), input$quadratic,
      input$table,
      budget = budget, control = input$control
    )
  }
  expect_error(run(new.env(parent = emptyenv())), class = "hetid_error_bad_argument")
  for (field in c("n_attempted", "n_evaluated", "n_cached", "n_failed")) {
    state <- lv_set_budget()
    state[[field]] <- -1L
    expect_error(run(state), class = "hetid_error_bad_argument")
  }
  state <- lv_set_budget()
  state$counters[[1L]] <- 1L
  expect_error(run(state), class = "hetid_error_bad_argument")
  state <- lv_set_budget(0L)
  expect_true(run(state)$diagnostics$budget_exhausted)
  first <- lv_test_search()
  before <- first$budget$n_attempted
  second <- search_log_variance_map(lv_test_linear(), input$quadratic, input$table,
    seed = c(0, 0), max_grid_points = 30L, budget = first$budget, cache = first$cache,
    tau = 0.1, control = input$control
  )
  expect_identical(second$budget, first$budget)
  expect_gt(second$budget$n_attempted, before)
  expect_identical(second$schema, first$schema)
})

test_that("explicit objective and gradient budget signals fail closed", {
  for (trigger in c("fn", "gr")) {
    calls <- c(fn = 0L, gr = 0L)
    hit <- FALSE
    after_hit <- 0L
    map <- lv_test_linear()
    loading <- lv_test_oracle()$inputs$loading
    map$coef_objective <- function(j) {
      force(j)
      invoke <- function(kind, b) {
        if (hit) after_hit <<- after_hit + 1L
        calls[[kind]] <<- calls[[kind]] + 1L
        if (kind == trigger) {
          hit <<- TRUE
          lv_set_budget_stop("polish", "explicit callback sentinel")
        }
        if (kind == "fn") sum(loading[j, ] * b) else unname(loading[j, ])
      }
      list(fn = function(b) invoke("fn", b), gr = function(b) invoke("gr", b))
    }
    result <- lv_test_search(map)
    expect_true(result$diagnostics$budget_exhausted)
    expect_true(all(result$schema$lower_status == "unreliable"))
    expect_true(all(result$schema$upper_status == "unreliable"))
    expect_identical(calls[[trigger]], 1L)
    expect_identical(after_hit, 0L)
    expect_match(result$diagnostics$budget_message, "explicit callback sentinel")
  }
  expect_true(lv_test_search(budget = 30L)$diagnostics$budget_exhausted)
})
