test_that("malformed batch failure counts cannot commit state or start later fitting", {
  input <- lv_test_oracle()$inputs
  invalid <- list(NULL, NA_real_, NaN, Inf, -1L, 0.5, "0", c(0L, 1L), matrix(0L))
  for (no_fit in c(FALSE, TRUE)) {
    for (count in invalid) {
      calls <- c(scan = 0L, fit = 0L)
      map <- lv_test_linear()
      map$fit_at_b <- function(...) calls[["fit"]] <<- calls[["fit"]] + 1L
      map$scan_grid <- function(mesh) {
        calls[["scan"]] <<- calls[["scan"]] + 1L
        list(
          min = if (no_fit) NULL else c(0, 0), max = c(0, 0),
          arg_min = matrix(0, 2L, 2L), arg_max = matrix(0, 2L, 2L), n_failed = count
        )
      }
      cache <- new.env(parent = emptyenv())
      lv_set_cache_bind(cache, map$metadata)
      budget <- lv_set_budget(1000L)
      before <- as.list(budget)
      store <- as.list(cache$store)
      expect_error(search_log_variance_map(map, input$quadratic, input$table,
        cache = cache, budget = budget, control = input$control
      ), class = "hetid_error_bad_argument")
      expect_identical(calls, c(scan = 1L, fit = 0L))
      expect_identical(as.list(budget), before)
      expect_identical(as.list(cache$store), store)
    }
  }
})

test_that("valid batch counts retain closures, final counters and one callback", {
  input <- lv_test_oracle()$inputs
  radius <- sqrt(-input$quadratic$c_i[[1L]])
  endpoint <- radius * sqrt(rowSums(input$loading^2))
  for (no_fit in c(FALSE, TRUE)) {
    for (count in list(0L, 0, 1L, 1)) {
      calls <- 0L
      rows <- NULL
      observed <- NULL
      budget <- lv_set_budget(1000L)
      map <- lv_test_linear()
      map$scan_grid <- function(mesh) {
        calls <<- calls + 1L
        rows <<- nrow(mesh)
        observed <<- as.list(budget)
        values <- mesh %*% t(input$loading)
        low <- apply(values, 2L, which.min)
        high <- apply(values, 2L, which.max)
        list(
          min = if (no_fit) NULL else apply(values, 2L, min),
          max = apply(values, 2L, max), arg_min = mesh[low, , drop = FALSE],
          arg_max = mesh[high, , drop = FALSE], n_failed = count
        )
      }
      before <- as.list(budget)
      result <- search_log_variance_map(map, input$quadratic, input$table,
        budget = budget, control = input$control
      )
      expect_identical(calls, 1L)
      expect_identical(observed, before)
      expect_identical(result$schema$fit_failure_count, rep(count, 2L))
      expect_equal(budget$n_failed, count)
      expect_equal(result$diagnostics$n_failed, count)
      expect_identical(budget$counters[["scan"]], rows)
      expect_gte(budget$n_evaluated, rows)
      expect_identical(budget$n_attempted, budget$n_evaluated + budget$n_cached)
      if (count > 0 || no_fit) {
        expect_true(all(result$schema$lower_status == "unreliable"))
        expect_true(all(result$schema$upper_status == "unreliable"))
        expect_identical(budget$n_attempted, rows)
        expect_identical(budget$n_evaluated, rows)
        if (count > 0) {
          expect_identical(result$diagnostics$scan_fit_failures, count)
        } else {
          expect_true(result$diagnostics$no_successful_fits)
        }
      } else {
        expect_true(all(result$schema$lower_status == "bounded"))
        expect_true(all(result$schema$upper_status == "bounded"))
        expect_equal(result$schema$lower, unname(-endpoint), tolerance = 1e-6)
        expect_equal(result$schema$upper, unname(endpoint), tolerance = 1e-6)
      }
    }
  }
})

test_that("shared batch failure counts accumulate once per validated scan", {
  input <- lv_test_oracle()$inputs
  map <- lv_test_linear()
  map$scan_grid <- function(mesh) list(min = NULL, n_failed = 1L)
  budget <- lv_set_budget(1000L)
  budget$n_failed <- 2L
  budget$n_evaluated <- budget$n_attempted <- 2L
  budget$counters[["scan"]] <- 2L
  for (expected in 3:4) {
    result <- search_log_variance_map(map, input$quadratic, input$table,
      budget = budget, control = input$control
    )
    expect_identical(budget$n_failed, expected)
    expect_identical(result$diagnostics$n_failed, expected)
    expect_identical(result$schema$fit_failure_count, rep(1L, 2L))
  }
})
