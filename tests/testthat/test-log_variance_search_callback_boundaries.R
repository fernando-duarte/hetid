lv_test_batch_pool <- function(pool = NULL, field = "arg_min_pool") {
  map <- lv_test_linear()
  loading <- lv_test_oracle()$inputs$loading
  map$scan_grid <- function(mesh) {
    values <- loading %*% t(mesh)
    found <- list(
      min = apply(values, 1L, min), max = apply(values, 1L, max),
      arg_min = unname(mesh[apply(values, 1L, which.min), , drop = FALSE]),
      arg_max = unname(mesh[apply(values, 1L, which.max), , drop = FALSE]), n_failed = 0L
    )
    found[field] <- list(pool)
    found
  }
  map
}

test_that("batch scan pools validate theta candidates before refinement", {
  for (field in c("arg_min_pool", "arg_max_pool")) {
    for (point in list(c(b2 = 0.2, b1 = -0.2), 0.2, c(0.2, Inf), matrix(c(0.2, -0.2)))) {
      expect_error(lv_test_search(lv_test_batch_pool(list(list(point), list()), field)),
        class = "hetid_error_bad_argument"
      )
    }
  }
  baseline <- lv_test_search(lv_test_batch_pool())
  for (pool in list(list(), list(NULL, list()))) {
    result <- lv_test_search(lv_test_batch_pool(pool))
    expect_identical(result$schema, baseline$schema)
    expect_identical(result$diagnostics, baseline$diagnostics)
  }
  named <- lv_test_search(lv_test_batch_pool(list(list(c(b1 = 0.2, b2 = -0.2)), list())))
  unnamed <- lv_test_search(lv_test_batch_pool(list(list(c(0.2, -0.2)), list())))
  partial <- lv_test_search(lv_test_batch_pool(list(list(c(0.2, -0.2)))))
  expect_identical(named$schema, unnamed$schema)
  expect_identical(named$diagnostics, unnamed$diagnostics)
  expect_identical(partial$schema, unnamed$schema)
  expect_identical(partial$diagnostics, unnamed$diagnostics)
})

test_that("nonfinite optimizer trials stay numerical while callback errors survive", {
  state <- new.env(parent = emptyenv())
  state$condition <- NULL
  calls <- 0L
  callback <- function(b) {
    calls <<- calls + 1L
    stop_bad_argument("deliberate callback error", "estimator")
  }
  guarded <- lv_set_guard_callback(callback, state, NULL)
  for (b in c(NA_real_, NaN, Inf, -Inf)) expect_true(is.nan(guarded(b)))
  expect_identical(calls, 0L)
  expect_null(state$condition)
  expect_true(is.nan(guarded(0)))
  expect_s3_class(state$condition, "hetid_error_bad_argument")
  expect_identical(conditionMessage(state$condition), "deliberate callback error")
})

test_that("selector shape and identity are checked before key construction", {
  for (selected in list(
    1, NULL, list(grid = 1, selector_id = "bad"),
    list(grid = NULL, selector_id = "bad"),
    list(grid = matrix(numeric(), 0L, 2L), selector_id = "bad"),
    list(grid = matrix("0", 1L, 2L), selector_id = "bad")
  )) {
    selector <- function(mesh, max_points) selected
    expect_error(lv_test_search(grid_selector = selector), class = "hetid_error_bad_argument")
  }
  for (id in list(NULL, 1, "", NA_character_, c("one", "two"))) {
    selector <- function(mesh, max_points) list(grid = mesh, selector_id = id)
    expect_error(lv_test_search(grid_selector = selector), class = "hetid_error_bad_argument")
  }
  for (change in c("duplicate", "outside")) {
    selector <- function(mesh, max_points) {
      chosen_grid <- if (change == "duplicate") rbind(mesh, mesh[1L, ]) else mesh + 10
      list(grid = chosen_grid, selector_id = "invalid-v1")
    }
    expect_error(lv_test_search(grid_selector = selector), class = "hetid_error_bad_argument")
  }
  seen <- chosen <- character()
  map <- lv_test_linear()
  fit <- map$fit_at_b
  map$fit_at_b <- function(b, start = NULL, phase = NULL) {
    if (phase == "scan") seen <<- c(seen, lv_set_b_key(b))
    fit(b, start, phase)
  }
  result <- lv_test_search(map, grid_selector = function(mesh, max_points) {
    chosen_grid <- mesh[rev(seq_len(nrow(mesh))), , drop = FALSE]
    chosen <<- apply(chosen_grid, 1L, lv_set_b_key)
    list(grid = chosen_grid, selector_id = "reverse-v1")
  })
  expect_identical(seen, unname(chosen))
  expect_identical(result$diagnostics$selector$selector_id, "reverse-v1")
  expect_identical(result$diagnostics$selector$traversal, "as_selected")
})

test_that("domain flags retain coefficient identity and unresolved precedence", {
  for (field in c("lower_unbounded", "upper_unbounded")) {
    for (flags in list(
      c(second = TRUE, first = FALSE), c(TRUE, NA), 1:2,
      TRUE, matrix(c(TRUE, FALSE))
    )) {
      map <- lv_test_linear(sides = function(scan, precheck) {
        sides <- list(lower_unbounded = c(FALSE, FALSE), upper_unbounded = c(FALSE, FALSE))
        sides[[field]] <- flags
        sides
      })
      expect_error(lv_test_search(map), class = "hetid_error_bad_argument")
    }
  }
  results <- lapply(c(TRUE, FALSE), function(named) {
    flags <- c(first = FALSE, second = TRUE)
    if (!named) flags <- unname(flags)
    lv_test_search(lv_test_linear(sides = function(scan, precheck) {
      list(lower_unbounded = flags, upper_unbounded = c(FALSE, FALSE))
    }))
  })
  expect_identical(results[[1L]]$schema, results[[2L]]$schema)
  expect_identical(results[[1L]]$schema$lower_status, c("bounded", "unbounded"))
  expect_identical(
    results[[1L]]$diagnostics$domain$lower_unbounded,
    c(first = FALSE, second = TRUE)
  )
  pending <- lv_test_search(lv_test_linear(sides = function(scan, precheck) {
    list(
      lower_unbounded = c(first = FALSE, second = TRUE),
      upper_unbounded = c(first = FALSE, second = FALSE), unresolved_endpoints = "second:min"
    )
  }))
  expect_identical(pending$schema$lower_status, c("bounded", "unreliable"))
})
