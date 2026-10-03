lv_test_oracle <- function() {
  readRDS(test_path("fixtures", "log-variance-search-oracle.rds"))
}

lv_test_path_oracle <- function() {
  readRDS(test_path("fixtures", "log-variance-path-oracle.rds"))
}

# Effort tallies and the labels naming which pass found an endpoint record the
# route a search took, and floating-point noise changes that route across
# platforms, so oracle comparisons drop them; expect_effort_consistent() checks
# the tallies' arithmetic on the fresh run instead.
LV_EFFORT_TALLIES <- c("n_attempted", "n_evaluated", "n_cached", "counters", "cache_hits")
LV_ROUTE_LABELS <- c("origin", "lower_source", "upper_source")

lv_test_core <- function(value) {
  if (is.list(value)) {
    fields_to_remove <- intersect(names(value), c("sample_id", "domain", "traversal"))
    for (name in fields_to_remove) value[[name]] <- NULL
    for (i in seq_along(value)) value[i] <- list(lv_test_core(value[[i]]))
  }
  value
}

lv_drop_route <- function(value) {
  if (is.list(value)) {
    fields_to_remove <- intersect(names(value), LV_ROUTE_LABELS)
    if (any(c("n_attempted", "cache_hits") %in% names(value))) {
      fields_to_remove <- c(fields_to_remove, intersect(names(value), LV_EFFORT_TALLIES))
    }
    for (name in fields_to_remove) value[[name]] <- NULL
    for (i in seq_along(value)) value[i] <- list(lv_drop_route(value[[i]]))
  }
  value
}

expect_effort_consistent <- function(value) {
  if (!is.list(value)) {
    return(invisible())
  }
  if ("n_attempted" %in% names(value)) {
    counters <- value$counters
    expect_identical(
      names(counters), c("scan", "extra_start", "polish", "cold_start", "cache_hit")
    )
    expect_true(all(counters >= 0L))
    expect_identical(value$n_attempted, value$n_evaluated + value$n_cached)
    expect_identical(value$n_attempted, sum(counters))
    expect_identical(value$n_cached, counters[["cache_hit"]])
  }
  if ("cache_hits" %in% names(value)) {
    expect_true(value$cache_hits >= 0L && value$n_evaluated >= 0L)
  }
  for (element in value) expect_effort_consistent(element)
  invisible()
}


lv_test_sample <- function() {
  input <- lv_test_oracle()$inputs
  n <- length(input$sample$w1)
  prepare_log_variance_search(input$sample$w1, input$sample$w2, input$raw,
    seq_len(n), seq_len(n),
    ols_residuals = input$sample$w1
  )
}

lv_test_linear <- function(fail = FALSE, cold = FALSE, sides = NULL) {
  input <- lv_test_oracle()$inputs
  loading <- input$loading
  list(
    metadata = list(
      estimator = "linear", target_functional = "linear",
      sample_id = "oracle", smoothness = "smooth", spec_id = "linear-v1"
    ),
    coef_labels = rownames(loading), fit_at_b = function(b, start = NULL, phase = NULL) {
      if (fail) {
        return(lv_set_fit_result(NULL, "nonconvergence", FALSE))
      }
      value <- drop(loading %*% b)
      if (cold && identical(phase, "cold_start")) value <- value + 0.1
      lv_set_fit_result(value, "ok", TRUE)
    }, jacobian_at_b = function(b, fit = NULL) loading, sides = sides
  )
}

lv_test_search <- function(estimator = lv_test_linear(), budget = 1000L, ...) {
  input <- lv_test_oracle()$inputs
  search_log_variance_map(estimator, input$quadratic, input$table,
    seed = c(0, 0),
    max_grid_points = 30L, max_fit_evals = budget, tau = 0.1, control = input$control, ...
  )
}
