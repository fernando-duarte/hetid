lv_test_oracle <- function() {
  readRDS(test_path("fixtures", "log-variance-search-oracle.rds"))
}

lv_test_path_oracle <- function() {
  readRDS(test_path("fixtures", "log-variance-path-oracle.rds"))
}

lv_test_core <- function(value) {
  if (is.list(value)) {
    fields_to_remove <- intersect(names(value), c("sample_id", "domain", "traversal"))
    for (name in fields_to_remove) value[[name]] <- NULL
    for (i in seq_along(value)) value[i] <- list(lv_test_core(value[[i]]))
  }
  value
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
