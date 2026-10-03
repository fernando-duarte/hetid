lv_test_one_column <- function() {
  ids <- seq_len(24L)
  w1 <- rep(c(-2, -1, 1, 2), 6L)
  sample_data <- prepare_log_variance_search(
    w1, cbind(news = rep(c(-1, 1), 12L)), cbind(pc1 = ids), ids, ids,
    ols_residuals = w1
  )
  ppml <- make_log_variance_map(sample_data, "ppml", point = c(news = 0))
  harvey <- make_log_variance_map(sample_data, "harvey",
    point = c(news = 0), ppml = ppml,
    logols_coef = stats::lm.fit(sample_data$x_mat, log(w1^2))$coefficients
  )
  list(
    sample = sample_data, maps = list(ppml = ppml, harvey = harvey),
    quadratic = list(A_i = list(matrix(1)), b_i = list(0), c_i = -0.1^2),
    table = data.frame(coef = "news", status = "bounded", outer_lower = -0.1, outer_upper = 0.1)
  )
}

for (method in c("ppml", "harvey")) {
  for (seed_kind in c("absent", "named", "unnamed")) {
    test_that(paste("one-column", method, "search accepts", seed_kind, "seeds"), {
      input <- lv_test_one_column()
      seed <- switch(seed_kind,
        absent = NULL,
        named = c(news = 0),
        unnamed = 0
      )
      result <- search_log_variance_map(input$maps[[method]], input$quadratic, input$table,
        seed = seed
      )
      expect_true(all(result$schema$lower_status == "bounded"))
      expect_true(all(result$schema$upper_status == "bounded"))
      expect_true(all(is.finite(c(result$schema$lower, result$schema$upper))))
      expect_false(result$diagnostics$budget_exhausted)
    })
  }
  test_that(paste("one-column", method, "aggregate preserves the declared axes"), {
    input <- lv_test_one_column()
    key <- sprintf("%.17g", 0.1)
    result <- profile_log_variance_map(input$sample,
      stats::setNames(list(input$quadratic), key), stats::setNames(list(input$table), key),
      0.1, method,
      point = c(news = 0)
    )
    expect_identical(names(result$results), key)
    schema <- result$results[[1L]]$schema
    expect_identical(schema$coef, c("(Intercept)", "pc1"))
    expect_true(all(schema$lower_status == "bounded"))
    expect_true(all(schema$upper_status == "bounded"))
    expect_true(all(is.finite(c(schema$lower, schema$upper))))
  })
  test_that(paste("one-column", method, "seed naming preserves numerical order and RNG"), {
    input <- lv_test_one_column()
    with_rng_scope({
      set.seed(130L)
      before <- .Random.seed
      named <- search_log_variance_map(input$maps[[method]], input$quadratic, input$table,
        seed = c(news = 0)
      )
      unnamed <- search_log_variance_map(input$maps[[method]], input$quadratic, input$table,
        seed = 0
      )
      expect_identical(.Random.seed, before)
      expect_identical(named$schema, unnamed$schema)
      expect_identical(named$n_feasible, unnamed$n_feasible)
      expect_identical(as.list(named$budget), as.list(unnamed$budget))
      expect_identical(named$diagnostics$selector, unnamed$diagnostics$selector)
    })
  })
}

test_that("one-column custom Jacobians retain optional column names after validation", {
  input <- lv_test_one_column()
  map <- lv_test_linear()
  map$coef_labels <- c("level", "twice")
  map$fit_at_b <- function(b, start = NULL, phase = NULL) {
    lv_set_axis(b, "news", "b")
    lv_set_fit_result(c(level = b[[1L]], twice = 2 * b[[1L]]), "ok", TRUE)
  }
  for (columns in list(NULL, "news")) {
    map$jacobian_at_b <- function(b, fit = NULL) {
      matrix(c(1, 2), 2L, 1L, dimnames = list(map$coef_labels, columns))
    }
    for (seed in list(NULL, c(news = 0), 0)) {
      result <- search_log_variance_map(map, input$quadratic, input$table, seed = seed)
      expect_true(all(result$schema$lower_status == "bounded"))
      expect_true(all(result$schema$upper_status == "bounded"))
      expect_equal(result$schema$lower, c(-0.1, -0.2), tolerance = 1e-6)
      expect_equal(result$schema$upper, c(0.1, 0.2), tolerance = 1e-6)
    }
  }
  expect_error(search_log_variance_map(map, input$quadratic, input$table,
    seed = c(wrong = 0)
  ), "names of seed", class = "hetid_error_bad_argument")
  for (axes in list(list(rev(map$coef_labels), "news"), list(map$coef_labels, "wrong"))) {
    map$jacobian_at_b <- function(b, fit = NULL) matrix(c(1, 2), 2L, 1L, dimnames = axes)
    expect_error(search_log_variance_map(map, input$quadratic, input$table),
      "Jacobian",
      class = "hetid_error_bad_argument"
    )
  }
  map$coef_objective <- function(j) {
    force(j)
    list(fn = function(b) j * b[[1L]], gr = function(b) c(wrong = j))
  }
  expect_error(search_log_variance_map(map, input$quadratic, input$table),
    "names of objective gradient",
    class = "hetid_error_bad_argument"
  )
})
