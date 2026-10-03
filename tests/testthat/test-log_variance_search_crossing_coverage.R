lv_test_covered_crossings <- function(x) {
  w1 <- c(-1.5, 1.5, 0, 2, -2, 4, -4, 8)
  w2 <- c(1, 1, 1, 0, 0, 0, 0, 0)
  prepared <- prepare_log_variance_search(
    c(w1, -8), cbind(news = c(w2, -3)), cbind(pc1 = x), 1:9, 1:8,
    impose_null = FALSE
  )
  expect_identical(prepared$w1, w1)
  expect_identical(prepared$w2, cbind(news = w2))
  expect_identical(prepared$prep$volatility_rows, 1:8)
  expect_identical(prepared$prep$x_centered, cbind(pc1 = x))
  map <- make_log_variance_map(prepared, "logols")
  quadratic <- list(A_i = list(matrix(1), matrix(-1)), b_i = list(0, 0), c_i = c(-4, 1))
  bounds_table <- data.frame(coef = "news", status = "bounded", outer_lower = -2, outer_upper = 2)
  control <- log_variance_search_control()
  control$search$grid_n <- 5L
  control$search$grid_floor <- 3L
  control$search$primary_starts_per_side <- 1L
  census <- map$precheck(quadratic, bounds_table)
  result <- search_log_variance_map(map, quadratic, bounds_table,
    max_grid_points = 5L, max_fit_evals = 100L, cold_start_check = FALSE, control = control
  )
  expect_identical(census$cross, 1:2)
  expect_identical(census$unresolved, 3L)
  expect_identical(census$zero_rows, integer(0))
  list(map = map, census = census, result = result)
}

test_that("independent infinities cover unresolved directions without resolving geometry", {
  found <- lv_test_covered_crossings(c(-1, 1, 0, -2, 2, -3, 3, 0))
  schema <- found$result$schema
  expect_identical(schema$coef, c("(Intercept)", "pc1"))
  expect_identical(schema$lower_status, rep("unbounded", 2L))
  expect_identical(schema$upper_status, c("bounded", "unbounded"))
  expect_identical(schema$lower, c(-Inf, -Inf))
  expect_identical(schema$upper[2L], Inf)
  expect_equal(schema$upper[1L], log(1792) / 4, tolerance = 1e-6)
  expect_true(log_variance_fit_ok(found$map$fit_at_b(schema$arg_upper[[1L]])))
  domain <- found$result$diagnostics$domain
  expect_identical(domain$unresolved, 3L)
  expect_identical(domain$crossing, 1:2)
  expect_identical(domain$unresolved_coverage, found$census$unresolved_coverage)
  expect_true(domain$unresolved_coverage$complete)
})

test_that("an exposed unresolved direction preserves whole-search closure", {
  found <- lv_test_covered_crossings(c(1, 1, -1, -1, 0, 0, 0, 0))
  schema <- found$result$schema
  expect_true(all(c(schema$lower_status, schema$upper_status) == "unreliable"))
  expect_true(all(is.na(c(schema$lower, schema$upper))))
  expect_identical(found$result$diagnostics$n_evaluated, 0L)
  expect_identical(found$result$diagnostics$precheck_failed, 3L)
  expect_identical(
    found$result$diagnostics$unresolved_coverage,
    found$census$unresolved_coverage
  )
  expect_false(found$census$unresolved_coverage$complete)
})

test_that("coverage requires every unresolved group and all active certified signs", {
  labels <- c("(Intercept)", "pc1")
  signs <- rbind(c(1, 1, 1, 1, 1), c(1, -1, 1, 1, -1))
  rownames(signs) <- labels
  check <- function(cross, order = 1:5, unknown = integer(0), complete) {
    value <- signs
    if (length(unknown)) value[2L, unknown] <- NA_real_
    groups <- list(rows = as.list(order), signs = value[, order], ambiguous = integer(0))
    census <- list(cross = cross, unresolved = order[order %in% 4:5], zero_rows = integer(0))
    evidence <- lv_log_crossing_coverage(groups, census, labels)
    expect_identical(evidence$complete, complete)
    expect_identical(evidence$unresolved, census$unresolved)
    expect_identical(evidence$group_rows, as.list(census$unresolved))
    expect_identical(evidence$groups, which(order %in% 4:5))
    expect_identical(unname(evidence$lower_unbounded), c(TRUE, TRUE))
    expect_identical(unname(evidence$upper_unbounded), c(FALSE, length(cross) > 1L))
  }
  check(1L, complete = FALSE)
  check(1L, c(1:3, 5L, 4L), complete = FALSE)
  check(1:2, complete = TRUE)
  check(1:2, c(1:3, 5L, 4L), complete = TRUE)
  check(1:2, unknown = 5L, complete = FALSE)
  check(1:2, c(1:3, 5L, 4L), unknown = 5L, complete = FALSE)
  check(1:3, unknown = 3L, complete = FALSE)
  groups <- list(rows = as.list(1:5), signs = signs, ambiguous = 5L)
  census <- list(cross = 1:2, unresolved = 4:5)
  expect_false(lv_log_crossing_coverage(groups, census, labels)$complete)
  groups$ambiguous <- integer(0)
  groups$signs[, 4:5] <- 0
  census$cross <- integer(0)
  expect_true(lv_log_crossing_coverage(groups, census, labels)$complete)
})

test_that("unresolved custom maps need explicit scalar coverage and zero rows close first", {
  for (coverage in list(
    NULL, TRUE, list(), list(complete = FALSE), list(complete = NA),
    list(complete = 1), list(complete = c(TRUE, TRUE)), list(complete = list(TRUE))
  )) {
    map <- lv_test_linear()
    map$precheck <- function(quadratic, theta_table) {
      list(unresolved = 1L, unresolved_coverage = coverage)
    }
    found <- lv_test_search(map)
    expect_identical(found$diagnostics$precheck_failed, 1L)
    expect_identical(found$diagnostics$n_evaluated, 0L)
    expect_true(all(is.na(c(found$schema$lower, found$schema$upper))))
    expect_true(all(c(found$schema$lower_status, found$schema$upper_status) == "unreliable"))
  }
  map$precheck <- function(quadratic, theta_table) {
    list(unresolved = 1L, zero_rows = 2L, unresolved_coverage = list(complete = TRUE))
  }
  found <- lv_test_search(map)
  expect_identical(found$diagnostics$closure_reason, "empty_log_domain")
  expect_identical(found$diagnostics$zero_rows, 2L)
  expect_identical(found$diagnostics$n_evaluated, 0L)
})
