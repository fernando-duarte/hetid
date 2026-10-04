test_that("coincident residual zeros cancel the slope without extending the log domain", {
  w1 <- rep(c(0, 2, -2), each = 2)
  w2 <- rep(c(1, -.5, -.5), each = 2)
  x <- cbind(pc1 = rep(c(-1, 1), 3))
  sample <- prepare_log_variance_search(w1, cbind(news = w2), x, 1:6, 1:6)
  map <- make_log_variance_map(sample, "logols")
  table <- data.frame(coef = "news", status = "bounded", outer_lower = -.1, outer_upper = .1)
  control <- log_variance_search_control()
  control$search$GRID_N <- 5L
  control$search$GRID_FLOOR <- 3L
  control$search$PRIMARY_STARTS_PER_SIDE <- 1L
  for (constant in c(-.01, -.1^2)) {
    for (cold in c(FALSE, TRUE)) {
      for (seed in list(NULL, 0)) {
        quadratic <- list(A_i = list(matrix(1)), b_i = list(0), c_i = constant)
        result <- search_log_variance_map(map, quadratic, table,
          seed = seed,
          max_grid_points = 5L, max_fit_evals = 100L, cold_start_check = cold, control = control
        )
        found <- list(map = map, result = result)
        schema <- found$result$schema
        expect_identical(schema$lower_status, c("unbounded", "bounded"))
        expect_identical(schema$upper_status[[2L]], "bounded")
        expect_identical(schema$lower[[1L]], -Inf)
        expect_true(is.finite(schema$upper[[1L]]))
        expect_equal(c(schema$lower[[2L]], schema$upper[[2L]]), c(0, 0), tolerance = 1e-12)
        for (arg in c(schema$arg_lower[2L], schema$arg_upper)) {
          expect_true(log_variance_fit_ok(found$map$fit_at_b(arg)))
        }
      }
    }
  }
  objective <- found$map$coef_objective(2L)
  expect_false(objective$admit(0))
  expect_true(is.nan(objective$fn(0)))
  expect_true(all(is.nan(objective$gr(0))))
  for (b in c(-.1, -1e-12, 1e-12, .1)) {
    expect_equal(objective$fn(b), 0, tolerance = 1e-12)
    expect_equal(unname(objective$gr(b)), 0, tolerance = 1e-12)
    expect_equal(found$map$coef_objective(1L)$fn(b),
      (2 / 3) * log(abs(b) * (4 - b^2 / 4)),
      tolerance = 1e-10
    )
  }
  prep <- sample$prep
  mesh <- matrix(c(-.1, -.05, 0, .05, .1), ncol = 1L)
  forward <- found$map$scan_grid(mesh)
  # the cancelled slope ties the extremes at both mesh ends, so which tied point
  # is reported depends on rounding; compare everything but the arguments
  values <- setdiff(names(forward), c("arg_min", "arg_max"))
  for (chunk in c(1L, 2L)) {
    scan <- lv_set_grid_scan(mesh, sample$w1, sample$w2, prep$projection, chunk, prep)
    expect_oracle_equal(scan[values], forward[values], ORACLE_TOLERANCE[["direct"]])
  }
  backward <- found$map$scan_grid(mesh[5:1, , drop = FALSE])
  expect_equal(
    forward[c("min", "max", "cross_grid")],
    backward[c("min", "max", "cross_grid")]
  )
})

test_that("full correlated-design group weights retain both singular directions", {
  x <- cbind(x1 = c(-1, -1, 1, 1), x2 = c(-2, 0, 0, 2))
  prep <- prepare_log_projection(
    c(0, 2, 0, -2),
    cbind(news = c(1, -1, 1, -1)), x, 1:4, 1:4
  )
  groups <- lv_set_logols_groups(prep)
  group <- which(vapply(groups$rows, function(rows) identical(rows, c(1L, 3L)), logical(1)))
  expect_length(group, 1L)
  expect_equal(unname(groups$weights[, group]), c(.5, .5, -.5), tolerance = 1e-12)
  expect_identical(unname(groups$signs[, group]), c(1, 1, -1))
})

test_that("supported affine multiples preserve offsets and finite-domain derivatives", {
  w1 <- c(1, 2, -1, -2, .5, -.5, 3, -3)
  w2 <- cbind(news = c(1, 2, -1, -2, .5, -.5, 0, 0))
  prep <- prepare_log_projection(w1, w2, cbind(pc1 = 1:8), 1:8, 1:8)
  sample <- prepare_log_variance_search(w1, w2, cbind(pc1 = 1:8), 1:8, 1:8)
  map <- make_log_variance_map(sample, "logols")
  groups <- lv_set_logols_groups(prep)
  expect_true(any(vapply(groups$rows, identical, logical(1), 1:6)))
  for (b in c(-.1, .1, 1 - 1e-8)) {
    e <- w1 - drop(w2) * b
    expected <- stats::lm.fit(sample$x_mat, 2 * log(abs(e)))$coef
    derivative <- stats::lm.fit(sample$x_mat, -2 * drop(w2) / e)$coef
    for (j in seq_along(expected)) {
      objective <- map$coef_objective(j)
      expect_equal(objective$fn(b), unname(expected[j]), tolerance = 1e-10)
      expect_equal(unname(objective$gr(b)), unname(derivative[j]), tolerance = 1e-5)
    }
  }
  expect_true(is.na(lv_log_relation(c(1, 3), c(3, 9))))
  expect_identical(lv_log_relation(c(1, 3), c(-2, -6)), -2)
  expect_identical(lv_log_relation(c(1, 3), c(2, 6)), 2)
  expect_identical(lv_log_relation(c(1, 3), c(.5, 1.5)), .5)
  expect_true(is.na(lv_log_relation(2^-1022, 2^1023)))
  expect_false(is.na(lv_log_relation(c(1, 3), c(1 + 1e-8, 3))))
})

test_that("arithmetic certificates enclose signs without snapping uncertain zeros", {
  expect_equal(lv_log_exact(matrix(c(1, 1, -1, 1), 2L, byrow = TRUE)), 0)
  expect_true(is.na(lv_log_exact(matrix(c(.1, .1), 1L))))
  positive <- lv_log_op(c(1, 1), c(1e-10, 1e-10), "+")
  expect_lte(positive[1L], 1 + 1e-10)
  expect_gte(positive[2L], 1 + 1e-10)
  expect_true(all(is.infinite(lv_log_op(c(1, 1), c(-1, 1), "/"))))
  expect_true(all(is.infinite(lv_log_op(rep(.Machine$double.xmax, 2L), c(2, 2), "*"))))
  design <- cbind(1, c(-1, 1, -1, 1) + c(0, 0, 0, 1e-8))
  prep <- prepare_log_projection(
    c(0, 0, 2, -2),
    cbind(news = c(1, 1, -1, -1)), design[, 2L, drop = FALSE], 1:4, 1:4
  )
  groups <- lv_set_logols_groups(prep)
  expect_true(all(is.finite(groups$weights)))
  expect_false(anyNA(groups$signs))
})

test_that("actual small group weights distinguish certified and unresolved signs", {
  for (delta in c(2^-26, 2^-50)) {
    sample <- prepare_log_variance_search(
      c(0, 0, 2, -2),
      cbind(news = c(1, 1, -1, -1)), cbind(pc1 = c(-1, 1, -1, 1 + delta)), 1:4, 1:4
    )
    prep <- sample$prep
    design <- cbind(1, prep$x_centered)
    groups <- lv_set_logols_groups(prep)
    group <- which(vapply(groups$rows, identical, logical(1), 1:2))
    expect_length(group, 1L)
    verified <- lv_log_inverse_bound(design, prep$projection)
    expect_false(is.null(verified))
    coefs <- groups$weights[, group]
    rhs <- lapply(seq_len(ncol(design)), function(j) lv_log_dot(design[1:2, j], c(1, 1)))
    product <- lv_log_matvec(verified$gram, lapply(coefs, rep, times = 2L))
    residual <- Map(function(a, b) lv_log_op(a, b, "-"), rhs, product)
    error <- max(vapply(residual, function(z) max(abs(z)), numeric(1)))
    bound <- lv_log_op(rep(verified$multiplier, 2L), rep(error, 2L), "*")[2L]
    interval <- lv_log_op(rep(coefs[2L], 2L), c(-bound, bound), "+")
    target <- -delta / (2 * (4 + 2 * delta + 3 * delta^2 / 4))
    expect_lt(target, 0)
    expect_lte(interval[1L], target)
    expect_gte(interval[2L], target)
    expect_lte(abs(coefs[2L] - target), bound)
    map <- make_log_variance_map(sample, "logols")
    quadratic <- list(A_i = list(matrix(1)), b_i = list(0), c_i = -.01)
    table <- data.frame(coef = "news", status = "bounded", outer_lower = -.1, outer_upper = .1)
    control <- log_variance_search_control()
    control$search$GRID_N <- 5L
    control$search$GRID_FLOOR <- 3L
    control$search$PRIMARY_STARTS_PER_SIDE <- 1L
    result <- search_log_variance_map(map, quadratic, table,
      max_grid_points = 5L,
      max_fit_evals = 100L, cold_start_check = FALSE, control = control
    )
    sides <- result$diagnostics$domain
    expect_identical(unname(sides$lower_unbounded), c(TRUE, FALSE))
    if (delta == 2^-26) {
      expect_gt(abs(target), 1e-10)
      expect_lt(abs(target), 1e-8)
      expect_lt(interval[2L], 0)
      expect_identical(unname(groups$signs[2L, group]), -1)
      expect_identical(result$schema$upper_status[2L], "unbounded")
      expect_identical(result$schema$upper[2L], Inf)
    } else {
      expect_lt(abs(target), .Machine$double.eps)
      expect_lte(interval[1L], 0)
      expect_gte(interval[2L], 0)
      expect_true(is.na(groups$signs[2L, group]))
      expect_identical(unname(sides$upper_unbounded), c(FALSE, FALSE))
      expect_setequal(sides$unresolved_endpoints, c("pc1:min", "pc1:max"))
      expect_identical(
        c(result$schema$lower_status[2L], result$schema$upper_status[2L]),
        rep("unreliable", 2L)
      )
    }
  }
})

test_that("the original near-zero derivative discrepancy is enclosed", {
  sample <- prepare_log_variance_search(
    rep(c(0, 2, -2), each = 2),
    cbind(news = rep(c(1, -.5, -.5), each = 2)),
    cbind(pc1 = rep(c(-1, 1), 3)), 1:6, 1:6
  )
  prep <- sample$prep
  design <- cbind(1, prep$x_centered)
  verified <- lv_log_inverse_bound(design, prep$projection)
  for (b in c(-1e-12, 1e-12)) {
    old <- evaluate_log_projection(prep, b, "log")$jacobian[, 1L]
    response <- -2 * drop(sample$w2) / (sample$w1 - drop(sample$w2) * b)
    rhs <- lapply(seq_len(ncol(design)), function(j) lv_log_dot(design[, j], response))
    fitted <- lv_log_matvec(verified$gram, lapply(old, rep, times = 2L))
    residual <- Map(function(a, z) lv_log_op(a, z, "-"), rhs, fitted)
    norm <- max(vapply(residual, function(z) max(abs(z)), numeric(1)))
    bound <- lv_log_op(rep(verified$multiplier, 2L), rep(norm, 2L), "*")[2L]
    expect_lte(abs(old[2L]), bound)
  }
})
