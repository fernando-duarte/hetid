test_that("residuals follow the candidate map without recentering", {
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  direct <- stats::lm.fit(fx$z, fx$y - drop(fx$news %*% fx$b))$residuals
  expect_equal(unname(drop(fx$w1 - fx$w2 %*% fx$b)), unname(direct),
    tolerance = 1e-10
  )
  expect_identical(prep$w1, fx$w1[prep$volatility_rows])
  expect_gt(abs(mean(prep$w1)), 1e-6)
})

test_that("rows are joined by identifier, never by position", {
  fx <- lp_fixture()
  base <- evaluate_log_projection(lp_prep(fx), fx$b, "log", jacobian = FALSE)
  perm_v <- rev(seq_along(fx$vol_ids))
  perm_m <- sample(seq_along(fx$mean_ids))
  prep <- prepare_log_projection(
    fx$w1[perm_m], fx$w2[perm_m, , drop = FALSE], fx$x_var[perm_v, ],
    fx$mean_ids[perm_m], fx$vol_ids[perm_v]
  )
  moved <- evaluate_log_projection(prep, fx$b, "log", jacobian = FALSE)
  expect_equal(moved$coef, base$coef, tolerance = 1e-12)
})

test_that("Date, integer, and classed numeric identifiers are accepted", {
  fx <- lp_fixture()
  dates <- as.Date("1990-01-01") + 100 * seq_along(fx$mean_ids)
  prep <- prepare_log_projection(
    fx$w1, fx$w2, fx$x_var, dates, utils::tail(dates, length(fx$vol_ids))
  )
  expect_s3_class(prep, "hetid_log_projection_prep")
  # a classed double, the shape of tsibble::yearquarter
  as_qtr <- function(x) structure(as.numeric(x), class = "quarter_id")
  n_mean <- length(fx$mean_ids)
  prep_q <- prepare_log_projection(
    fx$w1, fx$w2, fx$x_var, as_qtr(seq_len(n_mean)),
    as_qtr(utils::tail(seq_len(n_mean), length(fx$vol_ids)))
  )
  expect_identical(prep_q$volatility_rows, prep$volatility_rows)
})

test_that("identifier defects raise bad-argument errors", {
  fx <- lp_fixture()
  prep_ids <- function(mean_ids, vol_ids) {
    prepare_log_projection(fx$w1, fx$w2, fx$x_var, mean_ids, vol_ids)
  }
  vol_num <- as.numeric(fx$vol_ids)
  expect_error(prep_ids(fx$mean_ids, vol_num), class = "hetid_error_bad_argument")
  dup <- fx$vol_ids
  dup[2] <- dup[1]
  expect_error(prep_ids(fx$mean_ids, dup), class = "hetid_error_bad_argument")
  with_na <- fx$vol_ids
  with_na[3] <- NA
  expect_error(prep_ids(fx$mean_ids, with_na), class = "hetid_error_bad_argument")
  absent <- fx$vol_ids
  absent[1] <- 1L
  expect_error(prep_ids(fx$mean_ids, absent), class = "hetid_error_bad_argument")
})

test_that("centering changes only the intercept convention", {
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  expect_equal(unname(prep$x_center), unname(colMeans(fx$x_var)), tolerance = 1e-12)
  shifted <- fx
  shifted$x_var <- fx$x_var + 5
  a <- evaluate_log_projection(prep, fx$b, "log", jacobian = FALSE)$coef
  b <- evaluate_log_projection(lp_prep(shifted), fx$b, "log", jacobian = FALSE)$coef
  expect_equal(a, b, tolerance = 1e-12)
  g <- 2 * log(abs(prep$w1 - drop(prep$w2 %*% fx$b)))
  raw <- stats::lm.fit(cbind(1, fx$x_var), g)$coefficients
  expect_equal(unname(a[1] - sum(prep$x_center * a[-1])), unname(raw[1]),
    tolerance = 1e-10
  )
})

test_that("the intercept-only design returns the mean log square", {
  fx <- lp_fixture(d_r = 0L)
  prep <- lp_prep(fx)
  e <- prep$w1 - drop(prep$w2 %*% fx$b)
  fit <- evaluate_log_projection(prep, fx$b, "log")
  expect_equal(unname(fit$coef), mean(2 * log(abs(e))), tolerance = 1e-12)
})

test_that("the unrestricted Fuller lower bound matches the full regression", {
  fx <- lp_fixture()
  prep <- lp_prep(fx)
  full <- stats::lm.fit(cbind(fx$z, fx$news), fx$y)$residuals
  expect_equal(exp(prep$log_scale_lower), mean(full^2), tolerance = 1e-10)
  expect_true(prep$scale_lower_certified)
  exact <- fx
  exact$w1 <- 2 * fx$w2[, 1]
  expect_false(lp_prep(exact)$scale_lower_certified)
})

test_that("a rank-truncated news QR never certifies the Fuller scale", {
  u <- rep(c(-1, 1), 8L)
  v <- rep(c(-1, -1, 1, 1), 4L)
  w2 <- cbind(a = u, b = u + 2^-40 * v)
  prep <- prepare_log_projection(v, w2, matrix(numeric(0), 16L, 0L), 1:16, 1:16)
  expect_true(all(v - w2 %*% c(-2^40, 2^40) == 0))
  expect_false(prep$scale_lower_certified)
})

test_that("the mean-zero guard is scale-free", {
  fx <- lp_fixture()
  for (a in c(1e-200, 1e200)) {
    scaled <- fx
    scaled$w1 <- fx$w1 * a
    expect_s3_class(lp_prep(scaled), "hetid_log_projection_prep")
  }
  spike <- fx
  spike$w1 <- c(1, rep(0, length(fx$w1) - 1L)) * 1e200
  expect_error(lp_prep(spike), class = "hetid_error_bad_argument")
  shifted <- fx
  shifted$w2[, 1] <- shifted$w2[, 1] + 1
  expect_error(lp_prep(shifted), class = "hetid_error_bad_argument")
})

test_that("input guards raise their documented conditions", {
  fx <- lp_fixture()
  bad <- fx
  bad$w1[2] <- Inf
  expect_error(lp_prep(bad), class = "hetid_error_bad_argument")
  dup <- fx
  dup$x_var <- cbind(fx$x_var, fx$x_var[, 1])
  colnames(dup$x_var) <- c("pc1", "pc2", "pc3")
  expect_error(lp_prep(dup), class = "hetid_error_bad_argument")
  short <- fx
  short$vol_ids <- utils::tail(fx$mean_ids, 3L)
  short$x_var <- fx$x_var[1:3, ]
  expect_error(lp_prep(short), class = "hetid_error_insufficient_data")
})

test_that("the operator matches the normal-equations solve", {
  set.seed(5)
  x <- cbind(a = stats::rnorm(40), b = stats::rnorm(40))
  for (xx in list(x, x[, c(2L, 1L), drop = FALSE])) {
    design <- cbind("(Intercept)" = 1, xx)
    expected <- solve(crossprod(design), t(design))
    got <- log_projection_matrix(xx)
    expect_equal(unname(got), unname(expected), tolerance = 1e-12)
    expect_identical(rownames(got), colnames(design))
    expect_identical(names(attributes(got)), c("dim", "dimnames"))
  }
  u <- rep(c(-1, 1), 8L)
  v <- rep(c(-1, -1, 1, 1), 4L)
  expect_error(
    log_projection_matrix(cbind(u, u + 2^-40 * v, v)),
    class = "hetid_error_bad_argument"
  )
})

test_that("the operator never allocates an observation-square matrix", {
  skip_if(!capabilities("profmem"), "memory profiling unavailable")
  n_obs <- 20000L
  x <- cbind(a = stats::rnorm(n_obs), b = stats::rnorm(n_obs))
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  utils::Rprofmem(path, threshold = 1e5)
  log_projection_matrix(x)
  utils::Rprofmem(NULL)
  records <- grep("^[0-9]+ ?:", readLines(path), value = TRUE)
  sizes <- as.numeric(sub(" ?:.*", "", records))
  expect_lt(max(c(0, sizes)), 10 * n_obs * 3 * 8)
})

test_that("the prep records conditioning and prints", {
  prep <- lp_prep(lp_fixture())
  expect_true(is.finite(prep$projection_rcond) && prep$projection_rcond > 0)
  expect_output(out <- print(prep), "hetid_log_projection_prep")
  expect_identical(out, prep)
})
