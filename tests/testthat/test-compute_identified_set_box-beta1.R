# The structural (beta1) block of the identified-set box: the affine image
# beta1r - beta2r' theta of the same set, read off the same hull endpoints,
# so it inherits every property the theta block is tested for.

test_that("every finite beta1 bound is the image of a feasible witness", {
  fit <- box_fit()
  box <- compute_identified_set_box(fit, tau = 0.05, n_grid = 15L)
  checker <- make_system_checker(box$quadratic)
  expect_identical(box$beta1_bounds$coef, names(fit$beta1r))
  for (k in seq_len(nrow(box$beta1_bounds))) {
    lo <- box$beta1_arg_lower[k, ]
    hi <- box$beta1_arg_upper[k, ]
    expect_lte(max(checker(lo)), IDENTIFIED_SET_CONTROL$FEAS_TOL)
    expect_lte(max(checker(hi)), IDENTIFIED_SET_CONTROL$FEAS_TOL)
    # the witness must map to the bound it is stored for
    expect_equal(
      unname(recover_structural_coefficients(fit$beta1r, fit$beta2r, lo)[k]),
      box$beta1_bounds$lower[k],
      tolerance = 1e-10
    )
    expect_equal(
      unname(recover_structural_coefficients(fit$beta1r, fit$beta2r, hi)[k]),
      box$beta1_bounds$upper[k],
      tolerance = 1e-10
    )
  }
})

test_that("the theta witnesses map inside the beta1 bounds", {
  # both blocks are read off the same hull endpoints, so the image of any
  # theta witness is a value the structural sweep has already seen
  fit <- box_fit()
  box <- compute_identified_set_box(fit, tau = 0.05, n_grid = 15L)
  images <- recover_structural_coefficients(
    fit$beta1r, fit$beta2r, t(rbind(box$arg_lower, box$arg_upper))
  )
  expect_true(all(images >= box$beta1_bounds$lower - 1e-10))
  expect_true(all(images <= box$beta1_bounds$upper + 1e-10))
})

test_that("the beta1 bounds contain the tau = 0 structural point", {
  fit <- box_fit()
  for (tau in c(0.02, 0.05, 0.1)) {
    box <- compute_identified_set_box(fit, tau = tau, n_grid = 15L)
    expect_true(all(fit$beta1 >= box$beta1_bounds$lower - 1e-10))
    expect_true(all(fit$beta1 <= box$beta1_bounds$upper + 1e-10))
  }
})

test_that("beta1 intervals nest as the slack grows", {
  fit <- box_fit()
  tight <- compute_identified_set_box(fit, tau = 0.02, n_grid = 15L)
  loose <- compute_identified_set_box(fit, tau = 0.1, n_grid = 15L)
  expect_true(all(loose$beta1_bounds$lower <= tight$beta1_bounds$lower + 1e-9))
  expect_true(all(loose$beta1_bounds$upper >= tight$beta1_bounds$upper - 1e-9))
})

test_that("refining a nested grid never shrinks the beta1 intervals", {
  fit <- box_fit()
  coarse <- compute_identified_set_box(fit, tau = 0.05, n_grid = 21L)
  medium <- compute_identified_set_box(fit, tau = 0.05, n_grid = 41L)
  fine <- compute_identified_set_box(fit, tau = 0.05, n_grid = 81L)
  expect_true(all(medium$beta1_bounds$lower <= coarse$beta1_bounds$lower + 1e-9))
  expect_true(all(medium$beta1_bounds$upper >= coarse$beta1_bounds$upper - 1e-9))
  expect_true(all(fine$beta1_bounds$lower <= medium$beta1_bounds$lower + 1e-9))
  expect_true(all(fine$beta1_bounds$upper >= medium$beta1_bounds$upper - 1e-9))
})

test_that("the theta block is never narrower than a coordinates-only sweep", {
  # the coordinates grow the window first, along their own path, and the
  # structural objectives may only extend it afterwards, so the public
  # theta block contains what the coordinates alone would find
  fit <- box_fit()
  box <- compute_identified_set_box(fit, tau = 0.05, n_grid = 15L)
  built <- build_quadratic_system(fit$gamma, rep(0.05, 3), fit$moments)
  basis <- identified_set_basis(built$components, fit$point$theta, built$quadratic)
  alone <- apply_recession_bounds(
    identified_set_search(fit$point$theta, basis, built$quadratic, 15L, diag(3)),
    built$quadratic, diag(3)
  )
  expect_true(all(box$bounds$lower <= alone$lower + 1e-12))
  expect_true(all(box$bounds$upper >= alone$upper - 1e-12))
})

test_that("imposing the null makes every beta1 row a point", {
  # beta2r is exactly zero, so the structural map is constant and the sweep
  # sees the value 0 at every endpoint: lower and upper are beta1r itself
  d <- simulate_box_dgp()
  fit <- compute_tau0_system(d$y1, d$y2, d$x, d$z, impose_null = TRUE)
  box <- compute_identified_set_box(fit, tau = 0.05, n_grid = 11L)
  expect_identical(box$beta1_bounds$lower, unname(fit$beta1r))
  expect_identical(box$beta1_bounds$upper, unname(fit$beta1r))
  expect_false(anyNA(box$beta1_arg_lower))
  expect_true(all(attr(box, "null_loading")))
})

test_that("a numerically zero loading leaves its coefficient a point", {
  # centring both blocks makes the intercept loading a zero carrying rounding
  # noise; snapping it keeps the intercept a point even when the set is not
  d <- simulate_box_dgp(collinear = TRUE)
  x <- sweep(d$x, 2, colMeans(d$x))
  y2 <- sweep(d$y2, 2, colMeans(d$y2))
  fit <- compute_tau0_system(d$y1, y2, x, d$z)
  bounded <- compute_identified_set_box(fit, tau = 0.05, n_grid = 11L)
  expect_identical(bounded$beta1_bounds$lower[1L], unname(fit$beta1r[1L]))
  expect_identical(bounded$beta1_bounds$upper[1L], unname(fit$beta1r[1L]))
  expect_identical(
    attr(bounded, "null_loading"),
    c("(Intercept)" = TRUE, x1 = FALSE, x2 = FALSE)
  )
  unbounded <- compute_identified_set_box(fit, tau = 0.3, n_grid = 11L)
  expect_identical(unbounded$beta1_bounds$lower[1L], unname(fit$beta1r[1L]))
  expect_identical(unbounded$beta1_bounds$upper[1L], unname(fit$beta1r[1L]))
  expect_true(all(!is.finite(unbounded$beta1_bounds$lower[-1L])))
})

test_that("rescaling a regressor or a news column leaves the snap alone", {
  # a column of x a thousand times larger carries a loading a thousand times
  # smaller, a genuine loading whose interval rescales; a column of y2 a
  # thousand times larger scales one row of beta2r, which the row-relative
  # tolerance ignores
  d <- simulate_box_dgp()
  base <- compute_identified_set_box(box_fit(), tau = 0.05, n_grid = 11L)
  x <- d$x
  x[, "x1"] <- 1000 * x[, "x1"]
  scaled_x <- compute_identified_set_box(
    compute_tau0_system(d$y1, d$y2, x, d$z),
    tau = 0.05, n_grid = 11L
  )
  expect_identical(attr(scaled_x, "null_loading"), attr(base, "null_loading"))
  expect_false(any(attr(scaled_x, "null_loading")))
  expect_equal(
    1000 * scaled_x$beta1_bounds$lower[2L], base$beta1_bounds$lower[2L],
    tolerance = 1e-9
  )
  expect_equal(
    1000 * scaled_x$beta1_bounds$upper[2L], base$beta1_bounds$upper[2L],
    tolerance = 1e-9
  )
  y2 <- d$y2
  y2[, "news2"] <- 1000 * y2[, "news2"]
  scaled_y2 <- compute_identified_set_box(
    compute_tau0_system(d$y1, y2, d$x, d$z),
    tau = 0.05, n_grid = 11L
  )
  expect_identical(attr(scaled_y2, "null_loading"), attr(base, "null_loading"))
  expect_equal(scaled_y2$beta1_bounds$lower, base$beta1_bounds$lower, tolerance = 1e-6)
  expect_equal(scaled_y2$beta1_bounds$upper, base$beta1_bounds$upper, tolerance = 1e-6)
})

test_that("the snap tolerance is a documented ceiling with an off switch", {
  # a regressor scaled up by 1e9 carries a genuine loading below the default
  # tolerance; the default snaps it, tolerance zero keeps it live
  d <- simulate_box_dgp(collinear = TRUE)
  x <- d$x
  x[, "x1"] <- 1e9 * x[, "x1"]
  fit <- compute_tau0_system(d$y1, d$y2, x, d$z)
  snapped <- compute_identified_set_box(fit, tau = 0.3, n_grid = 11L)
  expect_true(attr(snapped, "null_loading")[["x1"]])
  expect_identical(snapped$beta1_bounds$lower[2L], unname(fit$beta1r[2L]))
  live <- compute_identified_set_box(
    fit,
    tau = 0.3, n_grid = 11L, null_loading_rtol = 0
  )
  expect_false(attr(live, "null_loading")[["x1"]])
  expect_true(all(!is.finite(live$beta1_bounds$lower[-1L])))
  expect_error(
    compute_identified_set_box(fit, tau = 0.3, null_loading_rtol = -1),
    "\\[0, 1\\)",
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_identified_set_box(fit, tau = 0.3, null_loading_rtol = 1),
    "\\[0, 1\\)",
    class = "hetid_error_bad_argument"
  )
})

test_that("an unbounded set makes every beta1 row with a live loading infinite", {
  # a witness makes every non-zero functional unbounded, and an infinite
  # side carries no witness
  fit <- box_fit(collinear = TRUE)
  box <- compute_identified_set_box(fit, tau = 0.3, n_grid = 11L)
  live <- !attr(box, "null_loading")
  expect_true(all(!is.finite(box$beta1_bounds$lower[live])))
  expect_true(all(!is.finite(box$beta1_bounds$upper[live])))
  expect_true(all(is.na(box$beta1_arg_lower[live, ])))
})
