test_that("prepared arrays require exact keys, finite matrices and distinct axes", {
  prepared <- variance_share_fixture()
  expect_silent(validate_variance_share_inputs(prepared, VARIANCE_SHARE_CONTROL))
  variants <- list(
    within(prepared, rm(y)),
    within(prepared, y <- matrix(y)),
    within(prepared, y[1] <- Inf),
    within(prepared, x <- as.data.frame(x)),
    within(prepared, y2[1, 1] <- NA_real_),
    within(prepared, z <- z + 1i),
    within(prepared, colnames(x) <- NULL),
    within(prepared, colnames(y2)[1] <- colnames(x)[1]),
    within(prepared, colnames(x)[1] <- "(Intercept)"),
    within(prepared, colnames(x)[1] <- ""),
    within(prepared, colnames(y2)[1] <- NA_character_)
  )
  for (bad in variants) {
    expect_error(compute_variance_shares(bad), class = "hetid_error_bad_argument")
  }
  partial <- prepared
  names(partial)[1] <- "yyyy"
  expect_error(compute_variance_shares(partial), class = "hetid_error_bad_argument")
  expect_error(compute_variance_shares(within(prepared, z <- cbind(z, z))),
    class = "hetid_error_dimension_mismatch"
  )
  expect_error(compute_variance_shares(within(prepared, x <- x[-1, ])),
    class = "hetid_error_dimension_mismatch"
  )
  sanitized <- prepared
  colnames(sanitized$x)[1] <- "expected score"
  expect_error(compute_variance_shares(sanitized), "axes", class = "hetid_error")
})

test_that("controls preserve defaults and refuse invalid or excessive grids before searching", {
  expect_identical(
    VARIANCE_SHARE_CONTROL[names(QUADRATIC_PROFILE_CONTROL)],
    QUADRATIC_PROFILE_CONTROL
  )
  expect_identical(VARIANCE_SHARE_CONTROL$taus, c(.05, .10, .20))
  expect_identical(VARIANCE_SHARE_CONTROL$grid_points_per_axis, 101L)
  expect_identical(VARIANCE_SHARE_CONTROL$grid_points_limit, 2e6)
  bad_values <- list(
    taus = c(.1, .1), grid_points_per_axis = 1.5,
    grid_points_limit = 0, coherence_ratio = 1.1, coherence_slack = -1,
    orthogonality_tolerance = -1, point_tolerance = 0, solver_maxeval = 0
  )
  for (key in names(bad_values)) {
    bad <- VARIANCE_SHARE_CONTROL
    bad[[key]] <- bad_values[[key]]
    expect_error(validate_variance_share_control(bad), class = "hetid_error_bad_argument")
  }
  expect_error(validate_variance_share_control(VARIANCE_SHARE_CONTROL[-1]),
    class = "hetid_error_bad_argument"
  )
  expect_error(validate_variance_share_control(c(VARIANCE_SHARE_CONTROL, list(taus = .1))),
    class = "hetid_error_bad_argument"
  )
  prepared <- variance_share_fixture()
  prepared$y2 <- cbind(prepared$y2, extra = seq_along(prepared$y))
  expect_error(compute_variance_shares(prepared), "too many points",
    class = "hetid_error_bad_argument"
  )
  control <- VARIANCE_SHARE_CONTROL
  control$grid_points_per_axis <- 3L
  control$grid_points_limit <- 81
  expect_silent(validate_variance_share_inputs(prepared, control))
  control$grid_points_limit <- 80
  expect_error(validate_variance_share_inputs(prepared, control), "too many points")
})

test_that("both signs of within-block correlation and degenerate variances are rejected", {
  prepared <- variance_share_fixture()
  for (key in c("x", "y2")) {
    for (sign in c(-1, 1)) {
      bad <- prepared
      bad[[key]][, 2] <- bad[[key]][, 2] + sign * bad[[key]][, 1]
      expect_error(variance_share_covariances(bad$y, bad$x, bad$y2, VARIANCE_SHARE_CONTROL),
        "uncorrelated",
        class = "hetid_error_bad_argument"
      )
    }
    bad <- prepared
    bad[[key]][, 1] <- 1
    expect_error(variance_share_covariances(bad$y, bad$x, bad$y2, VARIANCE_SHARE_CONTROL),
      "positive",
      class = "hetid_error_bad_argument"
    )
  }
  expect_error(variance_share_covariances(
    rep(1, 220), prepared$x, prepared$y2,
    VARIANCE_SHARE_CONTROL
  ), "Outcome variance", class = "hetid_error_bad_argument")
  expect_error(variance_share_ols(prepared$y, prepared$x, prepared$x),
    "full-rank",
    class = "hetid_error_bad_argument"
  )
  local_mocked_bindings(compute_tau0_system = function(...) {
    list(
      beta1r = setNames(rep(0, 4), c("(Intercept)", colnames(prepared$x))),
      beta2r = matrix(0, 3, 4, dimnames = list(
        colnames(prepared$y2),
        c("(Intercept)", colnames(prepared$x))
      )), point = NULL
    )
  })
  expect_error(variance_share_fit(
    prepared$y, prepared$x, prepared$y2, prepared$z,
    VARIANCE_SHARE_CONTROL
  ), "no unique", class = "hetid_error")
})
