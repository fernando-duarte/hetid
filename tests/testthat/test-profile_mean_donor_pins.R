test_that("coefficient paths preserve independent donor pins", {
  pins <- utils::read.csv(test_path("fixtures", "mean-profile-pins.csv"),
    colClasses = "character", na.strings = character()
  )
  scenarios <- list(
    synthetic_seed3 = c(0.05, 0.2, 0.5),
    synthetic_seed26 = c(0.05, 0.2, 0.5, 0.65), synthetic_seed34 = c(0.05, 0.2)
  )
  columns <- list(
    theta_lower = c("theta", "set_lower", "lower_status"),
    theta_upper = c("theta", "set_upper", "upper_status"),
    outer_lower = c("theta", "outer_lower"), outer_upper = c("theta", "outer_upper"),
    beta1_lower = c("beta1", "set_lower", "lower_status"),
    beta1_upper = c("beta1", "set_upper", "upper_status")
  )
  for (scenario in names(scenarios)) {
    fit <- mean_profile_fixture(as.integer(sub("synthetic_seed", "", scenario)))
    path <- profile_mean_tau_path(fit, scenarios[[scenario]])
    want <- pins[pins$scenario == scenario, ]
    for (key in names(path)) {
      tau <- as.numeric(key)
      at_tau <- want[as.numeric(want$tau) == tau, ]
      for (field in names(columns)) {
        col <- columns[[field]]
        rows <- at_tau[at_tau$field == field, ]
        table <- path[[key]][[col[1]]]
        values <- unname(table[[col[2]]])
        expected <- suppressWarnings(as.numeric(rows$bits))
        expect_identical(as.integer(rows$row), seq_along(values))
        expect_identical(is.na(values), is.na(expected))
        expect_oracle_equal(values, expected, ORACLE_TOLERANCE[["solver"]],
          info = paste(scenario, key, field)
        )
        if (length(col) == 3L) expect_identical(table[[col[3]]], rows$status)
      }
    }
    reverse <- profile_mean_tau_path(fit, rev(scenarios[[scenario]]))
    expect_identical(unname(reverse), unname(rev(path)))
  }
})

test_that("structural affine widening preserves constants and direct sum arithmetic", {
  beta1r <- c(intercept = 2, slope = 1)
  beta2r <- matrix(c(0, 0, 0.25, -0.4), 2,
    dimnames = list(c("a", "b"), names(beta1r))
  )
  got <- profile_quadratic_coefficients(mean_profile_ball(), beta1r, beta2r,
    points = matrix(0, 1, 2), warm = list(c(0, 0))
  )
  expect_identical(got$beta1$set_lower[1], 2)
  expect_identical(got$beta1$set_upper[1], 2)
  expect_equal(got$beta1$set_lower[2], 1 - sqrt(0.25^2 + 0.4^2), tolerance = 1e-6)
  expect_equal(got$beta1$set_upper[2], 1 + sqrt(0.25^2 + 0.4^2), tolerance = 1e-6)
  point_rows <- attr(got, "profile_points")
  ev <- compute_quadratic_set_evidence(mean_profile_ball(), diag(2), matrix(0, 1, 2))
  expect_true(all(vapply(point_rows, ev$check_point, logical(1))))
  values <- vapply(point_rows, function(w) unname(beta1r[2] - sum(beta2r[, 2] * w)), 0)
  expect_lte(got$beta1$set_lower[2], min(values))
  expect_gte(got$beta1$set_upper[2], max(values))
})

test_that("a rounding-noise loading takes the constant range from the theta enclosure", {
  fit <- mean_profile_fixture(34L)
  # intercept loadings that are rounding noise along directions where the thin
  # tau = 0.05 set defeats the endpoint search
  noise <- list(
    c(0x1.68b25b35d5a9ap-55, 0x1.1318a2b9d225cp-58, -0x1.cab698de27a22p-54),
    c(-0x1.6bab1545d3441p-56, -0x1.b5a7d6b17abd7p-59, 0x1.fb4c12dc4dd42p-55)
  )
  for (loading in noise) {
    fit$beta2r[, 1] <- loading
    got <- profile_mean_tau_path(fit, 0.05)[[1L]]$beta1
    expect_identical(c(got$lower_status[1], got$upper_status[1]), c("bounded", "bounded"))
    at_point <- fit$beta1r[[1]] - sum(loading * fit$point$theta)
    expect_lte(got$set_lower[1], at_point)
    expect_gte(got$set_upper[1], at_point)
    expect_lte(got$set_upper[1] - got$set_lower[1], 1e-15)
  }
})
