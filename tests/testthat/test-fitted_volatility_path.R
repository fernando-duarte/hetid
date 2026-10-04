test_that("sweep admission rejects invalid controls, caps and slacks before mutation", {
  setup <- fv_test_path_setup()
  calls <- 0L
  local_mocked_bindings(profile_mean_tau_path = function(...) {
    calls <<- calls + 1L
    stop("unexpected mean fit")
  })
  bad_taus <- list(numeric(), c(0.1, 0.1), 0, 1, NA_real_, Inf, c(0.05, 0.6), 0.7)
  for (taus in bad_taus) {
    expect_error(profile_fitted_volatility_path(setup$sets, setup$fit, 0.6, taus),
      class = "hetid_error_bad_argument"
    )
  }
  for (cap in list(0, -Inf, NA_real_, c(0.5, 0.6))) {
    expect_error(profile_fitted_volatility_path(setup$sets, setup$fit, cap, 0.05),
      class = "hetid_error_bad_argument"
    )
  }
  for (budget in c(0, Inf)) {
    control <- fv_test_control()
    control$search$ENVELOPE_FIT_BUDGET <- budget
    expect_error(profile_fitted_volatility_path(setup$sets, setup$fit, 0.6, 0.05,
      control = control
    ), class = "hetid_error_bad_argument")
  }
  expect_identical(calls, 0L)
  expect_length(setup$calls$fits, 0L)
  expect_length(ls(setup$sets$cache), 0L)
  error <- tryCatch(profile_fitted_volatility_path(
    setup$sets, setup$fit, 0.1,
    c(0.05, 0.1, 0.2)
  ), hetid_error = identity)
  expect_match(conditionMessage(error), "0.10000000000000001")
  expect_match(conditionMessage(error), "0.20000000000000001")
})

test_that("full mean samples and original anchors bind the path", {
  setup <- fv_test_path_setup()
  changed <- setup$sets
  changed$sample$prep$w1_mean[40L] <- changed$sample$prep$w1_mean[40L] + 1
  changed <- fv_test_rehash(changed)
  expect_error(profile_fitted_volatility_path(changed, setup$fit, 0.6, 0.05),
    "complete prepared mean sample",
    class = "hetid_error_bad_argument"
  )
  for (point in list(NULL, setup$fit$point$theta + 0.1)) {
    changed <- setup$sets
    changed$request$point <- point
    expect_error(profile_fitted_volatility_path(changed, setup$fit, 0.6, 0.05),
      class = "hetid_error_bad_argument"
    )
  }
  stale <- setup$fit
  stale$point$theta <- stale$point$theta + 0.1
  expect_error(profile_fitted_volatility_path(setup$sets, stale, 0.6, 0.05),
    "stale",
    class = "hetid_error_bad_argument"
  )
  expect_length(ls(setup$sets$cache), 0L)
})

test_that("ordered envelopes use one warm mean chain and retain adjacent tau keys", {
  setup <- fv_test_path_setup()
  taus <- c(0.1 + .Machine$double.eps, 0.05, 0.1)
  through <- c(0.025, 0.2)
  seen <- list()
  local_mocked_bindings(
    profile_mean_tau_path = function(fit, taus, through, control) {
      seen$mean <<- list(fit = fit, taus = taus, through = through, control = control)
      stats::setNames(lapply(taus, function(tau) {
        geometry <- fv_test_system()
        list(quadratic = geometry$quadratic, theta = geometry$table)
      }), profile_tau_key(taus))
    },
    profile_fitted_volatility = function(sets, quadratic, theta_table, tau, control) {
      seen$order <<- c(seen$order, tau)
      list(
        metadata = list(sample_id = sets$sample$sample_id),
        diagnostics = list(engine = list(n_raw_feasible = 3L))
      )
    }
  )
  result <- profile_fitted_volatility_path(setup$sets, setup$fit, Inf, taus,
    through = through, control = fv_test_control()
  )
  expect_identical(seen$order, sort(taus))
  expect_identical(seen$mean$taus, sort(taus))
  expect_identical(seen$mean$through, through)
  expect_identical(seen$mean$control, fv_test_control()$sets)
  expect_identical(result$grid, sort(taus))
  expect_identical(names(result$envelopes), profile_tau_key(sort(taus)))
  expect_length(unique(names(result$envelopes)), 3L)
  expect_identical(result$diagnostics, list())
})

test_that("sweeps restore seed presence and RNG kind on success and error", {
  setup <- fv_test_path_setup()
  geometry <- fv_test_system()
  local_mocked_bindings(profile_mean_tau_path = function(fit, taus, through, control) {
    stats::setNames(lapply(taus, function(tau) {
      list(quadratic = geometry$quadratic, theta = geometry$table)
    }), profile_tau_key(taus))
  })
  with_rng_scope(
    {
      for (present in c(TRUE, FALSE)) {
        if (present) {
          set.seed(823L)
        } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
          rm(".Random.seed", envir = globalenv())
        }
        kind <- RNGkind()
        seed <- if (present) get(".Random.seed", globalenv()) else NULL
        for (fail in c(FALSE, TRUE)) {
          local_mocked_bindings(profile_fitted_volatility = function(...) {
            runif(1)
            if (fail) stop_hetid("envelope probe failed")
            list(metadata = list(), diagnostics = list(engine = list(n_raw_feasible = 3L)))
          })
          if (fail) {
            expect_error(profile_fitted_volatility_path(setup$sets, setup$fit, 0.6, 0.05),
              class = "hetid_error"
            )
          } else {
            profile_fitted_volatility_path(setup$sets, setup$fit, 0.6, 0.05)
          }
          expect_identical(RNGkind(), kind)
          expect_identical(exists(".Random.seed", envir = globalenv(), inherits = FALSE), present)
          if (present) expect_identical(get(".Random.seed", globalenv()), seed)
        }
      }
    },
    kind = c("L'Ecuyer-CMRG", "Inversion", "Rejection")
  )
})

test_that("sample, intercept, design and date admission is explicit", {
  setup <- fv_test_sets()
  geometry <- fv_test_system()
  mutations <- list(
    function(s) {
      s$sample$w1[1L] <- 99
      s
    },
    function(s) {
      s$sample$response_date <- as.numeric(s$sample$response_date)
      fv_test_rehash(s)
    },
    function(s) {
      s$sample$response_date <- rev(s$sample$response_date)
      fv_test_rehash(s)
    },
    function(s) {
      s$sample$response_date[1L] <- s$sample$response_date[2L]
      fv_test_rehash(s)
    },
    function(s) {
      s$sample$response_date[1L] <- s$sample$response_date[1L] - 1
      fv_test_rehash(s)
    },
    function(s) {
      s$sample$x_mat[, 1L] <- 0
      fv_test_rehash(s)
    },
    function(s) {
      s$sample$x_mat[1L, 2L] <- Inf
      fv_test_rehash(s)
    },
    function(s) {
      s$estimator$theta_labels <- "foreign"
      s
    },
    function(s) {
      s$key <- "logols"
      s
    }
  )
  for (mutate in mutations) {
    expect_error(profile_fitted_volatility(
      mutate(setup$sets), geometry$quadratic,
      geometry$table, 0.05
    ), class = "hetid_error_bad_argument")
  }
  expect_length(ls(setup$sets$cache), 0L)
  expect_length(setup$calls$fits, 0L)
})
