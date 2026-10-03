test_that("paths execute upward, retain original anchors and return requested order", {
  fit <- mean_profile_fixture()
  seen <- list()
  testthat::local_mocked_bindings(
    build_quadratic_system = function(gamma, tau, moments) list(quadratic = list(tau = tau[1])),
    profile_tables_widened = function(quadratic, beta1r, beta2r, points, warm, control) {
      seen[[length(seen) + 1L]] <<- list(tau = quadratic$tau, points = points, warm = warm)
      structure(list(theta = data.frame(tau = quadratic$tau)),
        profile_points = list(rep(quadratic$tau, nrow(beta2r)))
      )
    }, .package = "hetid"
  )
  got <- profile_mean_tau_path(fit, c(0.2, 0.05), through = c(0.1, 0.1))
  expect_identical(vapply(seen, `[[`, numeric(1), "tau"), c(0.05, 0.1, 0.2))
  expect_identical(names(got), sprintf("%.17g", c(0.2, 0.05)))
  expect_identical(seen[[1]]$warm, list(fit$point$theta))
  expect_identical(seen[[2]]$warm, list(rep(0.05, ncol(fit$w2))))
  expect_identical(seen[[3]]$warm, list(rep(0.1, ncol(fit$w2))))
  expect_true(all(vapply(seen, function(x) {
    identical(x$points, matrix(fit$point$theta, nrow = 1L))
  }, logical(1))))
})

test_that("paths preserve caller RNG state when a multistart solve fails", {
  fit <- mean_profile_fixture()
  testthat::local_mocked_bindings(profile_tables_widened = function(...) {
    stats::runif(3)
    stop_hetid("multistart failed")
  }, .package = "hetid")
  with_rng_scope(
    {
      set.seed(52)
      seed <- .Random.seed
      kind <- RNGkind()
      expect_error(profile_mean_tau_path(fit, 0.1), "multistart failed", class = "hetid_error")
      expect_identical(RNGkind(), kind)
      expect_identical(.Random.seed, seed)
      rm(".Random.seed", envir = globalenv())
      expect_error(profile_mean_tau_path(fit, 0.1), class = "hetid_error")
      expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    },
    kind = c("L'Ecuyer-CMRG", "Inversion", "Rejection")
  )
})

test_that("paths reject duplicate, out-of-domain and absent-point inputs", {
  fit <- mean_profile_fixture()
  expect_error(profile_mean_tau_path(fit, c(0.1, 0.1)), class = "hetid_error_bad_argument")
  expect_error(profile_mean_tau_path(fit, 1), class = "hetid_error_bad_argument")
  expect_error(profile_mean_tau_path(fit, 0.1, through = NA_real_), class = "hetid_error")
  fit$point <- NULL
  fit$beta1 <- NULL
  expect_error(profile_mean_tau_path(fit, 0.1), "tau-zero point", class = "hetid_error")
})
