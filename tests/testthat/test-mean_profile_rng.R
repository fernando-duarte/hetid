test_that("every mean wrapper selects fixed RNG kind and restores caller state", {
  fit <- mean_profile_fixture()
  control <- MEAN_TAU_CONTROL
  control$cap <- 0.1
  control$sweep_step <- 0.1
  wrappers <- list(
    coefficients = function() {
      profile_quadratic_coefficients(mean_profile_ball(3), fit$beta1r, fit$beta2r)
    },
    path = function() profile_mean_tau_path(fit, 0.1),
    transition = function() find_mean_tau_star(fit, control)
  )
  original <- new_hetid_error("RNG scope probe", "hetid_error_rng_probe")
  seen <- list()
  fail <- FALSE
  observe <- function() {
    seen[[length(seen) + 1L]] <<- RNGkind()[1:3]
    stats::runif(1)
    if (fail) stop(original)
  }
  testthat::local_mocked_bindings(
    profile_tables_widened = function(...) {
      observe()
      structure(list(), profile_points = list(rep(0, 3)))
    },
    profile_mean_tau_status = function(...) {
      observe()
      "bounded"
    }, .package = "hetid"
  )
  with_rng_scope(
    {
      for (run in wrappers) {
        for (fail in c(FALSE, TRUE)) {
          for (present in c(TRUE, FALSE)) {
            set.seed(52)
            if (!present) rm(".Random.seed", envir = globalenv())
            saved <- if (present) get(".Random.seed", envir = globalenv()) else NULL
            caller_kind <- RNGkind()
            seen <- list()
            result <- tryCatch(run(), error = identity)
            if (fail) {
              expect_identical(result, original)
            } else {
              expect_false(inherits(result, "error"))
            }
            expect_gt(length(seen), 0L)
            expect_true(all(vapply(seen, identical, logical(1),
              y = c("Mersenne-Twister", "Inversion", "Rejection")
            )))
            expect_identical(RNGkind(), caller_kind)
            expect_identical(
              exists(".Random.seed", envir = globalenv(), inherits = FALSE), present
            )
            if (present) expect_identical(get(".Random.seed", envir = globalenv()), saved)
          }
        }
      }
    },
    kind = c("L'Ecuyer-CMRG", "Inversion", "Rejection")
  )
})
