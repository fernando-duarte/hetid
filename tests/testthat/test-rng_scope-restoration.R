test_that("legacy-kind cleanup survives warning escalation and exiting handlers", {
  saved <- bootstrap_rng_capture()
  old_options <- options(warn = 0)
  on.exit({
    options(old_options)
    bootstrap_rng_restore(saved)
  })
  fixed <- c("Mersenne-Twister", "Inversion", "Rejection")
  failure <- structure(list(message = "draw failed", call = NULL),
    class = c("rng_callback_error", "error", "condition")
  )
  for (mode in c("escalate", "handler")) {
    for (present in c(TRUE, FALSE)) {
      for (callback_error in c(FALSE, TRUE)) {
        options(warn = 0)
        expect_warning(RNGkind("Mersenne-Twister", "Inversion", "Rounding"), "non-uniform")
        set.seed(541)
        if (!present) rm(".Random.seed", envir = globalenv())
        before <- bootstrap_rng_capture()
        options(warn = if (mode == "escalate") 2 else 0)
        evaluate <- function() {
          with_rng_scope(
            {
              runif(2)
              if (callback_error) stop(failure)
              37L
            },
            seed = 17,
            kind = fixed
          )
        }
        actual <- if (mode == "escalate") {
          tryCatch(evaluate(), error = identity)
        } else {
          tryCatch(evaluate(), warning = identity, error = identity)
        }
        expect_identical(actual, if (callback_error) failure else 37L)
        expect_identical(bootstrap_rng_capture(), before)
        expect_identical(getOption("warn"), if (mode == "escalate") 2L else 0L)
      }
    }
  }
})

test_that("bootstrap callers receive the same protected legacy-kind cleanup", {
  saved <- bootstrap_rng_capture()
  old_options <- options(warn = 0)
  on.exit({
    options(old_options)
    bootstrap_rng_restore(saved)
  })
  full <- bootstrap_fixture()$full
  for (present in c(TRUE, FALSE)) {
    expect_warning(RNGkind(sample.kind = "Rounding"), "non-uniform")
    set.seed(541)
    if (!present) rm(".Random.seed", envir = globalenv())
    before <- bootstrap_rng_capture()
    options(warn = 2)
    callback <- function(index, draw_id) {
      RNGkind(sample.kind = "Rejection")
      runif(2)
      full
    }
    result <- bootstrap_endpoint_draws(full, list(1:4), callback)
    expect_equal(result$n_callback_failed, 0)
    expect_identical(bootstrap_rng_capture(), before)
    malformed <- function(index, draw_id) {
      callback(index, draw_id)
      NULL
    }
    expect_error(bootstrap_endpoint_draws(full, list(1:4), malformed),
      class = "hetid_error_bad_argument"
    )
    expect_identical(bootstrap_rng_capture(), before)
    options(warn = 0)
  }
})

test_that("explicit setup and code warnings keep the caller's warning policy", {
  saved <- bootstrap_rng_capture()
  old_options <- options(warn = 0)
  on.exit({
    options(old_options)
    bootstrap_rng_restore(saved)
  })
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(541)
  before <- bootstrap_rng_capture()
  legacy <- c("Mersenne-Twister", "Inversion", "Rounding")
  expect_warning(with_rng_scope(NULL, kind = legacy), "non-uniform")
  expect_identical(bootstrap_rng_capture(), before)
  options(warn = 2)
  setup_error <- tryCatch(with_rng_scope(NULL, kind = legacy), error = identity)
  expect_s3_class(setup_error, "hetid_error_bad_argument")
  expect_match(conditionMessage(setup_error), "converted from warning.*non-uniform")
  expect_identical(bootstrap_rng_capture(), before)
  expect_error(with_rng_scope({
    runif(2)
    warning("code warning")
  }), "converted from warning.*code warning")
  expect_identical(bootstrap_rng_capture(), before)
  options(warn = 0)
  caught <- tryCatch(with_rng_scope(NULL, kind = legacy), warning = identity)
  expect_s3_class(caught, "warning")
  expect_match(conditionMessage(caught), "non-uniform")
  expect_identical(bootstrap_rng_capture(), before)
  warning_condition <- simpleWarning("code warning")
  caught <- tryCatch(with_rng_scope({
    RNGkind("Wichmann-Hill")
    runif(2)
    warning(warning_condition)
  }), warning = identity)
  expect_identical(caught, warning_condition)
  expect_identical(bootstrap_rng_capture(), before)
})
