test_that("explicit scoped RNG setup reproduces draws and restores caller state", {
  saved <- bootstrap_rng_capture()
  on.exit(bootstrap_rng_restore(saved))
  fixed <- c("Mersenne-Twister", "Inversion", "Rejection")
  do.call(RNGkind, as.list(fixed))
  set.seed(17)
  expected <- list(uniform = runif(5), normal = rnorm(5), sample = sample.int(31, 7))
  for (present in c(TRUE, FALSE)) {
    RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
    set.seed(29)
    if (!present) rm(".Random.seed", envir = globalenv())
    before <- bootstrap_rng_capture()
    actual <- with_rng_scope(
      {
        expect_identical(RNGkind(), fixed)
        list(uniform = runif(5), normal = rnorm(5), sample = sample.int(31, 7))
      },
      seed = 17,
      kind = fixed
    )
    expect_identical(actual, expected)
    expect_identical(bootstrap_rng_capture(), before)
  }
})

test_that("default scope uses the current stream without changing its schedule", {
  saved <- bootstrap_rng_capture()
  on.exit(bootstrap_rng_restore(saved))
  RNGkind("Wichmann-Hill", "Inversion", "Rejection")
  set.seed(19)
  before <- bootstrap_rng_capture()
  expected <- runif(6)
  bootstrap_rng_restore(before)
  expect_identical(with_rng_scope(runif(6)), expected)
  expect_identical(bootstrap_rng_capture(), before)
  set.seed(23)
  expected <- runif(6)
  bootstrap_rng_restore(before)
  expect_identical(with_rng_scope(runif(6), seed = 23), expected)
  expect_identical(bootstrap_rng_capture(), before)
})

test_that("scope restores state on expression errors without replacing the condition", {
  saved <- bootstrap_rng_capture()
  on.exit(bootstrap_rng_restore(saved))
  failure <- structure(list(message = "draw failed", call = NULL),
    class = c("rng_callback_error", "error", "condition")
  )
  for (present in c(TRUE, FALSE)) {
    RNGkind("Wichmann-Hill", "Inversion", "Rejection")
    set.seed(29)
    if (!present) rm(".Random.seed", envir = globalenv())
    before <- bootstrap_rng_capture()
    caught <- tryCatch(with_rng_scope(
      {
        RNGkind("L'Ecuyer-CMRG")
        runif(4)
        stop(failure)
      },
      seed = 17,
      kind = c("Mersenne-Twister", "Inversion", "Rejection")
    ), error = identity)
    expect_identical(caught, failure)
    expect_identical(bootstrap_rng_capture(), before)
  }
})

test_that("nested scopes return each caller's stream and preserve expression visibility", {
  saved <- bootstrap_rng_capture()
  on.exit(bootstrap_rng_restore(saved))
  set.seed(29)
  before <- bootstrap_rng_capture()
  outer <- with_rng_scope(
    {
      runif(2)
      inner_before <- bootstrap_rng_capture()
      with_rng_scope(runif(3), seed = 37, kind = c("Wichmann-Hill", "Inversion", "Rejection"))
      expect_identical(bootstrap_rng_capture(), inner_before)
      runif(2)
    },
    seed = 31
  )
  expect_length(outer, 2)
  expect_identical(bootstrap_rng_capture(), before)
  value <- 43
  expect_identical(with_rng_scope(value), value)
  expect_identical(
    withVisible(with_rng_scope(invisible(value))),
    list(value = value, visible = FALSE)
  )
  expect_identical(withVisible(with_rng_scope(value)), list(value = value, visible = TRUE))
})

test_that("invalid RNG setup has structured errors and does not evaluate the expression", {
  saved <- bootstrap_rng_capture()
  on.exit(bootstrap_rng_restore(saved))
  evaluated <- FALSE
  invalid_seeds <- list(-1, 1.5, NA_real_, Inf, "17", numeric(0), .Machine$integer.max + 1)
  invalid_kinds <- list(
    character(0), "Mersenne-Twister", rep(NA_character_, 3),
    c("", "Inversion", "Rejection"), c("bad", "Inversion", "Rejection"),
    c("Wichmann-Hill", "bad", "Rejection"), c("Wichmann-Hill", "Inversion", "bad")
  )
  for (present in c(TRUE, FALSE)) {
    RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
    set.seed(29)
    if (!present) rm(".Random.seed", envir = globalenv())
    before <- bootstrap_rng_capture()
    for (seed in invalid_seeds) {
      error <- tryCatch(with_rng_scope(evaluated <- TRUE, seed = seed), error = identity)
      expect_s3_class(error, "hetid_error_bad_argument")
      expect_identical(error$arg, "seed")
      expect_identical(bootstrap_rng_capture(), before)
    }
    for (kind in invalid_kinds) {
      error <- tryCatch(with_rng_scope(evaluated <- TRUE, kind = kind), error = identity)
      expect_s3_class(error, "hetid_error_bad_argument")
      expect_identical(error$arg, "kind")
      expect_identical(bootstrap_rng_capture(), before)
    }
  }
  expect_false(evaluated)
})

test_that("warnings propagate and unchanged Rounding scopes restore silently", {
  saved <- bootstrap_rng_capture()
  on.exit(bootstrap_rng_restore(saved))
  expect_warning(RNGkind(sample.kind = "Rounding"), "non-uniform")
  set.seed(29)
  before <- bootstrap_rng_capture()
  expect_silent(with_rng_scope(runif(2)))
  expect_identical(bootstrap_rng_capture(), before)
  expect_warning(with_rng_scope({
    runif(2)
    warning("draw warning")
  }), "draw warning")
  expect_identical(bootstrap_rng_capture(), before)
})

test_that("all RNG kind components and absent seeds survive a scope without draws", {
  saved <- bootstrap_rng_capture()
  on.exit(bootstrap_rng_restore(saved))
  RNGkind("L'Ecuyer-CMRG", "Box-Muller", "Rejection")
  set.seed(29)
  for (present in c(TRUE, FALSE)) {
    if (!present) rm(".Random.seed", envir = globalenv())
    before <- bootstrap_rng_capture()
    expect_identical(with_rng_scope(NULL,
      kind = c("Mersenne-Twister", "Inversion", "Rejection")
    ), NULL)
    expect_identical(bootstrap_rng_capture(), before)
    expect_identical(with_rng_scope(NULL), NULL)
    expect_identical(bootstrap_rng_capture(), before)
  }
})
