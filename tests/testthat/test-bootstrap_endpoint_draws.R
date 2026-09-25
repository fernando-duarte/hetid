test_that("circular indices preserve the exact caller RNG schedule", {
  old <- bootstrap_rng_capture()
  on.exit(bootstrap_rng_restore(old))
  reference <- function(n, block) {
    block <- min(block, n)
    starts <- sample.int(n, ceiling(n / block), replace = TRUE)
    unlist(lapply(starts, function(s) (s + 0:(block - 1) - 1) %% n + 1))[seq_len(n)]
  }
  for (block in c(1, 4, 20)) {
    set.seed(91)
    expected <- lapply(seq_len(5), function(i) reference(11, block))
    post <- .Random.seed
    set.seed(91)
    actual <- circular_mbb_indices(11, block, 5)
    expect_identical(actual, expected)
    expect_identical(.Random.seed, post)
    expect_true(all(lengths(actual) == 11))
  }
  set.seed(1)
  idx <- circular_mbb_indices(11, 4)[[1]]
  expect_true(any(diff(idx[1:4]) < 0))
  expect_true(all(diff(idx[1:4]) %% 11 == 1))
  expect_error(circular_mbb_indices(0, 3), class = "hetid_error")
  expect_error(circular_mbb_indices(10, 1.5), class = "hetid_error")
})

test_that("serial bootstrap retains draw identity and feeds inference directly", {
  x <- bootstrap_fixture()
  indices <- stats::setNames(rep(list(seq_len(4)), 60), paste0("draw", seq_len(60)))
  statistic <- function(index, draw_id) {
    if (draw_id == 60) stop("last draw failed")
    out <- x$full
    out$lower <- out$lower + draw_id / 100
    out$upper <- out$upper + draw_id / 100
    out$evidence <- "retained"
    out
  }
  fit <- bootstrap_endpoint_draws(x$full, indices, statistic, seed = 20)
  expect_length(fit$results, 60)
  expect_length(fit$errors, 60)
  expect_identical(rownames(fit$lower), names(indices))
  expect_identical(fit$indices, indices)
  expect_equal(fit$n_callback_failed, 1)
  expect_identical(fit$errors[[60]]$message, "last draw failed")
  expect_true("error" %in% fit$errors[[60]]$classes)
  expect_identical(fit$results[[1]]$evidence, "retained")
  expect_true(is.na(fit$lower[60, 1]))
  expect_identical(fit$upper_status[60, 1], "failed")
  interval <- bootstrap_set_interval(x$full, fit, "pointwise", 0.1, 30, 0.85)
  expect_identical(interval$summary$reason, "reported")
  expect_identical(interval$draws$errors, fit$errors)
  malformed <- function(index, draw_id) if (draw_id == 60) NULL else x$full
  expect_error(bootstrap_endpoint_draws(x$full, indices, malformed),
    "statistic result for draw 60",
    class = "hetid_error"
  )
  wrong <- function(index, draw_id) transform(x$full, coef = "wrong")
  expect_error(bootstrap_endpoint_draws(x$full, indices, wrong), class = "hetid_error")
  expect_error(bootstrap_endpoint_draws(x$full, list(1:4, 1:3), statistic),
    class = "hetid_error"
  )
})

test_that("runner restores RNG kind, seed and absent seed even on contract errors", {
  saved <- bootstrap_rng_capture()
  on.exit(bootstrap_rng_restore(saved))
  x <- bootstrap_fixture()
  callback <- function(index, draw_id) {
    RNGkind("Wichmann-Hill")
    runif(2)
    x$full
  }
  set.seed(21)
  before <- bootstrap_rng_capture()
  bootstrap_endpoint_draws(x$full, list(1:4), callback, seed = 52)
  expect_identical(bootstrap_rng_capture(), before)
  bad <- function(index, draw_id) {
    callback(index, draw_id)
    NULL
  }
  expect_error(bootstrap_endpoint_draws(x$full, list(1:4), bad), class = "hetid_error")
  expect_identical(bootstrap_rng_capture(), before)
  rm(".Random.seed", envir = globalenv())
  bootstrap_endpoint_draws(x$full, list(1:4), callback)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  expect_identical(RNGkind(), before$kind)
})


test_that("all callback errors and returned partial failures keep distinct counts", {
  full <- bootstrap_fixture()$full
  error <- function(index, draw_id) stop("callback failure")
  failed <- bootstrap_endpoint_draws(full, list(a = 1:3, b = 1:3), error)
  expect_equal(failed$n_callback_failed, 2)
  expect_true(all(is.na(failed$lower)))
  expect_true(all(failed$lower_status == "failed"))
  out <- bootstrap_set_interval(full, failed, "containment", 0.1, 1, 0.5)
  expect_identical(out$summary$reason, "insufficient bounded draws")
  partial <- function(index, draw_id) {
    value <- full
    value$lower <- NA_real_
    value$lower_status <- "failed"
    value
  }
  returned <- bootstrap_endpoint_draws(full, list(a = 1:3, b = 1:3), partial)
  expect_equal(returned$n_callback_failed, 0)
  expect_true(all(returned$lower_status == "failed"))
  expect_true(all(returned$upper_status == "bounded"))
})


test_that("an unchanged Rounding kind does not warn on restore", {
  saved <- bootstrap_rng_capture()
  on.exit(bootstrap_rng_restore(saved))
  expect_warning(RNGkind(sample.kind = "Rounding"), "non-uniform")
  full <- bootstrap_fixture()$full
  callback <- function(index, draw_id) full
  expect_silent(bootstrap_endpoint_draws(full, list(1:3), callback))
  expect_identical(RNGkind()[3], "Rounding")
})
