test_that("named objectives retain the coordinate and structural map", {
  fit <- mean_profile_fixture()
  got <- build_identified_set_objectives(fit)
  want <- cbind(diag(ncol(fit$w2)), -fit$beta2r)
  dimnames(want) <- list(colnames(fit$w2), c(colnames(fit$w2), names(fit$beta1r)))
  expect_identical(got, want)
  theta <- seq_len(ncol(fit$w2)) / 10
  projected <- drop(crossprod(got, theta))
  expect_equal(unname(projected[seq_len(ncol(fit$w2))]), theta)
  expect_equal(
    tail(projected, length(fit$beta1r)) + fit$beta1r,
    fit$beta1r - drop(crossprod(fit$beta2r, theta))
  )
})

test_that("objective threshold is explicit and the old helper is unchanged", {
  fit <- mean_profile_fixture()
  fit$beta2r[, 1L] <- 0
  exact <- build_identified_set_objectives(fit)
  expect_identical(unname(exact[, ncol(fit$w2) + 1L]), numeric(ncol(fit$w2)))
  got <- build_identified_set_objectives(fit, 1e-4)
  expect_identical(unname(got), identified_set_objectives(fit, ncol(fit$w2), 1e-4))
  expect_error(build_identified_set_objectives(fit, -1), class = "hetid_error_bad_argument")
  expect_error(build_identified_set_objectives(fit, 1), class = "hetid_error_bad_argument")
  expect_error(build_identified_set_objectives(fit, NA_real_), class = "hetid_error")
})

test_that("objective labels reject ambiguous structural names", {
  fit <- mean_profile_fixture()
  names(fit$beta1r)[2L] <- colnames(fit$w2)[1L]
  colnames(fit$beta2r) <- names(fit$beta1r)
  names(fit$beta1) <- names(fit$beta1r)
  expect_error(build_identified_set_objectives(fit), class = "hetid_error_bad_argument")
})
