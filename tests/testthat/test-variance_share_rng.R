test_that("share orchestration fixes the search kind and restores present or absent caller seed", {
  prepared <- variance_share_fixture()
  observed <- NULL
  local_mocked_bindings(profile_mean_tau_path = function(...) {
    observed <<- RNGkind()
    runif(1)
    stop_hetid("Downstream share probe")
  })
  with_rng_scope({
    for (present in c(TRUE, FALSE)) {
      RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
      set.seed(72)
      kind <- RNGkind()
      if (!present) rm(".Random.seed", envir = globalenv())
      seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
      expect_error(compute_variance_shares(prepared), "Downstream share probe",
        class = "hetid_error"
      )
      expect_identical(observed, c("Mersenne-Twister", "Inversion", "Rejection"))
      expect_identical(RNGkind(), kind)
      expect_identical(get0(".Random.seed", envir = globalenv(), inherits = FALSE), seed)
      bad <- prepared
      bad$x[, 2] <- bad$x[, 2] + bad$x[, 1]
      expect_error(compute_variance_shares(bad), "uncorrelated", class = "hetid_error")
      expect_identical(RNGkind(), kind)
      expect_identical(get0(".Random.seed", envir = globalenv(), inherits = FALSE), seed)
    }
  })
})

test_that("successful share output preserves caller RNG state", {
  prepared <- variance_share_fixture()
  control <- VARIANCE_SHARE_CONTROL
  control$taus <- .05
  control$grid_points_per_axis <- 11L
  with_rng_scope({
    for (present in c(TRUE, FALSE)) {
      RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
      set.seed(19)
      kind <- RNGkind()
      if (!present) rm(".Random.seed", envir = globalenv())
      seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
      expect_type(compute_variance_shares(prepared, control), "list")
      expect_identical(RNGkind(), kind)
      expect_identical(get0(".Random.seed", envir = globalenv(), inherits = FALSE), seed)
    }
  })
})
