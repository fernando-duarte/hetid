# Indexed callbacks use the authenticated post-index RNG state and restore callers.

local({
  old_kind <- RNGkind()
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (old_seed_exists) .Random.seed else NULL
  on.exit(
    {
      do.call(RNGkind, as.list(old_kind))
      if (old_seed_exists) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  RNGkind("Wichmann-Hill", "Inversion", "Rejection")
  set.seed(881L)
  ambient_kind <- RNGkind()
  ambient_seed <- .Random.seed
  family <- paper_mbb_index_family(4L, 11L, 3L, 1L, "primary")
  check(
    "index construction pins the full RNG protocol and restores ambient state",
    identical(family$rng_kind, c("Mersenne-Twister", "Inversion", "Rejection")) &&
      identical(RNGkind(), ambient_kind) && identical(.Random.seed, ambient_seed)
  )
  do.call(RNGkind, as.list(family$rng_kind))
  assign(".Random.seed", family$draw_rng_state, envir = .GlobalEnv)
  expected <- stats::runif(family$n_draws)
  do.call(RNGkind, as.list(ambient_kind))
  assign(".Random.seed", ambient_seed, envir = .GlobalEnv)
  random <- paper_run_indexed_draws(family, function(index, draw_id) stats::runif(1L))
  check(
    "serial random callbacks start from the authenticated post-index state",
    identical(unlist(random$draws, use.names = FALSE), expected)
  )
  check(
    "successful random callbacks restore ambient RNG kind and seed",
    identical(RNGkind(), ambient_kind) && identical(.Random.seed, ambient_seed)
  )
  failed <- paper_run_indexed_draws(family, function(index, draw_id) {
    value <- stats::runif(1L)
    if (draw_id == 2L) stop("rng failure fixture")
    value
  })
  expected_failed <- as.list(expected)
  expected_failed[[2L]] <- "rng failure fixture"
  check(
    "failed random callbacks consume their RNG slot and retain later draw positions",
    identical(failed$draws, expected_failed) &&
      identical(failed$failed, c(FALSE, TRUE, FALSE, FALSE)) && failed$n_failed == 1L
  )
  check(
    "captured callback failures restore ambient RNG state",
    identical(RNGkind(), ambient_kind) && identical(.Random.seed, ambient_seed)
  )
  progress_error <- tryCatch(
    paper_run_indexed_draws(
      family, function(index, draw_id) stats::runif(1L),
      progress = function(...) stop("progress fixture")
    ),
    error = conditionMessage
  )
  check(
    "an escaping progress error restores ambient RNG state",
    identical(progress_error, "progress fixture") &&
      identical(RNGkind(), ambient_kind) && identical(.Random.seed, ambient_seed)
  )
  rm(".Random.seed", envir = .GlobalEnv)
  no_seed_family <- paper_mbb_index_family(2L, 8L, 3L, 21L, "primary")
  paper_run_indexed_draws(no_seed_family, function(index, draw_id) stats::runif(1L))
  check(
    "construction and execution preserve an absent ambient seed",
    !exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  )
  if (.Platform$OS.type == "windows") {
    skip("forked random callbacks preserve caller RNG", "no fork on windows")
  } else {
    set.seed(887L)
    ambient_seed <- .Random.seed
    forked <- paper_run_indexed_draws(
      family, function(index, draw_id) stats::runif(1L),
      cores = 2L
    )
    check(
      "forked random callbacks preserve caller RNG and the stored schedule",
      identical(RNGkind(), ambient_kind) && identical(.Random.seed, ambient_seed) &&
        identical(forked$indices, family$indices) && length(forked$draws) == 4L &&
        all(vapply(forked$draws, is.numeric, logical(1)))
    )
  }
})
