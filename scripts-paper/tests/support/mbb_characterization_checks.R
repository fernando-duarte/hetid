# Frozen behavior of the legacy RNG-owning MBB runner. These checks complement
# mbb_checks.R without extending that file beyond the 200-line limit.

mbc_index_run <- paper_run_mbb_draws(
  n_draws = 4L,
  sample_size = 11L,
  block_length = 3L,
  seed = 1L,
  draw = function(index, draw_id) index
)
check(
  "a callback receives the exact pre-drawn index objects",
  identical(mbc_index_run$draws, mbc_index_run$indices)
)
check(
  "the small legacy index-family serialization is pinned",
  identical(
    paper_sha256_object(mbc_index_run$indices),
    "7eabe93e17b6c69ff39d0aae012f80394ad26655fdffe707fa47e3080e232b3f"
  )
)

local({
  old_kind <- RNGkind()
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (old_seed_exists) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
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

  mbc_random <- paper_run_mbb_draws(
    n_draws = 4L,
    sample_size = 11L,
    block_length = 3L,
    seed = 1L,
    draw = function(index, draw_id) stats::runif(1L)
  )
  check(
    "serial callback RNG starts after the legacy index schedule",
    identical(
      unlist(mbc_random$draws, use.names = FALSE),
      c(
        0x1.8e0d4cdp-1,
        0x1.de91af1ep-1,
        0x1.b277c738p-3,
        0x1.4da82f12p-1
      )
    )
  )
  check(
    "a successful random callback run restores ambient RNG state",
    identical(RNGkind(), ambient_kind) &&
      identical(.Random.seed, ambient_seed)
  )

  mbc_failed <- paper_run_mbb_draws(
    n_draws = 4L,
    sample_size = 11L,
    block_length = 3L,
    seed = 1L,
    draw = function(index, draw_id) {
      value <- stats::runif(1L)
      if (draw_id == 2L) stop("rng failure fixture")
      value
    }
  )
  check(
    "a failed callback consumes its draw RNG and later draws keep their positions",
    identical(
      mbc_failed$draws,
      list(
        0x1.8e0d4cdp-1,
        "rng failure fixture",
        0x1.b277c738p-3,
        0x1.4da82f12p-1
      )
    ) &&
      identical(mbc_failed$failed, c(FALSE, TRUE, FALSE, FALSE)) &&
      identical(mbc_failed$n_failed, 1L)
  )
  check(
    "a captured callback failure restores ambient RNG state",
    identical(RNGkind(), ambient_kind) &&
      identical(.Random.seed, ambient_seed)
  )

  rm(".Random.seed", envir = .GlobalEnv)
  paper_run_mbb_draws(
    n_draws = 2L,
    sample_size = 8L,
    block_length = 3L,
    seed = 21L,
    draw = function(index, draw_id) sum(index)
  )
  check(
    "a runner call does not leave an ambient seed when none existed at entry",
    !exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  )
})

if (.Platform$OS.type == "windows") {
  skip("forked random callbacks preserve caller RNG", "no fork on windows")
} else {
  local({
    old_kind <- RNGkind()
    on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE)
    RNGkind("Wichmann-Hill")
    set.seed(887L)
    ambient_kind <- RNGkind()
    ambient_seed <- .Random.seed
    forked <- paper_run_mbb_draws(
      n_draws = 4L,
      sample_size = 11L,
      block_length = 3L,
      seed = 1L,
      cores = 2L,
      draw = function(index, draw_id) stats::runif(1L)
    )
    check(
      "forked random callbacks preserve caller RNG and the stored schedule",
      identical(RNGkind(), ambient_kind) &&
        identical(.Random.seed, ambient_seed) &&
        identical(forked$indices, mbc_index_run$indices) &&
        length(forked$draws) == 4L &&
        all(vapply(forked$draws, is.numeric, logical(1)))
    )
  })
}

rm(mbc_index_run)
