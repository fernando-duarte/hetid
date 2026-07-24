# Canonical stored MBB families authenticate schedules without regenerating them.

check(
  "the index-family API keeps its five explicit design arguments",
  identical(
    names(formals(paper_mbb_index_family)),
    c("n_draws", "sample_size", "block_length", "seed", "family")
  )
)

primary <- paper_mbb_index_family(
  10000L, 256L, 10L, 20260708L, "primary"
)
sensitivity <- paper_mbb_index_family(
  10000L, 256L, 20L, 20260708L, "doubled_block_sensitivity"
)
check(
  "stored primary and doubled-block schedules retain their pinned hashes",
  identical(
    paper_mbb_index_sha(primary),
    "5701196733f917351bcb22d111bff0d01ed8638daabf18df7561b5857b2a225e"
  ) && identical(
    paper_mbb_index_sha(sensitivity),
    "d2550c39daa842fb3b14b19909bbfab1e26e4f3446c44bd80ce7874baab180ce"
  )
)

local({
  original_index <- mbb_index
  calls <- 0L
  assign("mbb_index", function(...) {
    calls <<- calls + 1L
    original_index(...)
  }, envir = .GlobalEnv)
  on.exit(assign("mbb_index", original_index, envir = .GlobalEnv), add = TRUE)
  family <- paper_mbb_index_family(4L, 11L, 3L, 1L, "primary")
  after_construction <- calls
  family_sha <- paper_mbb_index_sha(family)
  executed <- paper_run_indexed_draws(
    family,
    function(index, draw_id) c(draw_id, sum(index))
  )
  check(
    "construction alone calls mbb_index once per stored draw",
    identical(after_construction, 4L)
  )
  check(
    "hashing and indexed execution consume stored schedules without mbb_index",
    identical(calls, after_construction) &&
      identical(family_sha, family$index_sha256) &&
      identical(executed$indices, family$indices)
  )
})

local({
  family <- paper_mbb_index_family(4L, 11L, 3L, 1L, "primary")
  changed <- unclass(family)
  set.seed(90210L)
  changed$draw_rng_state <- .Random.seed
  changed <- structure(changed, class = "paper_mbb_index_family")
  callbacks <- 0L
  error <- tryCatch(
    paper_run_indexed_draws(changed, function(index, draw_id) {
      callbacks <<- callbacks + 1L
      sum(index)
    }),
    error = conditionMessage
  )
  check(
    "post-index RNG-state mutation is rejected before indexed callbacks",
    identical(error, "paper MBB post-index RNG state is not authenticated") &&
      identical(callbacks, 0L)
  )
})

local({
  draw <- function(index, draw_id) c(draw_id, total = sum(index))
  wrapper <- paper_run_mbb_draws(4L, 11L, 3L, draw, seed = 1L)
  family <- paper_mbb_index_family(4L, 11L, 3L, 1L, "compatibility")
  indexed <- paper_run_indexed_draws(family, draw)
  check(
    "the compatibility wrapper matches the canonical serial executor",
    identical(wrapper$draws, indexed$draws) &&
      identical(wrapper$indices, indexed$indices) &&
      identical(names(wrapper), names(indexed))
  )
  if (.Platform$OS.type == "windows") {
    skip("indexed executor preserves fork behavior", "no fork on windows")
  } else {
    forked <- paper_run_indexed_draws(family, draw, cores = 2L)
    check(
      "the indexed executor retains serial and forked deterministic behavior",
      identical(indexed$draws, forked$draws) &&
        identical(indexed$indices, forked$indices)
    )
  }
})

local({
  old_kind <- RNGkind()
  set.seed(818L)
  old_seed <- .Random.seed
  on.exit(
    {
      do.call(RNGkind, as.list(old_kind))
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    },
    add = TRUE
  )
  zero <- paper_mbb_index_family(2L, 8L, 3L, 0L, "primary")
  null <- paper_mbb_index_family(2L, 8L, 3L, NULL, "primary")
  invalid <- tryCatch(
    paper_run_mbb_draws(0L, 8L, 3L, function(index, draw_id) index, 1L),
    error = conditionMessage
  )
  check(
    "zero and NULL seeds retain their legacy records and restore ambient RNG",
    identical(zero$seed, 0L) && is.null(null$seed) &&
      identical(RNGkind(), old_kind) && identical(.Random.seed, old_seed)
  )
  check(
    "the compatibility wrapper retains legacy validation order",
    identical(invalid, "n_draws >= 1L is not TRUE")
  )
})

rm(primary, sensitivity)
