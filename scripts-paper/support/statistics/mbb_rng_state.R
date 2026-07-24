# Ambient RNG snapshots used by stored MBB family construction and execution.

.paper_mbb_rng_capture <- function() {
  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  list(
    kind = RNGkind(),
    seed_exists = seed_exists,
    seed = if (seed_exists) get(".Random.seed", .GlobalEnv) else NULL
  )
}

.paper_mbb_rng_restore <- function(snapshot) {
  do.call(RNGkind, as.list(snapshot$kind))
  if (snapshot$seed_exists) {
    assign(".Random.seed", snapshot$seed, envir = .GlobalEnv)
  } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  invisible(NULL)
}

.paper_mbb_rng_install <- function(kind, seed_state) {
  do.call(RNGkind, as.list(kind))
  assign(".Random.seed", seed_state, envir = .GlobalEnv)
  invisible(NULL)
}
