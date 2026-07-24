#!/usr/bin/env Rscript

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("support", "statistics", "api.R"))
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_cache_validation.R"
))
paper_source_once(paper_path("inference", "bootstrap_stage_cache.R"))

stopifnot(
  length(BOOTSTRAP_STAGE_CACHE_FIELDS) == 7L,
  identical(
    BOOTSTRAP_STAGE_CACHE_FIELDS[4L],
    "volatility_primary_n_failed"
  ),
  identical(
    BOOTSTRAP_STAGE_CACHE_FIELDS[6L],
    "volatility_sensitivity_n_failed"
  )
)

path <- tempfile("bootstrap-stage-cache-", fileext = ".rds")
on.exit(unlink(path), add = TRUE)
calls <- 0L
current <- list(token = 1L)
provenance <- function() {
  calls <<- calls + 1L
  current
}
validator <- function(value, ...) {
  identical(value$provenance, current)
}
original_validate <- bootstrap_stage_cache_validate
assign("bootstrap_stage_cache_validate", validator, envir = .GlobalEnv)
on.exit(assign(
  "bootstrap_stage_cache_validate",
  original_validate,
  envir = .GlobalEnv
), add = TRUE)

saveRDS(list(provenance = current), path, version = 3L)
run_calls <- 0L
out <- bootstrap_stage_cached_or_run(
  path = path,
  mode = "reuse",
  stage_spec = list(),
  provenance = provenance,
  run_fn = function() {
    run_calls <<- run_calls + 1L
    stop("scientific callback ran")
  }
)
stopifnot(
  identical(out$source, "reuse"),
  run_calls == 0L,
  calls >= 2L
)

candidate <- stats::setNames(
  as.list(seq_along(BOOTSTRAP_STAGE_CANDIDATE_FIELDS)),
  BOOTSTRAP_STAGE_CANDIDATE_FIELDS
)
writer <- function(value, destination) {
  saveRDS(value, destination, version = 3L)
}
saveRDS("malformed", path, version = 3L)
malformed_calls <- 0L
malformed <- suppressWarnings(bootstrap_stage_cached_or_run(
  path = path,
  mode = "reuse",
  stage_spec = list(),
  provenance = provenance,
  run_fn = function() {
    malformed_calls <<- malformed_calls + 1L
    candidate
  },
  writer = writer
))
stopifnot(
  identical(malformed$source, "fallback-rerun"),
  malformed_calls == 1L
)

saveRDS(list(provenance = list(token = 0L)), path, version = 3L)
stale_calls <- 0L
stale <- suppressWarnings(bootstrap_stage_cached_or_run(
  path = path,
  mode = "reuse",
  stage_spec = list(),
  provenance = provenance,
  run_fn = function() {
    stale_calls <<- stale_calls + 1L
    candidate
  },
  writer = writer
))
stopifnot(
  identical(stale$source, "fallback-rerun"),
  stale_calls == 1L
)

legacy_dir <- tempfile("bootstrap-stage-legacy-")
dir.create(legacy_dir)
legacy_cache <- file.path(legacy_dir, "bootstrap_stage_draws.rds")
legacy_paths <- file.path(
  legacy_dir,
  BOOTSTRAP_STAGE_LEGACY_CACHE_BASENAMES
)
unrelated <- file.path(legacy_dir, "keep.rds")
saveRDS(list(provenance = current), legacy_cache, version = 3L)
invisible(lapply(c(legacy_paths, unrelated), function(target) {
  saveRDS(TRUE, target, version = 3L)
}))
removed <- bootstrap_stage_remove_legacy_caches(
  legacy_cache,
  function(value) identical(value$provenance, current)
)
stopifnot(
  identical(sort(removed), sort(legacy_paths)),
  !any(file.exists(legacy_paths)),
  file.exists(unrelated),
  file.exists(legacy_cache)
)

cat("test_bootstrap_stage_cache: PASS\n")
