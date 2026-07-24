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

cat("test_bootstrap_stage_cache: PASS\n")
