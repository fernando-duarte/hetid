#!/usr/bin/env Rscript

source(file.path("scripts-paper", "config", "paths.R"))
runner_path <- paper_path("inference", "run_bootstrap_stage.R")
stopifnot(file.exists(runner_path))

runner_text <- paste(readLines(runner_path, warn = FALSE), collapse = "\n")
count_call <- function(name) {
  hits <- gregexpr(paste0(name, "[[:space:]]*\\("), runner_text, perl = TRUE)[[1L]]
  if (identical(hits[[1L]], -1L)) 0L else length(hits)
}

stopifnot(
  count_call("paper_mbb_index_family") == 2L,
  count_call("bootstrap_stage_cached_or_run") == 1L,
  count_call("paper_run_mbb_draws") == 0L,
  !grepl("bootstrap_capsule|system2\\(|callr::", runner_text),
  grepl("bootstrap_stage_results", runner_text, fixed = TRUE)
)

runner <- parse(file = runner_path)
is_source_call <- function(expression) {
  is.call(expression) &&
    identical(as.character(expression[[1L]]), "paper_source_once")
}
invisible(lapply(
  Filter(is_source_call, as.list(runner)),
  eval,
  envir = .GlobalEnv
))

stopifnot(
  identical(
    BOOTSTRAP_STAGE_CACHE_FIELDS,
    c(
      "anchor", "mean", "volatility_primary",
      "volatility_primary_n_failed", "volatility_sensitivity",
      "volatility_sensitivity_n_failed", "provenance"
    )
  ),
  is.function(bootstrap_stage_candidate),
  is.function(bootstrap_stage_results)
)

cat("test_bootstrap_stage: PASS\n")
