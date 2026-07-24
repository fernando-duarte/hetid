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

manifest <- bootstrap_stage_code_manifest()
stopifnot(all(c(
  "support/statistics/mbb_protocol_authority.R",
  "support/inference/bootstrap_stage_logvar_controls.R",
  "log_variance/estimators/controls.R",
  "log_variance/estimators/set_orchestration.R",
  "log_variance/estimators/shared.R",
  "log_variance/estimators/log_ols/estimator.R"
) %in% manifest))

execution_text <- paste(
  readLines(paper_path(
    "support", "inference", "bootstrap_stage_execution.R"
  ), warn = FALSE),
  collapse = "\n"
)
gate_offset <- function(name) {
  offset <- regexpr(name, execution_text, fixed = TRUE)[[1L]]
  stopifnot(offset > 0L)
  offset
}
stopifnot(
  gate_offset("bootstrap_stage_mean_transport_gate") <
    gate_offset("bootstrap_stage_anchor_gate"),
  gate_offset("bootstrap_stage_anchor_gate") <
    gate_offset("bootstrap_stage_volatility_transport_gate"),
  gate_offset("bootstrap_stage_volatility_transport_gate") <
    gate_offset("\"primary\", control"),
  gate_offset("\"primary\", control") <
    gate_offset("sensitivity <- paper_run_indexed_draws"),
  gate_offset("sensitivity <- paper_run_indexed_draws") <
    gate_offset("\"sensitivity\", control")
)

bounded <- PAPER_ENDPOINT_STATUS[["bounded"]]
anchor <- list(ppml = list(list(
  lower = c(2), upper = c(1),
  lower_status = bounded, upper_status = bounded
)))
anchor_spec <- list(estimator_ids = "ppml", coefs = "(Intercept)")
stopifnot(
  !isTRUE(logvar_boot_anchor_validate(anchor, anchor_spec, 0))
)

cat("test_bootstrap_stage: PASS\n")
