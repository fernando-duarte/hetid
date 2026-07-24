#!/usr/bin/env Rscript

tool_dir <- dirname(normalizePath(
  sub("^--file=", "", grep(
    "^--file=",
    commandArgs(trailingOnly = FALSE),
    value = TRUE
  )),
  mustWork = TRUE
))
helper_path <- file.path(tool_dir, "pipeline_expression.R")
if (file.exists(helper_path)) {
  source(helper_path)
} else {
  bootstrap_validation_expression_has_paper_path <- function(...) FALSE
}
if (!exists(
  "bootstrap_validation_expression_is_assignment_call",
  mode = "function"
)) {
  bootstrap_validation_expression_is_assignment_call <- function(...) FALSE
}

check <- function(label, value) {
  if (!isTRUE(value)) {
    cat("FAIL:", label, "\n")
    quit(status = 1L)
  }
  cat("PASS:", label, "\n")
}

route_expression <- quote(paper_source_once(paper_path(
  "log_variance",
  "extensions",
  "egarch",
  "run_route.R"
)))
stage_expression <- quote(.stage_output <- paper_source_once(paper_path(
  "inference",
  "run_bootstrap_stage.R"
)))
decoy_expression <- quote(message(
  "extensions",
  "egarch",
  "run_route.R"
))
dispatch_expression <- quote(
  .bootstrap_stage_output <- run_bootstrap_stage(
    stage_spec,
    provenance
  )
)

check(
  "wrapped EGARCH route source is detected structurally",
  bootstrap_validation_expression_has_paper_path(
    route_expression,
    c("log_variance", "extensions", "egarch", "run_route.R")
  )
)
check(
  "wrapped bootstrap stage source is detected structurally",
  bootstrap_validation_expression_has_paper_path(
    stage_expression,
    c("inference", "run_bootstrap_stage.R")
  )
)
check(
  "unrelated string literals are not treated as a paper path",
  !bootstrap_validation_expression_has_paper_path(
    decoy_expression,
    c("extensions", "egarch", "run_route.R")
  )
)
check(
  "strict-reuse dispatch assignment is detected structurally",
  bootstrap_validation_expression_is_assignment_call(
    dispatch_expression,
    ".bootstrap_stage_output",
    "run_bootstrap_stage"
  )
)
