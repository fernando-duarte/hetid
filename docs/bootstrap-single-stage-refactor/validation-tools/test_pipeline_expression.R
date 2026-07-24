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

stale_decision <- list(
  gate_science_sha256 = "stale",
  sample_id = "stale",
  gate_lag = 1L,
  gate_alpha = 0.01,
  gate_q = 1,
  gate_p = 0.5,
  gate_verdict = "non_reject",
  decisions = list(egarch = "run")
)
fresh_gate <- list(
  sample_id = "fresh",
  gate_lag = 4L,
  gate_alpha = 0.05,
  q_stats = c(lag1 = 1, lag4 = 2 + 1e-12),
  p_values = c(lag1 = 0.5, lag4 = 0.25 - 1e-12),
  verdict = "non_reject"
)
rebound_decision <- bootstrap_validation_rebind_gate_decision(
  stale_decision,
  fresh_gate,
  "fresh-hash"
)

check(
  "runtime gate decision is fully rebound to the accepted fresh gate",
  identical(
    rebound_decision,
    list(
      gate_science_sha256 = "fresh-hash",
      sample_id = "fresh",
      gate_lag = 4L,
      gate_alpha = 0.05,
      gate_q = unname(fresh_gate$q_stats[["lag4"]]),
      gate_p = unname(fresh_gate$p_values[["lag4"]]),
      gate_verdict = "non_reject",
      decisions = list(egarch = "run")
    )
  )
)
