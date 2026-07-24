args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("usage: capture_pipeline_record.R output-record.rds", call. = FALSE)
}

source(file.path(
  "scripts-paper", "tests", "support", "scientific_comparison.R"
))
source(file.path(
  "docs",
  "bootstrap-single-stage-refactor",
  "validation-tools",
  "pipeline_expression.R"
))
gate_reference_path <- Sys.getenv(
  "HETID_GATE_REFERENCE_RDS",
  unset = ""
)
gate_reference <- if (nzchar(gate_reference_path)) {
  readRDS(gate_reference_path)
} else {
  NULL
}
strict_reuse <- identical(
  Sys.getenv("HETID_VALIDATION_STRICT_REUSE", unset = "0"),
  "1"
)

bootstrap_validation_source_stage_strict <- function() {
  path <- normalizePath(
    paper_path("inference", "run_bootstrap_stage.R"),
    winslash = "/",
    mustWork = TRUE
  )
  state <- get0(
    path,
    envir = .paper_source_registry,
    inherits = FALSE
  )
  if (isTRUE(state)) return(invisible(FALSE))
  if (identical(state, FALSE)) {
    stop("circular strict-reuse stage source", call. = FALSE)
  }
  assign(path, FALSE, envir = .paper_source_registry)
  loaded <- FALSE
  on.exit({
    if (!loaded) {
      rm(list = path, envir = .paper_source_registry)
    }
  }, add = TRUE)
  for (stage_expression in parse(path)) {
    is_dispatch <- bootstrap_validation_expression_is_assignment_call(
      stage_expression,
      ".bootstrap_stage_output",
      "run_bootstrap_stage"
    )
    if (is_dispatch) {
      original_dispatch <- bootstrap_stage_cached_or_run
      bootstrap_stage_cached_or_run <- function(
        path, mode, stage_spec, provenance, run_fn, ...
      ) {
        original_dispatch(
          path, mode, stage_spec, provenance,
          function() {
            stop(
              "strict reuse refused a fallback bootstrap",
              call. = FALSE
            )
          },
          ...
        )
      }
      assign(
        "bootstrap_stage_cached_or_run",
        bootstrap_stage_cached_or_run,
        envir = .GlobalEnv
      )
      on.exit(assign(
        "bootstrap_stage_cached_or_run",
        original_dispatch,
        envir = .GlobalEnv
      ), add = TRUE)
    }
    eval(stage_expression, envir = .GlobalEnv)
    if (is_dispatch) {
      assign(
        "bootstrap_stage_cached_or_run",
        original_dispatch,
        envir = .GlobalEnv
      )
    }
  }
  assign(path, TRUE, envir = .paper_source_registry)
  loaded <- TRUE
  invisible(TRUE)
}

pipeline <- parse(file.path("scripts-paper", "run_pipeline.R"))
for (expression in pipeline) {
  is_route <- bootstrap_validation_expression_has_paper_path(
    expression,
    c("log_variance", "extensions", "egarch", "run_route.R")
  )
  if (is_route && !is.null(gate_reference)) {
    paper_source_once(paper_path("config", "decisions", "egarch.R"))
    fresh_gate <- readRDS(artifact_path("dynamics_gate"))
    reference_projection <- gate_reference
    fresh_projection <- fresh_gate
    reference_projection$sample_id <- NULL
    reference_projection$benchmark_commit <- NULL
    fresh_projection$sample_id <- NULL
    fresh_projection$benchmark_commit <- NULL
    stopifnot(isTRUE(paper_scientific_compare(
      reference_projection,
      fresh_projection
    )))
    stopifnot(
      identical(fresh_gate$verdict, "non_reject"),
      identical(logvar_egarch_decision$gate_verdict, "non_reject")
    )
    logvar_egarch_decision$sample_id <- fresh_gate$sample_id
    logvar_egarch_decision$gate_science_sha256 <-
      logvar_egarch_gate_science_sha256(fresh_gate)
  }
  is_bootstrap_stage <- bootstrap_validation_expression_has_paper_path(
    expression,
    c("inference", "run_bootstrap_stage.R")
  )
  if (is_bootstrap_stage && strict_reuse) {
    bootstrap_validation_source_stage_strict()
  } else {
    eval(expression, envir = .GlobalEnv)
  }
}

source(file.path(
  "docs",
  "bootstrap-single-stage-refactor",
  "validation-tools",
  "scientific_record.R"
))
record <- bootstrap_validation_record(
  file.path("scripts-paper", "output")
)
saveRDS(record, args[[1L]], version = 3L)
cat("scientific record:", normalizePath(args[[1L]]), "\n")
