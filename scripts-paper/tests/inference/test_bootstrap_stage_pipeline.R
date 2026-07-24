#!/usr/bin/env Rscript

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))

pipeline_path <- paper_path("run_pipeline.R")
pipeline_text <- paste(readLines(pipeline_path, warn = FALSE), collapse = "\n")
source_offset <- function(path) {
  match <- regexpr(path, pipeline_text, fixed = TRUE)[[1L]]
  stopifnot(match > 0L)
  match
}

stage <- "inference\", \"run_bootstrap_stage.R"
structural <- "mean_equation\", \"tables\", \"render_structural_equation_table.R"
logvar <- "log_variance\", \"tables\", \"render_inference_panels.R"
combined <- "log_variance\", \"tables\", \"render_combined_inference_table.R"

stopifnot(
  !grepl("mean_equation\", \"inference\", \"run_bootstrap.R",
    pipeline_text,
    fixed = TRUE
  ),
  !grepl("log_variance\", \"inference\", \"run_set_bootstrap.R",
    pipeline_text,
    fixed = TRUE
  ),
  source_offset(stage) < source_offset(structural),
  source_offset(structural) < source_offset(logvar),
  source_offset(logvar) < source_offset(combined)
)

runner_text <- paste(
  readLines(paper_path("inference", "run_bootstrap_stage.R"), warn = FALSE),
  collapse = "\n"
)
stopifnot(
  grepl("bootstrap_stage_remove_legacy_caches", runner_text, fixed = TRUE),
  grepl("artifact_path(\"bootstrap_stage_draws\")", runner_text, fixed = TRUE)
)

bootstrap_rows <- artifact_manifest[
  artifact_manifest$group == "state" &
    grepl("bootstrap", artifact_manifest$id, fixed = TRUE), ,
  drop = FALSE
]
stopifnot(
  identical(bootstrap_rows$id, "bootstrap_stage_draws"),
  identical(bootstrap_rows$basename, "bootstrap_stage_draws.rds"),
  identical(
    bootstrap_rows$producer,
    "inference/run_bootstrap_stage.R"
  ),
  identical(
    strsplit(bootstrap_rows$consumer, ";", fixed = TRUE)[[1L]],
    c(
      "mean_equation/tables/render_structural_equation_table.R",
      "log_variance/tables/render_inference_panels.R",
      "log_variance/tables/render_combined_inference_table.R"
    )
  )
)

reporting_consumers <- c(
  "mean_equation/variance_shares/compute_variance_shares.R",
  "mean_equation/inference/compute_bounds_by_tau.R"
)
stopifnot(all(vapply(reporting_consumers, function(path) {
  text <- paste(readLines(paper_path(path), warn = FALSE), collapse = "\n")
  grepl(
    "support\", \"reporting\", \"inference.R",
    text,
    fixed = TRUE
  )
}, logical(1))))

production <- list.files(
  paper_path(),
  pattern = "[.]R$",
  recursive = TRUE,
  full.names = TRUE
)
production <- production[!grepl("/tests/", production, fixed = TRUE)]
production_text <- paste(
  unlist(lapply(production, readLines, warn = FALSE), use.names = FALSE),
  collapse = "\n"
)
stopifnot(
  !file.exists(file.path(
    paper_path(), "mean_equation", "inference", "run_bootstrap.R"
  )),
  !file.exists(file.path(
    paper_path(), "log_variance", "inference", "run_set_bootstrap.R"
  )),
  !file.exists(file.path(
    paper_path(), "log_variance", "inference", "set_bootstrap_reuse.R"
  )),
  !grepl("paper_boot_index_sha", production_text, fixed = TRUE)
)

cat("test_bootstrap_stage_pipeline: PASS\n")
