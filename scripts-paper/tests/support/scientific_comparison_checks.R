#!/usr/bin/env Rscript

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path(
  "tests", "support", "scientific_comparison.R"
))

reference <- list(
  estimate = matrix(
    c(0.25, 0.5),
    nrow = 1L,
    dimnames = list("draw_1", c("lower", "upper"))
  ),
  status = "bounded",
  provenance = list(
    git_commit = "reference",
    model_version = "v1"
  )
)
candidate <- reference
candidate$estimate[[1L]] <- candidate$estimate[[1L]] + 1e-5
candidate$provenance$git_commit <- "candidate"
candidate$provenance$runtime_sha <- "candidate-runtime"
candidate$provenance$elapsed_minutes <- 42
stopifnot(paper_scientific_equal(reference, candidate))

candidate$estimate[[1L]] <- reference$estimate[[1L]] + 1e-3
stopifnot(!paper_scientific_equal(reference, candidate))

candidate <- reference
candidate$status <- "failed"
stopifnot(!paper_scientific_equal(reference, candidate))

candidate <- reference
candidate$provenance$model_version <- "v2"
stopifnot(!paper_scientific_equal(reference, candidate))

cat("scientific_comparison_checks: PASS\n")
