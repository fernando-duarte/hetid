#!/usr/bin/env Rscript

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path(
  "tests", "validation", "table_projection_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "table_record_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "table_comparison_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "ssot_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "cli_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "compatibility_wrapper_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "clean_runner_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "clean_runner_reference_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "clean_runner_safety_checks.R"
))
cat("test_table_acceptance: PASS\n")
