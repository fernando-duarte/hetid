#!/usr/bin/env Rscript

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path(
  "tests", "validation", "table_projection_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "table_comparison_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "mutation_matrix_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "ssot_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "cli_checks.R"
))
cat("test_table_acceptance: PASS\n")
