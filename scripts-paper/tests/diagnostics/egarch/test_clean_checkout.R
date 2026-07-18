#!/usr/bin/env Rscript
# Clean-checkout routing decisions must not depend on ignored docs state.

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

paper_source_once(paper_path(
  "tests",
  "diagnostics",
  "egarch",
  "clean_checkout_checks.R"
))

.test$finish()
