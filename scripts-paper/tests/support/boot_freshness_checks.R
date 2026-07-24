# Contract checks for unified-stage runtime and source hashes.

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check
paper_source_once(paper_path("support", "statistics", "api.R"))
paper_source_once(paper_path("support", "runtime", "core.R"))

check(
  "runtime sha is stable within a session",
  identical(paper_boot_runtime_sha(), paper_boot_runtime_sha())
)
runtime_text <- paste(deparse(body(paper_boot_runtime_sha)), collapse = "\n")
check(
  "stage runtime hash excludes optional estimator packages",
  !grepl("quantreg", runtime_text, fixed = TRUE) &&
    !grepl("rugarch", runtime_text, fixed = TRUE)
)
check(
  "stage runtime hash binds the package source",
  grepl("paper_repo_code_sha", runtime_text, fixed = TRUE)
)
check("code sha changes when a hashed file's content differs", {
  a <- paper_boot_code_sha(c("support/statistics/mbb_runner.R"))
  b <- paper_boot_code_sha(c(
    "support/statistics/bootstrap_and_stationarity.R"
  ))
  is.character(a) && nchar(a) == 64L && !identical(a, b)
})

.test$finish()
