#!/usr/bin/env Rscript
# Structural checks for the semantic scripts-paper tree.

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))

paper_root <- paper_path()
r_files <- list.files(
  paper_root,
  pattern = "[.]R$",
  recursive = TRUE,
  full.names = TRUE
)
problems <- character()
record_problem <- function(...) {
  problems <<- c(problems, sprintf(...))
}
relative_paper <- function(path) {
  substring(path, nchar(paper_root) + 2L)
}

for (file in r_files) {
  relative <- relative_paper(file)
  parsed <- tryCatch(parse(file), error = identity)
  if (inherits(parsed, "error")) {
    record_problem(
      "Parse failure %s: %s",
      relative,
      conditionMessage(parsed)
    )
  }
  lines <- readLines(file, warn = FALSE)
  if (length(lines) >= 200L) {
    record_problem(
      "File has %d lines: %s",
      length(lines),
      relative
    )
  }
  long <- which(nchar(lines, type = "width") >= 100L)
  if (length(long)) {
    record_problem(
      "Lines reach 100 columns in %s: %s",
      relative,
      paste(long, collapse = ",")
    )
  }
}

paper_source_once(paper_path(
  "tests", "support", "topology_source_checks.R"
))
paper_source_once(paper_path(
  "tests", "support", "topology_artifact_checks.R"
))

if (length(problems)) {
  cat(
    "Topology checks failed:\n",
    paste0("- ", problems, collapse = "\n"),
    "\n"
  )
  quit(status = 1L)
}
cat(sprintf(
  "Topology checks passed for %d R files and %d artifacts.\n",
  length(r_files),
  nrow(artifact_manifest)
))
