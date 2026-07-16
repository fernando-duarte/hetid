#!/usr/bin/env Rscript
# Structural checks for the semantic scripts-paper tree. Run from the package root.

source(file.path("scripts-paper", "config", "paths.R"))
source(paper_path("config", "artifacts.R"))
paper_root <- paper_path()
r_files <- list.files(paper_root, pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
problems <- character()
record_problem <- function(...) problems <<- c(problems, sprintf(...))
relative_paper <- function(path) substring(path, nchar(paper_root) + 2L)
# Every R source parses and respects the repository file and line limits.
for (file in r_files) {
  rel <- relative_paper(file)
  parsed <- tryCatch(parse(file), error = identity)
  if (inherits(parsed, "error")) {
    record_problem("Parse failure %s: %s", rel, conditionMessage(parsed))
  }
  lines <- readLines(file, warn = FALSE)
  if (length(lines) >= 200L) record_problem("File has %d lines: %s", length(lines), rel)
  long <- which(nchar(lines, type = "width") >= 100L)
  if (length(long)) {
    record_problem("Lines reach 100 columns in %s: %s", rel, paste(long, collapse = ","))
  }
}
literal_path <- function(expr) {
  if (is.character(expr) && length(expr) == 1L) {
    if (grepl("^/", expr)) {
      return(expr)
    }
    return(file.path(repo_root, expr))
  }
  if (!is.call(expr)) {
    return(NA_character_)
  }
  name <- as.character(expr[[1L]])
  if (name == "normalizePath") {
    return(literal_path(expr[[2L]]))
  }
  if (!name %in% c("paper_path", "repo_path", "file.path")) {
    return(NA_character_)
  }
  parts <- lapply(as.list(expr[-1L]), function(x) {
    if (is.character(x) && length(x) == 1L) x else NA_character_
  })
  if (anyNA(parts)) {
    return(NA_character_)
  }
  path <- do.call(file.path, parts)
  if (name == "paper_path") {
    return(do.call(paper_path, parts))
  }
  if (name == "repo_path") {
    return(do.call(repo_path, parts))
  }
  if (grepl("^/", path)) path else file.path(repo_root, path)
}
source_targets <- function(file) {
  targets <- character()
  walk <- function(node) {
    if (is.call(node) && identical(as.character(node[[1L]]), "source")) {
      target <- literal_path(node[[2L]])
      if (!is.na(target)) targets <<- c(targets, target)
    }
    if (is.call(node) || is.expression(node) || is.pairlist(node)) {
      children <- as.list(node)
      for (i in seq_along(children)) {
        if (identical(children[[i]], quote(expr = ))) next
        walk(children[[i]])
      }
    }
  }
  walk(parse(file))
  unique(targets)
}
# Literal sources must exist and stay inside the paper tree.
source_map <- stats::setNames(lapply(r_files, source_targets), r_files)
paper_prefix <- paste0(paper_root, "/")
record_paths <- function(label, paths) {
  if (!length(paths)) {
    return()
  }
  record_problem("%s: %s", label, paste(paths, collapse = ", "))
}
for (file in names(source_map)) {
  missing <- source_map[[file]][!file.exists(source_map[[file]])]
  external <- source_map[[file]][!startsWith(source_map[[file]], paper_prefix)]
  record_paths(sprintf("Missing source targets from %s", relative_paper(file)), missing)
  record_paths(sprintf("External source targets from %s", relative_paper(file)), external)
}
# Removed entrypoints, flat paper-source paths, and old test paths must stay gone.
text_files <- list.files(
  paper_root,
  pattern = "[.](R|md)$", recursive = TRUE, full.names = TRUE
)
removed_patterns <- c(
  "run_all[.]R", "scripts/utils/tests/(test_logvar|logvar)",
  "scripts-paper/(?!run_pipeline[.]R)[a-z0-9_]+[.]R"
)
for (file in text_files) {
  text <- readLines(file, warn = FALSE)
  for (pattern in removed_patterns) {
    if (any(grepl(pattern, text, perl = TRUE))) {
      record_problem("Removed path reference in %s: %s", relative_paper(file), pattern)
    }
  }
}
# Source filenames named in comments and documentation must still exist somewhere
# in the repository. This catches stale bare basenames after semantic moves.
known_r_basenames <- basename(list.files(
  repo_root,
  pattern = "[.]R$", recursive = TRUE, full.names = TRUE
))
for (file in text_files) {
  text <- readLines(file, warn = FALSE)
  if (grepl("[.]R$", file)) text <- text[grepl("^[[:space:]]*#", text)]
  matches <- regmatches(text, gregexpr("[A-Za-z0-9_./*-]+[.]R\\b", text))
  references <- unique(unlist(matches, use.names = FALSE))
  missing <- references[!basename(references) %in% known_r_basenames]
  if (length(missing)) {
    record_problem(
      "Stale source basename in %s: %s", relative_paper(file),
      paste(missing, collapse = ", ")
    )
  }
}
# Every active production module is reachable from the sole entrypoint.
entrypoint <- paper_path("run_pipeline.R")
reachable <- entrypoint
frontier <- entrypoint
while (length(frontier)) {
  children <- unique(unlist(source_map[frontier], use.names = FALSE))
  children <- children[startsWith(children, paste0(paper_root, "/"))]
  children <- intersect(children, r_files)
  frontier <- setdiff(children, reachable)
  reachable <- unique(c(reachable, frontier))
}
inactive <- paper_path(
  "log_variance", "tables", "legacy_log_ols_caption.R"
)
inactive <- c(
  inactive,
  paper_path("log_variance", "estimators", "lad", "offline_refinement.R"),
  paper_path("log_variance", "figures", "bounds_by_tau_test_support.R")
)
production <- r_files[
  !grepl("/tests/", r_files) & !r_files %in% inactive
]
unreachable <- setdiff(production, reachable)
if (length(unreachable)) {
  record_problem(
    "Unreachable production modules: %s",
    paste(vapply(unreachable, relative_paper, character(1)), collapse = ", ")
  )
}

# The manifest is complete, typed, and internally consistent.
required_columns <- c(
  "id", "basename", "old_path", "new_path", "producer", "consumer", "status"
)
if (!identical(names(artifact_manifest), required_columns)) {
  record_problem("Artifact manifest columns do not match the required schema")
}
if (anyDuplicated(artifact_manifest$id) || anyDuplicated(artifact_manifest$basename)) {
  record_problem("Artifact IDs and basenames must be unique")
}
if (!all(basename(artifact_manifest$old_path) == artifact_manifest$basename) ||
  !all(basename(artifact_manifest$new_path) == artifact_manifest$basename)) {
  record_problem("Artifact path basenames disagree with the manifest")
}
if (!all(dirname(artifact_manifest$old_path) == out_dir)) {
  record_problem("Old artifact paths must describe the flat output layout")
}
typed <- c("tables", "figures", "reports", "diagnostics", "state")
new_group <- sub(paste0("^", out_dir, "/([^/]+).*$"), "\\1", artifact_manifest$new_path)
if (!all(new_group %in% typed)) record_problem("New artifact paths must use typed groups")

# Existing output may contain only manifested typed artifacts, never sidecars or root files.
if (dir.exists(out_dir)) {
  output_files <- list.files(out_dir, recursive = TRUE, full.names = FALSE, all.files = TRUE)
  output_files <- output_files[file.info(file.path(out_dir, output_files))$isdir %in% FALSE]
  output_files <- output_files[basename(output_files) != ".DS_Store"]
  sidecar <- "[.](aux|log|fls|fdb_latexmk|synctex[.]gz|out|toc)$"
  if (any(grepl(sidecar, output_files, ignore.case = TRUE))) {
    record_problem("LaTeX sidecars found under output")
  }
  if (any(dirname(output_files) == ".")) record_problem("Root-level output files found")
  expected <- sub(paste0("^", out_dir, "/"), "", artifact_manifest$new_path)
  extra <- setdiff(output_files, expected)
  if (length(extra)) record_problem("Unmanifested output files: %s", paste(extra, collapse = ", "))
}

if (length(problems)) {
  cat("Topology checks failed:\n", paste0("- ", problems, collapse = "\n"), "\n")
  quit(status = 1L)
}
cat(sprintf(
  "Topology checks passed for %d R files and %d artifacts.\n",
  length(r_files), nrow(artifact_manifest)
))
