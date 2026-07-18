#!/usr/bin/env Rscript
# Compare a pre-migration flat output root with a post-migration typed output root.
# Values are exact. Rendered figures (PDF/SVG) are verified separately; this check only
# requires each figure file to exist and be nonempty.

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "Usage: compare_pipeline_artifacts.R <old-output-root> <new-output-root>",
    call. = FALSE
  )
}
old_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
new_root <- normalizePath(args[[2L]], winslash = "/", mustWork = TRUE)

relative_to_output <- function(path) {
  prefix <- paste0(gsub("([][{}()+*^$|\\?.])", "\\\\\\1", out_dir), "/")
  sub(paste0("^", prefix), "", path)
}

old_rel <- vapply(artifact_manifest$old_path, relative_to_output, character(1))
new_rel <- vapply(artifact_manifest$new_path, relative_to_output, character(1))

path_tokens <- unlist(lapply(seq_len(nrow(artifact_manifest)), function(i) {
  variants <- unique(c(
    artifact_manifest$old_path[[i]], artifact_manifest$new_path[[i]],
    old_rel[[i]], new_rel[[i]], file.path("..", new_rel[[i]]),
    artifact_manifest$basename[[i]]
  ))
  stats::setNames(rep(
    sprintf("<ARTIFACT:%s>", artifact_manifest$id[[i]]),
    length(variants)
  ), variants)
}), use.names = TRUE)
path_tokens <- path_tokens[order(nchar(names(path_tokens)), decreasing = TRUE)]

normalize_paths <- function(x) {
  for (path in names(path_tokens)) {
    x <- gsub(path, path_tokens[[path]], x, fixed = TRUE)
  }
  x
}

volatile_timestamp_fields <- c(
  "built_at", "built_at_utc", "created_at", "created_at_utc",
  "generated_at", "generated_at_utc", "timestamp", "timestamp_utc"
)
intentional_path_fields <- c("path", "gate_record_path")

normalize_object <- function(x, field = "") {
  if (field %in% volatile_timestamp_fields) {
    return("<VOLATILE_TIMESTAMP>")
  }
  if (is.character(x) && field %in% intentional_path_fields) {
    return(normalize_paths(x))
  }
  if (is.list(x)) {
    out <- x
    fields <- names(x)
    if (is.null(fields)) fields <- rep("", length(x))
    for (i in seq_along(x)) out[[i]] <- normalize_object(x[[i]], fields[[i]])
    return(out)
  }
  x
}

normalize_text <- function(lines, artifact_id) {
  if (identical(artifact_id, "descriptive_report_tex")) {
    reference_line <- grepl("\\\\(input|includegraphics)", lines)
    lines[reference_line] <- normalize_paths(lines[reference_line])
  }
  gsub(
    "[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?Z?",
    "<VOLATILE_TIMESTAMP>", lines
  )
}

listed_files <- function(root) {
  files <- list.files(root, recursive = TRUE, full.names = FALSE, all.files = TRUE)
  files <- files[file.info(file.path(root, files))$isdir %in% FALSE]
  files[basename(files) != ".DS_Store"]
}

problems <- character()
record_problem <- function(...) problems <<- c(problems, sprintf(...))

old_files <- listed_files(old_root)
old_transient <- paste0(
  "(^|/)([.]DS_Store|set_id_region[.]json)$|",
  "[.](aux|log|fls|fdb_latexmk|synctex[.]gz|out|toc|nav|snm|vrb)$"
)
old_files <- old_files[!grepl(old_transient, old_files, ignore.case = TRUE)]
unexpected_old <- setdiff(old_files, old_rel)
unexpected_new <- setdiff(listed_files(new_root), new_rel)
if (length(unexpected_old)) {
  record_problem("Unmanifested old artifacts: %s", paste(unexpected_old, collapse = ", "))
}
if (length(unexpected_new)) {
  record_problem("Unmanifested new artifacts: %s", paste(unexpected_new, collapse = ", "))
}

compare_value <- function(old_path, new_path, extension, artifact_id) {
  if (extension %in% c("pdf", "svg")) {
    sizes <- file.info(c(old_path, new_path))$size
    if (any(is.na(sizes)) || any(sizes <= 0)) {
      return("PDF is empty")
    }
    return(NA_character_)
  }
  old_value <- tryCatch(
    switch(extension,
      csv = normalize_object(utils::read.csv(old_path, stringsAsFactors = FALSE)),
      rds = normalize_object(readRDS(old_path)),
      tex = normalize_text(readLines(old_path, warn = FALSE), artifact_id),
      stop(sprintf("Unsupported manifested extension: %s", extension))
    ),
    error = function(e) e
  )
  new_value <- tryCatch(
    switch(extension,
      csv = normalize_object(utils::read.csv(new_path, stringsAsFactors = FALSE)),
      rds = normalize_object(readRDS(new_path)),
      tex = normalize_text(readLines(new_path, warn = FALSE), artifact_id),
      stop(sprintf("Unsupported manifested extension: %s", extension))
    ),
    error = function(e) e
  )
  if (inherits(old_value, "error")) {
    return(paste("old parse error:", conditionMessage(old_value)))
  }
  if (inherits(new_value, "error")) {
    return(paste("new parse error:", conditionMessage(new_value)))
  }
  if (!identical(old_value, new_value)) "schema, string, or numeric difference" else NA_character_
}

for (i in seq_len(nrow(artifact_manifest))) {
  old_path <- file.path(old_root, old_rel[[i]])
  new_path <- file.path(new_root, new_rel[[i]])
  old_exists <- file.exists(old_path)
  new_exists <- file.exists(new_path)
  status <- artifact_manifest$status[[i]]
  id <- artifact_manifest$id[[i]]
  if (identical(status, "required") && (!old_exists || !new_exists)) {
    record_problem("Required artifact %s presence old=%s new=%s", id, old_exists, new_exists)
    next
  }
  if (!identical(old_exists, new_exists)) {
    record_problem("Conditional artifact %s presence old=%s new=%s", id, old_exists, new_exists)
    next
  }
  if (!old_exists) next
  extension <- tolower(tools::file_ext(artifact_manifest$basename[[i]]))
  difference <- compare_value(old_path, new_path, extension, id)
  if (!is.na(difference)) record_problem("Artifact %s: %s", id, difference)
}

if (length(problems)) {
  cat("Artifact comparison failed:\n", paste0("- ", problems, collapse = "\n"), "\n")
  quit(status = 1L)
}
cat(sprintf(
  "Artifact comparison passed for %d manifest records; PDFs require visual checks.\n",
  nrow(artifact_manifest)
))
