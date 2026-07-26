paper_record_sources <- vapply(
  sys.frames(),
  function(frame) if (is.null(frame$ofile)) "" else frame$ofile,
  character(1)
)
paper_record_source <- tail(
  paper_record_sources[
    basename(paper_record_sources) == "scientific_record.R"
  ],
  1L
)
if (!length(paper_record_source)) {
  paper_record_source <- file.path(
    "docs",
    "bootstrap-single-stage-refactor",
    "validation-tools",
    "scientific_record.R"
  )
}
paper_record_source <- normalizePath(paper_record_source, mustWork = TRUE)
repo_root <- normalizePath(file.path(
  dirname(paper_record_source),
  "..",
  "..",
  ".."
))
paper_record_cwd <- setwd(repo_root)
tryCatch(
  source(file.path(
    repo_root,
    "scripts-paper",
    "config",
    "paths.R"
  )),
  finally = setwd(paper_record_cwd)
)
paper_source_once(paper_path(
  "validation",
  "table_comparison.R"
))

bootstrap_validation_record <- paper_table_record

rm(paper_record_sources, paper_record_source, paper_record_cwd)
