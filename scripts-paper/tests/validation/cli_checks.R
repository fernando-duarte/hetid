# Subprocess checks for table-record capture and comparison commands.

cli_write_table <- function(root, body) {
  table_path <- file.path(root, "tables", "table.tex")
  dir.create(dirname(table_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    "\\begin{tabular}{lr}",
    "\\toprule",
    " & Estimate \\\\",
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}"
  ), table_path)
}

cli_table_body <- function(value, stars) {
  paste0(
    "Estimate & ", value, "$^{", stars, "}$ ", strrep("\\\\", 2L)
  )
}

cli_write_non_table_artifacts <- function(root, suffix) {
  write.csv(data.frame(value = suffix), file.path(root, "other.csv"), row.names = FALSE)
  saveRDS(list(value = suffix), file.path(root, "other.rds"))
  writeLines(paste("report", suffix), file.path(root, "other.md"))
  writeLines(paste("svg", suffix), file.path(root, "other.svg"))
  writeLines(paste("pdf", suffix), file.path(root, "other.pdf"))
  dir.create(file.path(root, "diagnostics"), showWarnings = FALSE)
  writeLines(suffix, file.path(root, "diagnostics", "details.txt"))
}

cli_status <- function(script, arguments) {
  system2(file.path(R.home("bin"), "Rscript"), args = c(script, arguments))
}

cli_output <- function(script, arguments) {
  system2(
    file.path(R.home("bin"), "Rscript"),
    args = c(script, arguments),
    stdout = TRUE,
    stderr = TRUE
  )
}

cli_reference_root <- tempfile("table-cli-reference-")
cli_candidate_root <- tempfile("table-cli-candidate-")
dir.create(cli_reference_root)
dir.create(cli_candidate_root)
cli_write_table(cli_reference_root, cli_table_body("1.23", "***"))
cli_write_table(cli_candidate_root, cli_table_body("1.23", "***"))
cli_write_non_table_artifacts(cli_reference_root, "reference")
cli_write_non_table_artifacts(cli_candidate_root, "candidate")

cli_artifact_comparator <- paper_path(
  "tests", "support", "compare_pipeline_artifacts.R"
)
status <- cli_status(
  cli_artifact_comparator,
  c(cli_reference_root, cli_candidate_root)
)
stopifnot(identical(status, 0L))

cli_write_table(cli_candidate_root, cli_table_body("1.24", "***"))
status <- cli_status(
  cli_artifact_comparator,
  c(cli_reference_root, cli_candidate_root)
)
stopifnot(identical(status, 1L))

cli_write_table(cli_candidate_root, cli_table_body("1.23", "**"))
status <- cli_status(
  cli_artifact_comparator,
  c(cli_reference_root, cli_candidate_root)
)
stopifnot(identical(status, 1L))

cli_write_table(cli_candidate_root, cli_table_body("1.23", "***"))
cli_record_path <- tempfile("table-record-", fileext = ".rds")
capture_output <- cli_output(
  paper_path("validation", "capture_table_record.R"),
  c(cli_reference_root, cli_record_path)
)
capture_status <- attr(capture_output, "status")
if (is.null(capture_status)) {
  capture_status <- 0L
}
stopifnot(
  identical(capture_status, 0L),
  !any(grepl("[1] TRUE", capture_output, fixed = TRUE)),
  file.exists(cli_record_path)
)
cli_record <- readRDS(cli_record_path)
stopifnot(
  identical(cli_record$schema_version, 3L),
  identical(
    cli_record$published_tables$table.tex[["tabular_1/row_1/column_1"]]$stars,
    "***"
  )
)

status <- cli_status(
  paper_path("validation", "compare_table_records.R"),
  c(cli_record_path, cli_record_path)
)
stopifnot(identical(status, 0L))

unlink(cli_reference_root, recursive = TRUE)
unlink(cli_candidate_root, recursive = TRUE)
unlink(cli_record_path)
rm(
  cli_write_table,
  cli_table_body,
  cli_write_non_table_artifacts,
  cli_status,
  cli_output,
  cli_reference_root,
  cli_candidate_root,
  cli_artifact_comparator,
  cli_record_path,
  cli_record,
  capture_output,
  capture_status,
  status
)
