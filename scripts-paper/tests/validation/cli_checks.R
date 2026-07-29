# Subprocess checks for direct output-table comparison.

source(file.path("scripts-paper", "config", "paths.R"))

cli_write_table <- function(root, body, relative = "table.tex") {
  table_path <- file.path(root, "tables", relative)
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
  paste0("Estimate & ", value, "$^{", stars, "}$ \\\\")
}

cli_write_non_table_artifacts <- function(root, suffix) {
  writeLines(suffix, file.path(root, "other.csv"))
  writeLines(suffix, file.path(root, "other.rds"))
  writeLines(suffix, file.path(root, "other.md"))
  writeLines(suffix, file.path(root, "other.svg"))
  writeLines(suffix, file.path(root, "other.pdf"))
  dir.create(file.path(root, "diagnostics"), showWarnings = FALSE)
  writeLines(suffix, file.path(root, "diagnostics", "details.txt"))
}

cli_output <- function(arguments) {
  system2(
    file.path(R.home("bin"), "Rscript"),
    args = c(
      "--vanilla",
      paper_path("validation", "compare_output_tables.R"),
      arguments
    ),
    stdout = TRUE,
    stderr = TRUE
  )
}

cli_status <- function(output) {
  status <- attr(output, "status")
  if (is.null(status)) 0L else as.integer(status)
}

cli_reference_root <- tempfile("table-cli-reference-")
cli_candidate_root <- tempfile("table-cli-candidate-")
dir.create(cli_reference_root)
dir.create(cli_candidate_root)
cli_write_table(
  cli_reference_root,
  cli_table_body("1.23", "***")
)
cli_write_table(
  cli_candidate_root,
  cli_table_body("1.23", "***")
)
cli_write_table(
  cli_candidate_root,
  "Status & not estimated \\\\",
  "nonnumeric-only.tex"
)
cli_write_non_table_artifacts(cli_reference_root, "reference")
cli_write_non_table_artifacts(cli_candidate_root, "candidate")

output <- cli_output(c(cli_reference_root, cli_candidate_root))
stopifnot(
  identical(cli_status(output), 0L),
  any(grepl(
    "Published table-result comparison passed.",
    output,
    fixed = TRUE
  ))
)

cli_write_table(cli_candidate_root, cli_table_body("1.24", "***"))
output <- cli_output(c(cli_reference_root, cli_candidate_root))
stopifnot(
  identical(cli_status(output), 1L),
  any(grepl("displayed values differ", output, fixed = TRUE))
)

cli_write_table(cli_candidate_root, cli_table_body("1.23", "**"))
output <- cli_output(c(cli_reference_root, cli_candidate_root))
stopifnot(
  identical(cli_status(output), 1L),
  any(grepl("stars differ", output, fixed = TRUE))
)

empty_reference <- tempfile("table-cli-empty-reference-")
empty_candidate <- tempfile("table-cli-empty-candidate-")
dir.create(file.path(empty_reference, "tables"), recursive = TRUE)
dir.create(file.path(empty_candidate, "tables"), recursive = TRUE)
output <- cli_output(c(empty_reference, empty_candidate))
stopifnot(identical(cli_status(output), 0L))

output <- cli_output(cli_reference_root)
stopifnot(
  identical(cli_status(output), 1L),
  any(grepl("Usage: compare_output_tables.R", output, fixed = TRUE))
)

unlink(cli_reference_root, recursive = TRUE)
unlink(cli_candidate_root, recursive = TRUE)
unlink(empty_reference, recursive = TRUE)
unlink(empty_candidate, recursive = TRUE)
rm(
  cli_write_table,
  cli_table_body,
  cli_write_non_table_artifacts,
  cli_output,
  cli_status,
  cli_reference_root,
  cli_candidate_root,
  empty_reference,
  empty_candidate,
  output
)
