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
paper_record_root <- normalizePath(file.path(
  dirname(paper_record_source),
  "..",
  "..",
  ".."
))
source(file.path(
  paper_record_root,
  "scripts-paper",
  "tests",
  "support",
  "published_table_comparison.R"
))

paper_validate_table_record <- function(record) {
  fail <- function(message) {
    stop("invalid published-table record: ", message, call. = FALSE)
  }
  if (!is.list(record) ||
    !identical(names(record), c("schema_version", "published_tables")) ||
    !identical(record$schema_version, 2L)) {
    fail("unsupported schema")
  }
  tables <- record$published_tables
  paths <- names(tables)
  if (!is.list(tables) || !length(tables) || is.null(paths) ||
    any(!nzchar(paths)) || anyDuplicated(paths) ||
    any(!grepl("^([[:alnum:]_.-]+/)*[[:alnum:]_.-]+[.]tex$", paths)) ||
    any(grepl("(^|/)[.]{1,2}(/|$)", paths))) {
    fail("malformed table paths")
  }
  for (path in paths) {
    table <- tables[[path]]
    coordinates <- names(table)
    if (!is.list(table) || !length(table) || is.null(coordinates) ||
      any(!nzchar(coordinates)) || anyDuplicated(coordinates) ||
      any(!grepl(
        "^tabular_[1-9][0-9]*/row_[1-9][0-9]*/column_[1-9][0-9]*$",
        coordinates
      ))) {
      fail(paste("malformed table projection:", path))
    }
    for (cell in table) {
      if (!is.data.frame(cell) ||
        !identical(names(cell), c("value", "quantum")) ||
        !is.numeric(cell$value) || !is.numeric(cell$quantum) ||
        !is.null(dim(cell$value)) || !is.null(dim(cell$quantum)) ||
        length(cell$value) != nrow(cell) ||
        length(cell$quantum) != nrow(cell) ||
        any(!is.finite(cell$value)) ||
        any(!is.finite(cell$quantum) | cell$quantum <= 0)) {
        fail(paste("malformed numeric cell:", path))
      }
    }
    if (!sum(vapply(table, nrow, integer(1)))) {
      fail(paste("table has no numeric cells:", path))
    }
  }
  TRUE
}

rm(paper_record_sources, paper_record_source, paper_record_root)

bootstrap_validation_record <- function(output_root) {
  record <- list(
    schema_version = 2L,
    published_tables =
      paper_published_tables_projection(output_root)
  )
  paper_validate_table_record(record)
  record
}
