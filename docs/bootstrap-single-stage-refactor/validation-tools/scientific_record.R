source(file.path(
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
    any(!grepl("^[^/\\\\].*[.]tex$", paths)) ||
    any(grepl("(^|/)[.][.](/|$)", paths))) {
    fail("malformed table paths")
  }
  for (path in paths) {
    table <- tables[[path]]
    coordinates <- names(table)
    if (!is.list(table) || !length(table) || is.null(coordinates) ||
      any(!nzchar(coordinates)) || anyDuplicated(coordinates)) {
      fail(paste("malformed table projection:", path))
    }
    for (cell in table) {
      if (!is.data.frame(cell) ||
        !identical(names(cell), c("value", "quantum")) ||
        !is.numeric(cell$value) || !is.numeric(cell$quantum) ||
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

bootstrap_validation_record <- function(output_root) {
  record <- list(
    schema_version = 2L,
    published_tables =
      paper_published_tables_projection(output_root)
  )
  paper_validate_table_record(record)
  record
}
