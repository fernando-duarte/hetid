# Schema-3 records for published-table numeric projections.

paper_source_once(paper_path("validation", "table_projection.R"))

paper_invalid_table_record <- function(message) {
  stop("invalid published-table record: ", message, call. = FALSE)
}

paper_table_path_is_safe <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(FALSE)
  }
  if (!grepl("^[^/\\\\]+(?:/[^/\\\\]+)*[.]tex$", path, perl = TRUE) ||
    grepl(":", path, fixed = TRUE)) {
    return(FALSE)
  }
  !any(strsplit(path, "/", fixed = TRUE)[[1L]] %in% c(".", ".."))
}

paper_table_coordinate_is_valid <- function(coordinate) {
  is.character(coordinate) && length(coordinate) == 1L && !is.na(coordinate) &&
    grepl(
      "^tabular_[1-9][0-9]*/row_[1-9][0-9]*/column_[1-9][0-9]*$",
      coordinate
    )
}

paper_validate_table_cell <- function(cell, path, coordinate) {
  location <- paste0(path, "/", coordinate)
  if (!is.data.frame(cell)) {
    paper_invalid_table_record(paste("cell is not a data frame:", location))
  }
  expected_columns <- c("value", "quantum", "stars")
  if (!identical(names(cell), expected_columns)) {
    paper_invalid_table_record(paste("cell columns are invalid:", location))
  }
  if (!all(vapply(cell, is.atomic, logical(1)))) {
    paper_invalid_table_record(paste("cell columns must be atomic:", location))
  }
  cell_rows <- nrow(cell)
  if (any(vapply(cell, function(column) !is.null(dim(column)), logical(1))) ||
    any(vapply(cell, length, integer(1)) != cell_rows)) {
    paper_invalid_table_record(paste("cell columns must be vectors:", location))
  }
  if (!is.numeric(cell$value) || !is.numeric(cell$quantum) ||
    !is.character(cell$stars)) {
    paper_invalid_table_record(paste("cell column types are invalid:", location))
  }
  if (any(!is.finite(cell$value)) || any(!is.finite(cell$quantum))) {
    paper_invalid_table_record(paste("cell values must be finite:", location))
  }
  if (any(cell$quantum <= 0)) {
    paper_invalid_table_record(paste("cell quanta must be positive:", location))
  }
  if (any(!cell$stars %in% c("", "*", "**", "***"))) {
    paper_invalid_table_record(paste("cell stars are invalid:", location))
  }
}

paper_validate_table_record <- function(record) {
  if (!is.list(record) ||
    !identical(names(record), c("schema_version", "published_tables"))) {
    paper_invalid_table_record("top-level fields must be schema_version and published_tables")
  }
  if (!identical(record$schema_version, 3L)) {
    paper_invalid_table_record("schema_version must be 3L")
  }
  tables <- record$published_tables
  if (!is.list(tables) || !length(tables) || is.null(names(tables)) ||
    anyDuplicated(names(tables)) || any(!vapply(
    names(tables), paper_table_path_is_safe, logical(1)
  ))) {
    paper_invalid_table_record("published table paths must be unique safe .tex paths")
  }
  for (path in names(tables)) {
    table <- tables[[path]]
    if (!is.list(table) || !length(table) || is.null(names(table)) ||
      anyDuplicated(names(table)) || any(!vapply(
      names(table), paper_table_coordinate_is_valid, logical(1)
    ))) {
      paper_invalid_table_record(paste("coordinates are invalid for:", path))
    }
    for (coordinate in names(table)) {
      paper_validate_table_cell(table[[coordinate]], path, coordinate)
    }
    if (!any(vapply(table, nrow, integer(1)) > 0L)) {
      paper_invalid_table_record(paste("table has no numeric tokens:", path))
    }
  }
  TRUE
}

paper_table_record <- function(output_root) {
  record <- list(
    schema_version = 3L,
    published_tables = paper_published_tables_projection(output_root)
  )
  paper_validate_table_record(record)
  record
}
