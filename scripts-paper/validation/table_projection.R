# Numeric projection for final published TeX tables.

paper_source_once(paper_path("validation", "table_tokens.R"))

paper_require_readable_directory <- function(path, label) {
  if (!dir.exists(path)) {
    stop(label, " does not exist: ", path, call. = FALSE)
  }
  if (file.access(path, mode = 5L) != 0L) {
    stop(label, " is not readable: ", path, call. = FALSE)
  }
  invisible(path)
}

paper_table_numeric_projection <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    stop("published table does not exist: ", path, call. = FALSE)
  }
  if (file.access(path, mode = 4L) != 0L) {
    stop("published table is not readable: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  projection <- list()
  in_tabular <- FALSE
  data_started <- FALSE
  tabular_id <- 0L
  row_id <- 0L
  for (line in lines) {
    if (grepl("\\begin{tabular}", line, fixed = TRUE)) {
      in_tabular <- TRUE
      data_started <- FALSE
      tabular_id <- tabular_id + 1L
      row_id <- 0L
      next
    }
    if (grepl("\\end{tabular}", line, fixed = TRUE)) {
      in_tabular <- FALSE
      next
    }
    if (!in_tabular) {
      next
    }
    if (grepl("\\midrule", line, fixed = TRUE)) {
      data_started <- TRUE
      next
    }
    if (!data_started || !grepl("&", line, fixed = TRUE)) {
      next
    }
    row_id <- row_id + 1L
    cells <- strsplit(line, "&", fixed = TRUE)[[1L]]
    if (length(cells) < 2L) {
      next
    }
    for (column_id in seq_along(cells[-1L])) {
      key <- sprintf(
        "tabular_%d/row_%d/column_%d",
        tabular_id,
        row_id,
        column_id
      )
      projection[[key]] <- paper_table_cell_results(cells[[column_id + 1L]])
    }
  }
  has_values <- vapply(projection, nrow, integer(1)) > 0L
  if (any(has_values)) projection[has_values] else list()
}

paper_published_tables_projection <- function(output_root) {
  paper_require_readable_directory(output_root, "output root")
  table_root <- file.path(output_root, "tables")
  paper_require_readable_directory(table_root, "tables directory")
  paths <- list.files(
    table_root,
    pattern = "[.]tex$",
    recursive = TRUE,
    full.names = TRUE
  )
  relative <- substring(paths, nchar(table_root) + 2L)
  ordering <- order(relative)
  projections <- stats::setNames(
    lapply(paths[ordering], paper_table_numeric_projection),
    relative[ordering]
  )
  has_values <- vapply(projections, length, integer(1)) > 0L
  projections[has_values]
}
