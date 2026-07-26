# Numeric projection for final published TeX tables.

paper_source_once(paper_path("validation", "table_tokens.R"))

paper_table_numeric_projection <- function(path) {
  if (!file.exists(path)) {
    stop("published table does not exist: ", path, call. = FALSE)
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
  n_values <- sum(vapply(projection, nrow, integer(1)))
  if (n_values == 0L) {
    stop("published table has no numeric result cells: ", path, call. = FALSE)
  }
  projection
}

paper_published_tables_projection <- function(output_root) {
  table_root <- file.path(output_root, "tables")
  paths <- list.files(
    table_root,
    pattern = "[.]tex$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (!length(paths)) {
    stop("no published TeX tables found under: ", table_root, call. = FALSE)
  }
  relative <- substring(paths, nchar(table_root) + 2L)
  paths <- paths[order(relative)]
  relative <- sort(relative)
  stats::setNames(lapply(paths, paper_table_numeric_projection), relative)
}
