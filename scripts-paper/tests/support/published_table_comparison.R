# Numeric projection and comparison for final published TeX tables.

source(file.path(
  "scripts-paper",
  "tests",
  "support",
  "published_table_tokens.R"
))

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
      projection[[key]] <- paper_table_cell_numbers(
        cells[[column_id + 1L]]
      )
    }
  }
  n_values <- sum(vapply(
    projection,
    nrow,
    integer(1)
  ))
  if (n_values == 0L) {
    stop(
      "published table has no numeric result cells: ",
      path,
      call. = FALSE
    )
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
  stats::setNames(
    lapply(paths, paper_table_numeric_projection),
    relative
  )
}

paper_table_tokens_equal <- function(reference, candidate) {
  stopifnot(nrow(reference) == nrow(candidate))
  difference <- abs(reference$value - candidate$value)
  rounding_overlap <- (
    reference$quantum + candidate$quantum
  ) / 2
  scale <- pmax(abs(reference$value), abs(candidate$value), rounding_overlap)
  boundary_slack <- 8 * .Machine$double.eps * scale
  difference == 0 | difference < rounding_overlap - boundary_slack
}

paper_published_tables_compare <- function(reference, candidate) {
  reference_paths <- sort(names(reference))
  candidate_paths <- sort(names(candidate))
  if (!identical(reference_paths, candidate_paths)) {
    missing <- setdiff(reference_paths, candidate_paths)
    extra <- setdiff(candidate_paths, reference_paths)
    return(c(
      if (length(missing)) {
        paste("missing candidate tables:", paste(missing, collapse = ", "))
      },
      if (length(extra)) {
        paste("extra candidate tables:", paste(extra, collapse = ", "))
      }
    ))
  }
  problems <- character()
  for (path in reference_paths) {
    reference_table <- reference[[path]]
    candidate_table <- candidate[[path]]
    coordinates <- intersect(
      names(reference_table),
      names(candidate_table)
    )
    n_compared <- 0L
    for (coordinate in coordinates) {
      reference_cell <- reference_table[[coordinate]]
      candidate_cell <- candidate_table[[coordinate]]
      if (!nrow(reference_cell) ||
        nrow(reference_cell) != nrow(candidate_cell)) {
        next
      }
      equal <- paper_table_tokens_equal(
        reference_cell,
        candidate_cell
      )
      n_compared <- n_compared + length(equal)
      if (any(!equal)) {
        token_ids <- which(!equal)
        problems <- c(
          problems,
          sprintf(
            "%s/%s/token_%d differs",
            path,
            coordinate,
            token_ids
          )
        )
      }
    }
    if (n_compared == 0L) {
      problems <- c(
        problems,
        paste("no comparable numeric cells:", path)
      )
    }
  }
  if (length(problems)) problems else TRUE
}
