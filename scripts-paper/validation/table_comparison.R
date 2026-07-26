# Strict comparisons of schema-3 published-table records.

paper_source_once(paper_path("validation", "table_record.R"))

paper_table_tokens_equal <- function(reference, candidate) {
  stopifnot(nrow(reference) == nrow(candidate))
  difference <- abs(reference$value - candidate$value)
  rounding_overlap <- (reference$quantum + candidate$quantum) / 2
  scale <- pmax(
    abs(reference$value),
    abs(candidate$value),
    rounding_overlap
  )
  slack <- 8 * .Machine$double.eps * scale
  difference == 0 |
    difference < rounding_overlap - slack
}

paper_numeric_coordinates <- function(table) {
  sort(names(table)[vapply(table, nrow, integer(1)) > 0L])
}

paper_coordinate_difference <- function(path, reference, candidate) {
  missing <- setdiff(reference, candidate)
  extra <- setdiff(candidate, reference)
  if (!length(missing) && !length(extra)) {
    return(character())
  }
  details <- c(
    if (length(missing)) {
      paste("missing candidate:", paste(missing, collapse = ", "))
    },
    if (length(extra)) {
      paste("extra candidate:", paste(extra, collapse = ", "))
    }
  )
  paste0(
    "numeric coordinates differ: ",
    path,
    " (",
    paste(details, collapse = "; "),
    ")"
  )
}

paper_compare_table_records <- function(reference, candidate) {
  paper_validate_table_record(reference)
  paper_validate_table_record(candidate)
  reference_paths <- sort(names(reference$published_tables))
  candidate_paths <- sort(names(candidate$published_tables))
  if (!identical(reference_paths, candidate_paths)) {
    return(c(
      if (length(setdiff(reference_paths, candidate_paths))) {
        paste(
          "missing candidate tables:",
          paste(setdiff(reference_paths, candidate_paths), collapse = ", ")
        )
      },
      if (length(setdiff(candidate_paths, reference_paths))) {
        paste(
          "extra candidate tables:",
          paste(setdiff(candidate_paths, reference_paths), collapse = ", ")
        )
      }
    ))
  }
  problems <- character()
  for (path in reference_paths) {
    reference_table <- reference$published_tables[[path]]
    candidate_table <- candidate$published_tables[[path]]
    reference_coordinates <- paper_numeric_coordinates(reference_table)
    candidate_coordinates <- paper_numeric_coordinates(candidate_table)
    problems <- c(
      problems,
      paper_coordinate_difference(path, reference_coordinates, candidate_coordinates)
    )
    for (coordinate in intersect(reference_coordinates, candidate_coordinates)) {
      reference_cell <- reference_table[[coordinate]]
      candidate_cell <- candidate_table[[coordinate]]
      if (nrow(reference_cell) != nrow(candidate_cell)) {
        problems <- c(
          problems,
          sprintf(
            "token counts differ: %s/%s (reference: %d, candidate: %d)",
            path,
            coordinate,
            nrow(reference_cell),
            nrow(candidate_cell)
          )
        )
        next
      }
      unequal_values <- which(!paper_table_tokens_equal(
        reference_cell,
        candidate_cell
      ))
      if (length(unequal_values)) {
        problems <- c(
          problems,
          sprintf(
            "displayed values differ: %s/%s/token_%d",
            path,
            coordinate,
            unequal_values
          )
        )
      }
      unequal_stars <- which(reference_cell$stars != candidate_cell$stars)
      if (length(unequal_stars)) {
        problems <- c(
          problems,
          sprintf(
            "stars differ: %s/%s/token_%d",
            path,
            coordinate,
            unequal_stars
          )
        )
      }
    }
  }
  if (length(problems)) problems else TRUE
}
