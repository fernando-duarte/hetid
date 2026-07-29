# Direct comparisons of published-table numeric projections.

paper_source_once(paper_path("validation", "table_projection.R"))

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

paper_projection_names <- function(projection) {
  projection_names <- names(projection)
  if (is.null(projection_names)) character() else sort(projection_names)
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

paper_table_path_difference <- function(reference, candidate) {
  missing <- setdiff(reference, candidate)
  extra <- setdiff(candidate, reference)
  c(
    if (length(missing)) {
      paste(
        "missing candidate numeric tables:",
        paste(missing, collapse = ", ")
      )
    },
    if (length(extra)) {
      paste(
        "extra candidate numeric tables:",
        paste(extra, collapse = ", ")
      )
    }
  )
}

paper_compare_table_projections <- function(reference, candidate) {
  reference_paths <- paper_projection_names(reference)
  candidate_paths <- paper_projection_names(candidate)
  problems <- paper_table_path_difference(reference_paths, candidate_paths)
  for (path in intersect(reference_paths, candidate_paths)) {
    reference_table <- reference[[path]]
    candidate_table <- candidate[[path]]
    reference_coordinates <- paper_projection_names(reference_table)
    candidate_coordinates <- paper_projection_names(candidate_table)
    problems <- c(
      problems,
      paper_coordinate_difference(
        path,
        reference_coordinates,
        candidate_coordinates
      )
    )
    for (coordinate in intersect(
      reference_coordinates,
      candidate_coordinates
    )) {
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

paper_compare_output_tables <- function(
  reference_output_root,
  candidate_output_root
) {
  paper_compare_table_projections(
    paper_published_tables_projection(reference_output_root),
    paper_published_tables_projection(candidate_output_root)
  )
}
