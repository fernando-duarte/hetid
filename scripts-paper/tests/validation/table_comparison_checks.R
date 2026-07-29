# Focused checks for direct published-table comparisons.

source(file.path("scripts-paper", "config", "paths.R"))

paper_source_once(paper_path("validation", "table_comparison.R"))

comparison_cell <- function(value, quantum = 0.01, stars = "") {
  data.frame(
    value = value,
    quantum = rep(quantum, length(value)),
    stars = rep(stars, length(value)),
    stringsAsFactors = FALSE
  )
}

coordinate <- "tabular_1/row_1/column_1"
second_coordinate <- "tabular_1/row_1/column_2"
reference <- list("table.tex" = list(
  "tabular_1/row_1/column_1" = comparison_cell(1.234),
  "tabular_1/row_1/column_2" = comparison_cell(4.56)
))
candidate <- list("table.tex" = list(
  "tabular_1/row_1/column_1" = comparison_cell(1.23),
  "tabular_1/row_1/column_2" = comparison_cell(4.560)
))
stopifnot(isTRUE(paper_compare_table_projections(reference, candidate)))

candidate$table.tex[[coordinate]]$stars <- "**"
problems <- paper_compare_table_projections(reference, candidate)
stopifnot(any(grepl("stars differ", problems, fixed = TRUE)))
candidate$table.tex[[coordinate]]$stars <- ""

reference$table.tex[[coordinate]]$value <- 1.23
candidate$table.tex[[coordinate]]$value <- 1.24
problems <- paper_compare_table_projections(reference, candidate)
stopifnot(any(grepl("displayed values differ", problems, fixed = TRUE)))
reference$table.tex[[coordinate]]$value <- 1.234
candidate$table.tex[[coordinate]]$value <- 1.23

candidate$table.tex[[coordinate]] <- comparison_cell(c(1.23, 2.34))
problems <- paper_compare_table_projections(reference, candidate)
stopifnot(any(grepl("token counts differ", problems, fixed = TRUE)))
candidate$table.tex[[coordinate]] <- comparison_cell(1.23)

candidate$table.tex[[coordinate]] <- NULL
problems <- paper_compare_table_projections(reference, candidate)
stopifnot(any(grepl("numeric coordinates differ", problems, fixed = TRUE)))
candidate$table.tex[[coordinate]] <- comparison_cell(1.23)

moved_candidate <- list("table.tex" = list(
  "tabular_1/row_1/column_2" = comparison_cell(1.23),
  "tabular_1/row_1/column_3" = comparison_cell(4.560)
))
problems <- paper_compare_table_projections(reference, moved_candidate)
stopifnot(any(grepl("numeric coordinates differ", problems, fixed = TRUE)))

missing_table_reference <- reference
missing_table_reference$extra.tex <- list(
  "tabular_1/row_1/column_1" = comparison_cell(1.23)
)
problems <- paper_compare_table_projections(
  missing_table_reference,
  candidate
)
stopifnot(any(grepl("missing candidate numeric tables", problems, fixed = TRUE)))

extra_table <- candidate
extra_table$extra.tex <- list(
  "tabular_1/row_1/column_1" = comparison_cell(1.23)
)
problems <- paper_compare_table_projections(reference, extra_table)
stopifnot(any(grepl("extra candidate numeric tables", problems, fixed = TRUE)))

empty_projection <- stats::setNames(list(), character())
stopifnot(isTRUE(paper_compare_table_projections(
  empty_projection,
  empty_projection
)))

scientific_reference <- list("table.tex" = list(
  "tabular_1/row_1/column_1" = comparison_cell(2.31e-9, 1e-11)
))
scientific_candidate <- list("table.tex" = list(
  "tabular_1/row_1/column_1" = comparison_cell(2.32e-9, 1e-11)
))
stopifnot(!isTRUE(paper_compare_table_projections(
  scientific_reference,
  scientific_candidate
)))

rm(
  comparison_cell,
  coordinate,
  second_coordinate,
  reference,
  candidate,
  problems,
  moved_candidate,
  missing_table_reference,
  extra_table,
  empty_projection,
  scientific_reference,
  scientific_candidate
)
