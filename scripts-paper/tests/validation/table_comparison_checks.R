# Focused checks for schema-3 published-table comparisons.

paper_source_once(paper_path("validation", "table_comparison.R"))

comparison_cell <- function(value, quantum = 0.01, stars = "") {
  data.frame(
    value = value,
    quantum = rep(quantum, length(value)),
    stars = rep(stars, length(value)),
    stringsAsFactors = FALSE
  )
}

comparison_record <- function(cells) {
  list(
    schema_version = 3L,
    published_tables = list("table.tex" = cells)
  )
}

coordinate <- "tabular_1/row_1/column_1"
reference <- comparison_record(list(
  "tabular_1/row_1/column_1" = comparison_cell(1.234),
  "tabular_1/row_1/column_2" = comparison_cell(4.56),
  "tabular_1/row_2/column_1" = data.frame(
    value = double(), quantum = double(), stars = character()
  )
))
candidate <- comparison_record(list(
  "tabular_1/row_1/column_1" = comparison_cell(1.23),
  "tabular_1/row_1/column_2" = comparison_cell(4.560),
  "tabular_1/row_2/column_1" = data.frame(
    value = double(), quantum = double(), stars = character()
  )
))
stopifnot(isTRUE(paper_compare_table_records(reference, candidate)))

candidate$published_tables$table.tex[[coordinate]]$stars <- "**"
problems <- paper_compare_table_records(reference, candidate)
stopifnot(
  !isTRUE(problems),
  any(grepl("stars differ", problems, fixed = TRUE))
)
candidate$published_tables$table.tex[[coordinate]]$stars <- ""

reference$published_tables$table.tex[[coordinate]]$value <- 1.23
candidate$published_tables$table.tex[[coordinate]]$value <- 1.24
problems <- paper_compare_table_records(reference, candidate)
stopifnot(any(grepl("displayed values differ", problems, fixed = TRUE)))
reference$published_tables$table.tex[[coordinate]]$value <- 1.234
candidate$published_tables$table.tex[[coordinate]]$value <- 1.23

scientific_reference <- comparison_record(list(
  "tabular_1/row_1/column_1" = comparison_cell(2.31e-9, 1e-11)
))
scientific_candidate <- comparison_record(list(
  "tabular_1/row_1/column_1" = comparison_cell(2.32e-9, 1e-11)
))
stopifnot(!isTRUE(paper_compare_table_records(
  scientific_reference,
  scientific_candidate
)))

candidate$published_tables$table.tex[[coordinate]] <- comparison_cell(c(1.23, 2.34))
problems <- paper_compare_table_records(reference, candidate)
stopifnot(any(grepl("token counts differ", problems, fixed = TRUE)))
candidate$published_tables$table.tex[[coordinate]] <- comparison_cell(1.23)

candidate$published_tables$table.tex[[coordinate]] <- data.frame(
  value = double(), quantum = double(), stars = character()
)
problems <- paper_compare_table_records(reference, candidate)
stopifnot(any(grepl("numeric coordinates differ", problems, fixed = TRUE)))
candidate$published_tables$table.tex[[coordinate]] <- comparison_cell(1.23)

candidate$published_tables$table.tex[["tabular_1/row_3/column_1"]] <- comparison_cell(5.67)
problems <- paper_compare_table_records(reference, candidate)
stopifnot(any(grepl("numeric coordinates differ", problems, fixed = TRUE)))
candidate$published_tables$table.tex[["tabular_1/row_3/column_1"]] <- NULL

moved_candidate <- comparison_record(list(
  "tabular_1/row_1/column_2" = comparison_cell(1.23),
  "tabular_1/row_1/column_3" = comparison_cell(4.560),
  "tabular_1/row_2/column_1" = data.frame(
    value = double(), quantum = double(), stars = character()
  )
))
problems <- paper_compare_table_records(reference, moved_candidate)
stopifnot(any(grepl("numeric coordinates differ", problems, fixed = TRUE)))

missing_table_reference <- reference
missing_table_reference$published_tables$extra.tex <- list(
  "tabular_1/row_1/column_1" = comparison_cell(1.23)
)
problems <- paper_compare_table_records(missing_table_reference, candidate)
stopifnot(any(grepl("missing candidate tables", problems, fixed = TRUE)))

extra_table <- candidate
extra_table$published_tables$extra.tex <- list(
  "tabular_1/row_1/column_1" = comparison_cell(1.23)
)
problems <- paper_compare_table_records(reference, extra_table)
stopifnot(any(grepl("extra candidate tables", problems, fixed = TRUE)))

empty_record_table <- comparison_record(list(
  "tabular_1/row_1/column_1" = data.frame(
    value = double(), quantum = double(), stars = character()
  )
))
empty_problem <- tryCatch(
  paper_compare_table_records(empty_record_table, empty_record_table),
  error = function(error) conditionMessage(error)
)
stopifnot(grepl("invalid published-table record:", empty_problem))

definition_roots <- c(
  paper_path("tests", "support"),
  repo_path(
    "docs",
    "bootstrap-single-stage-refactor",
    "validation-tools"
  )
)
definition_files <- unlist(lapply(
  definition_roots,
  list.files,
  pattern = "[.]R$",
  recursive = TRUE,
  full.names = TRUE
))
definition_files <- definition_files[
  !grepl("/archives/", definition_files, fixed = TRUE)
]
definition_patterns <- c(
  numeric_token = "paper_table_(normalize_token|cell_results)\\s*<-\\s*function",
  quantum = "paper_table_number_quantum\\s*<-\\s*function",
  schema_validation = "paper_validate_table_record\\s*<-\\s*function",
  schema_record = "paper_table_record\\s*<-\\s*function",
  rounding_overlap = "paper_table_tokens_equal\\s*<-\\s*function"
)
definition_hits <- unlist(lapply(definition_files, function(path) {
  lines <- readLines(path, warn = FALSE)
  matched <- vapply(
    definition_patterns,
    function(pattern) any(grepl(pattern, lines)),
    logical(1)
  )
  if (!any(matched)) {
    return(character())
  }
  paste0(path, ": ", names(definition_patterns)[matched])
}))
if (length(definition_hits)) {
  stop(
    "duplicate table-acceptance definitions outside scripts-paper/validation: ",
    paste(definition_hits, collapse = "; "),
    call. = FALSE
  )
}

rm(
  comparison_cell,
  comparison_record,
  coordinate,
  reference,
  candidate,
  problems,
  scientific_reference,
  scientific_candidate,
  moved_candidate,
  missing_table_reference,
  extra_table,
  empty_record_table,
  empty_problem,
  definition_roots,
  definition_files,
  definition_patterns,
  definition_hits
)
