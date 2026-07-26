# Compatibility loader for published-table comparisons.

paper_source_once(paper_path("validation", "table_comparison.R"))

paper_table_cell_numbers <- paper_table_cell_results
paper_published_tables_compare <- function(reference, candidate) {
  paper_compare_table_records(
    list(schema_version = 3L, published_tables = reference),
    list(schema_version = 3L, published_tables = candidate)
  )
}
