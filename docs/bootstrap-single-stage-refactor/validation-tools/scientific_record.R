source(file.path(
  "scripts-paper",
  "tests",
  "support",
  "published_table_comparison.R"
))

bootstrap_validation_record <- function(output_root) {
  list(
    schema_version = 2L,
    published_tables =
      paper_published_tables_projection(output_root)
  )
}
