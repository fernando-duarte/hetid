# Mutation proof for direct final-table acceptance.
source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("validation", "table_comparison.R"))
mutation_write_table <- function(root, lines, relative = "table.tex") {
  path <- file.path(root, "tables", relative)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path)
}

mutation_replace <- function(lines, old, new) {
  sub(old, new, lines, fixed = TRUE)
}

baseline_lines <- c(
  "\\caption{Baseline caption}",
  "\\begin{tabular}{lrr}",
  "Measure & Estimate & Interval \\\\",
  "\\midrule",
  "Main & 1.23$^{***}$ & [-0.50, 2.0] \\\\",
  "Status & not estimated & unavailable \\\\",
  "\\end{tabular}",
  "\\emph{Notes:} Baseline note."
)

mutation_cases <- data.frame(
  id = c(
    "caption_text",
    "header_text",
    "note_text",
    "paired_status_text",
    "extra_nonnumeric_table",
    "non_table_artifact",
    "displayed_value",
    "significance_stars",
    "missing_numeric_token",
    "added_numeric_token",
    "moved_numeric_token",
    "numeric_token_count"
  ),
  accepted = c(
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE
  ),
  stringsAsFactors = FALSE
)

reference_root <- tempfile("table-mutation-reference-")
candidate_root <- tempfile("table-mutation-candidate-")
dir.create(reference_root)
mutation_write_table(reference_root, baseline_lines)

mutation_failures <- character()
mutation_seen <- character()
check_mutation <- function(
  id,
  lines = baseline_lines,
  setup = function(root) invisible(root),
  expected_message = NULL
) {
  unlink(candidate_root, recursive = TRUE)
  dir.create(candidate_root)
  mutation_write_table(candidate_root, lines)
  setup(candidate_root)
  comparison <- paper_compare_output_tables(
    reference_root,
    candidate_root
  )
  expected <- mutation_cases$accepted[mutation_cases$id == id]
  passed <- isTRUE(comparison)
  message_ok <- is.null(expected_message) ||
    any(grepl(expected_message, comparison, fixed = TRUE))
  if (length(expected) != 1L || passed != expected || !message_ok) {
    mutation_failures <<- c(
      mutation_failures,
      paste(id, paste(comparison, collapse = " | "), sep = ": ")
    )
  }
  mutation_seen <<- c(mutation_seen, id)
  invisible(comparison)
}

check_mutation(
  "caption_text",
  mutation_replace(
    baseline_lines,
    "Baseline caption",
    "Changed caption"
  )
)
check_mutation(
  "header_text",
  mutation_replace(baseline_lines, "Measure", "Statistic")
)
check_mutation(
  "note_text",
  mutation_replace(baseline_lines, "Baseline note.", "Changed note.")
)
check_mutation(
  "paired_status_text",
  mutation_replace(baseline_lines, "not estimated", "not available")
)
check_mutation(
  "extra_nonnumeric_table",
  setup = function(root) {
    mutation_write_table(
      root,
      c(
        "\\begin{tabular}{lr}",
        "Measure & Status \\\\",
        "\\midrule",
        "Model & not estimated \\\\",
        "\\end{tabular}"
      ),
      "nonnumeric-only.tex"
    )
  }
)
check_mutation(
  "non_table_artifact",
  setup = function(root) {
    writeLines("ignored", file.path(root, "different.rds"))
  }
)
check_mutation(
  "displayed_value",
  mutation_replace(baseline_lines, "1.23", "1.24"),
  expected_message = "displayed values differ"
)
check_mutation(
  "significance_stars",
  mutation_replace(baseline_lines, "***", "**"),
  expected_message = "stars differ"
)
check_mutation(
  "missing_numeric_token",
  mutation_replace(baseline_lines, "1.23$^{***}$", "not reported"),
  expected_message = "numeric coordinates differ"
)
check_mutation(
  "added_numeric_token",
  mutation_replace(baseline_lines, "not estimated", "7.89"),
  expected_message = "numeric coordinates differ"
)
check_mutation(
  "moved_numeric_token",
  mutation_replace(
    baseline_lines,
    "Main & 1.23$^{***}$ & [-0.50, 2.0]",
    "Main & not reported & [1.23$^{***}$, -0.50, 2.0]"
  ),
  expected_message = "numeric coordinates differ"
)
check_mutation(
  "numeric_token_count",
  mutation_replace(
    baseline_lines,
    "1.23$^{***}$",
    "[1.23$^{***}$, 1.24]"
  ),
  expected_message = "token counts differ"
)

if (!identical(sort(mutation_seen), sort(mutation_cases$id))) {
  mutation_failures <- c(
    mutation_failures,
    "mutation case coverage differs from the declared matrix"
  )
}
if (length(mutation_failures)) {
  stop(
    "table acceptance mutation failures: ",
    paste(mutation_failures, collapse = "; "),
    call. = FALSE
  )
}

unlink(reference_root, recursive = TRUE)
unlink(candidate_root, recursive = TRUE)
rm(
  mutation_write_table,
  mutation_replace,
  baseline_lines,
  mutation_cases,
  reference_root,
  candidate_root,
  mutation_failures,
  mutation_seen,
  check_mutation
)
