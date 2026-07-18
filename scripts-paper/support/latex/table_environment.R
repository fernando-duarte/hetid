# Shared table/threeparttable environment and notes renderer.

latex_tablenotes <- function(
  notes,
  notes_label = "Notes:",
  separate_items = FALSE
) {
  prefix <- if (nzchar(notes_label)) {
    paste0(notes_label, " ")
  } else {
    ""
  }
  items <- if (isTRUE(separate_items)) {
    paste0("\\item ", notes)
  } else {
    paste0("\\item ", prefix, paste(notes, collapse = " "))
  }
  c(
    "\\begin{tablenotes}[flushleft]",
    "\\scriptsize",
    items,
    "\\end{tablenotes}"
  )
}

latex_table_environment <- function(
  tabular_lines,
  caption,
  label,
  notes = NULL,
  notes_label = "Notes:",
  fontsize = ""
) {
  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\begin{threeparttable}",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    if (nzchar(fontsize)) fontsize else NULL,
    tabular_lines
  )
  if (!is.null(notes)) {
    lines <- c(
      lines,
      latex_tablenotes(notes, notes_label)
    )
  }
  c(lines, "\\end{threeparttable}", "\\end{table}")
}
