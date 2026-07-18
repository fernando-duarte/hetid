# Shared table/threeparttable environment and notes renderer.

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
      "\\begin{tablenotes}[flushleft]",
      "\\scriptsize",
      paste0(
        "\\item ",
        if (nzchar(notes_label)) paste0(notes_label, " ") else "",
        paste(notes, collapse = " ")
      ),
      "\\end{tablenotes}"
    )
  }
  c(lines, "\\end{threeparttable}", "\\end{table}")
}
