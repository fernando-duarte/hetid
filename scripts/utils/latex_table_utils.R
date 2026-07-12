# LaTeX panel-table helpers
# Booktabs/threeparttable/siunitx layout with bold panel rows, a column-group
# spanner, and tablenotes, replicating the legacy publication format. Also
# provides a standalone compilable document variant for quick PDF checks.

# Brace-wrap any cell a siunitx S column cannot parse as a plain number
# (e.g. "unbounded", "unreliable", "--"); leave numbers and already-braced
# cells untouched.
.brace_s_cell <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "--"
  needs_brace <- !grepl("^[-+]?[0-9]*\\.?[0-9]+$", x) & !grepl("^\\{.*\\}$", x)
  x[needs_brace] <- paste0("{", x[needs_brace], "}")
  x
}

#' Build a booktabs/threeparttable/siunitx multi-panel LaTeX table
#'
#' @param panels named list; names are panel titles rendered as bold
#'   "Panel <letter>: <title>" rows (letter assigned by position). Each
#'   element is a data frame whose first column holds row labels (character,
#'   may contain math) and whose remaining length(col_headers) columns hold
#'   pre-formatted character cell values.
#' @param col_headers character vector of column headers (e.g. maturities)
#' @param caption table caption
#' @param label LaTeX label key
#' @param notes character vector for the tablenotes block (NULL to omit);
#'   elements are concatenated into a single Notes item
#' @param col_group_label spanner text over the numeric columns
#' @param table_format siunitx S-column table-format specification
#' @return character vector of LaTeX lines (table environment fragment)
build_panel_latex_table <- function(panels, col_headers, caption, label,
                                    notes = NULL,
                                    col_group_label = "Maturity (months)",
                                    table_format = "1.3") {
  n_cols <- length(col_headers)
  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\begin{threeparttable}",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    paste0(
      "\\begin{tabular}{l@{\\hskip 0.5in}*{", n_cols,
      "}{S[table-format=", table_format, "]}}"
    ),
    "\\toprule",
    paste0("& \\multicolumn{", n_cols, "}{c}{", col_group_label, "} \\\\"),
    paste0("\\cmidrule(lr){2-", n_cols + 1, "}"),
    paste0("& ", paste0("{", col_headers, "}", collapse = " & "), " \\\\"),
    "\\midrule"
  )

  for (panel_idx in seq_along(panels)) {
    panel_df <- panels[[panel_idx]]
    stopifnot(ncol(panel_df) == n_cols + 1)
    spacing <- if (panel_idx == 1) {
      "\\addlinespace[0.5em]"
    } else {
      "\\addlinespace[1em]"
    }
    lines <- c(
      lines,
      spacing,
      paste0(
        "\\multicolumn{", n_cols + 1, "}{l}{\\textbf{Panel ",
        LETTERS[panel_idx], ": ", names(panels)[panel_idx], "}} \\\\"
      ),
      "\\addlinespace[0.3em]"
    )
    for (row_idx in seq_len(nrow(panel_df))) {
      cells <- .brace_s_cell(unlist(panel_df[row_idx, -1]))
      lines <- c(
        lines,
        paste0(
          "\\quad ", as.character(panel_df[row_idx, 1]), " & ",
          paste(cells, collapse = " & "), " \\\\"
        )
      )
    }
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  if (!is.null(notes)) {
    lines <- c(
      lines,
      "\\begin{tablenotes}[flushleft]",
      "\\scriptsize",
      paste0("\\item \\textit{Notes:} ", paste(notes, collapse = " ")),
      "\\end{tablenotes}"
    )
  }
  c(lines, "\\end{threeparttable}", "\\end{table}")
}

#' Wrap a table fragment in a compilable standalone LaTeX document
#'
#' @param table_lines character vector from build_panel_latex_table()
#' @return character vector of LaTeX lines for a complete document
make_standalone_latex <- function(table_lines, landscape = FALSE) {
  geometry <- if (isTRUE(landscape)) {
    "\\usepackage[landscape,margin=1in]{geometry}"
  } else {
    "\\usepackage[margin=1in]{geometry}"
  }
  c(
    "\\documentclass[11pt]{article}",
    "\\usepackage{booktabs}",
    "\\usepackage{threeparttable}",
    "\\usepackage{siunitx}",
    "\\usepackage{array}",
    "\\usepackage{amssymb}",
    geometry,
    "",
    "\\begin{document}",
    "",
    table_lines,
    "",
    "\\end{document}"
  )
}

#' Compile a LaTeX document with latexmk and clean its auxiliary files
#'
#' Stops on failure, so a LaTeX regression fails the calling pipeline.
#'
#' @param tex_path path to the .tex document to compile
compile_latex_pdf <- function(tex_path) {
  status <- system2(
    "latexmk", c("-cd", "-pdf", "-silent", tex_path),
    stdout = FALSE, stderr = FALSE
  )
  if (status != 0) stop("latexmk failed on ", tex_path, " (status ", status, ")")
  invisible(system2(
    "latexmk", c("-cd", "-c", tex_path),
    stdout = FALSE, stderr = FALSE
  ))
}

#' Write a table fragment and (optionally) its standalone document variant
#'
#' Writes <dir>/<base_name>.tex and <dir>/<base_name>_standalone.tex.
#'
#' @param table_lines character vector from build_panel_latex_table()
#' @param dir output directory (created if missing)
#' @param base_name file name without extension
#' @param standalone whether to also write the standalone variant
#' @return (invisibly) character vector of paths written
write_latex_table <- function(table_lines, dir, base_name, standalone = TRUE,
                              landscape = FALSE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  fragment_path <- file.path(dir, paste0(base_name, ".tex"))
  writeLines(table_lines, fragment_path)
  paths <- fragment_path
  if (standalone) {
    standalone_path <- file.path(dir, paste0(base_name, "_standalone.tex"))
    writeLines(make_standalone_latex(table_lines, landscape = landscape), standalone_path)
    paths <- c(paths, standalone_path)
  }
  invisible(paths)
}
