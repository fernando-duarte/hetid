# Checks for the panel tabular builder and the decimal alignment of its cells.
# Run from the package root:
#   Rscript scripts-paper/tests/support/panel_table_checks.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("support", "reporting", "cells.R"))
paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

# decimal_align pads one column at a time; negatives arrive already wrapped in
# math by paper_math_negative, as the render script hands them over
column <- function(...) matrix(c(...), ncol = 1L)
check(
  "a column with a negative gets a phantom minus on its non-negative cells",
  identical(
    decimal_align(column("0.409", "$-1.203$", "0.167")),
    column("$\\phantom{-}0.409$", "$-1.203$", "$\\phantom{-}0.167$")
  )
)
check(
  "a column without negatives gets no phantom minus",
  identical(
    decimal_align(column("0.409", "0.339")),
    column("$0.409$", "$0.339$")
  )
)
check(
  "a shorter integer part gets one phantom digit per missing digit",
  identical(
    decimal_align(column("12.345", "1.234", "123.4")),
    column("$\\phantom{0}12.345$", "$\\phantom{0}\\phantom{0}1.234$", "$123.4$")
  )
)
check(
  "sign and digit padding combine so the cells share one width",
  identical(
    decimal_align(column("$-1.5$", "12.5")),
    column("$\\phantom{0}-1.5$", "$\\phantom{-}12.5$")
  )
)
check(
  "columns are padded independently of each other",
  identical(
    decimal_align(cbind(c("0.1", "$-0.2$"), c("12.3", "1.2"))),
    cbind(c("$\\phantom{-}0.1$", "$-0.2$"), c("$12.3$", "$\\phantom{0}1.2$"))
  )
)

# panel_tabular_lines on two data columns: a per-column panel, then a panel
# whose only row carries one value in its first cell and nothing after it
panels <- list(
  "Per-column rows" = data.frame(
    label = c("first", "second"), a = c("0.1", "0.3"), b = c("0.2", "0.4"),
    stringsAsFactors = FALSE
  ),
  "Joint row" = data.frame(
    label = "joint", a = "0.007***", b = "", stringsAsFactors = FALSE
  )
)
lines <- panel_tabular_lines(panels, col_headers = c("1", "2"), col_group_label = "Group")
title_at <- function(letter) {
  which(startsWith(lines, sprintf("\\multicolumn{3}{l}{Panel %s: ", letter)))
}

check(
  "a row with one value in its first cell is plain cells under column 1, not a span",
  "\\quad joint & 0.007*** &  \\\\" %in% lines &&
    !any(grepl("\\multicolumn{2}{c}{0.007***}", lines, fixed = TRUE))
)
check(
  "panel titles are left-aligned rows lettered by position",
  identical(
    lines[c(title_at("A"), title_at("B"))],
    c(
      "\\multicolumn{3}{l}{Panel A: Per-column rows} \\\\",
      "\\multicolumn{3}{l}{Panel B: Joint row} \\\\"
    )
  )
)
check(
  "the first panel opens with the line space alone, after the header's rule",
  identical(
    lines[title_at("A") - 3:1],
    c("& 1 & 2 \\\\", "\\midrule", "\\addlinespace[0.5em]")
  )
)
check(
  "every later panel opens with its own rule and then the line space",
  identical(
    lines[title_at("B") - 3:1],
    c("\\quad second & 0.3 & 0.4 \\\\", "\\midrule", "\\addlinespace[0.5em]")
  )
)

wide <- panel_tabular_lines(
  panels,
  col_headers = c("1", "2"), col_group_label = "Group", col_width = "2.9cm"
)
check(
  "col_width gives every data column a centered fixed width after the stub",
  wide[[1L]] == paste0(
    "\\begin{tabular}{l@{\\hskip 0.5in}",
    ">{\\centering\\arraybackslash}p{2.9cm}>{\\centering\\arraybackslash}p{2.9cm}}"
  ) &&
    identical(wide[-1L], lines[-1L])
)
check(
  "the default column width keeps plain centered columns",
  lines[[1L]] == "\\begin{tabular}{l@{\\hskip 0.5in}cc}"
)

.test$finish()
