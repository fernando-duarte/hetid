# The paper's own table scaffold, matched to the hand-tuned fragments in the
# V-FCI-Overleaf repository so a published table drops straight in.
#
# Two scaffolds are in use there. The plain one is a \begingroup that scopes a
# 12pt font over an ordinary booktabs tabular. The kern one replaces the
# booktabs rules with \cmidrule variants and adjusts \cmidrulekern per rule
# position, which is what closes the gap booktabs leaves between adjacent
# \cmidrule segments; the kerns are set \global inside \noalign, so they escape
# the group and have to be restored by hand after \endgroup.
#
# These constants live outside config/reporting.R deliberately. That file is one
# of BOOTSTRAP_STAGE_CODE_FILES, so editing it digests differently and discards
# the cached bootstrap draws, and a font size cannot change a draw.

# The \cmidrulekern settings, by rule position. Outer rules pull their kern
# negative so the heavy rule spans the full width; a spanner row's rules push
# theirs positive so adjacent column groups stay visually separate.
PAPER_OVERLEAF_KERN_OUTER <-
  "\\newcommand{\\kernouter}{\\noalign{\\global\\cmidrulekern=-6pt}}"
PAPER_OVERLEAF_KERN_SPAN <-
  "\\newcommand{\\kernspan}{\\noalign{\\global\\cmidrulekern=3pt}}"
PAPER_OVERLEAF_KERN_MID <-
  "\\newcommand{\\kernmid}{\\noalign{\\global\\cmidrulekern=-2.5pt}}"
PAPER_OVERLEAF_KERN_INNER <-
  "\\newcommand{\\kerninner}{\\noalign{\\global\\cmidrulekern=1pt}}"

# booktabs' default kern is .5em, which is 6pt at the 12pt body size.
PAPER_OVERLEAF_KERN_RESTORE <- c(
  paste(
    "\\global\\cmidrulekern=6pt % restore booktabs default (.5em at 12pt):",
    "the \\global"
  ),
  paste(
    "% assignments above escape \\endgroup and would otherwise leak into",
    "the rest of the document"
  )
)

PAPER_OVERLEAF_FONTSIZE <- "\\fontsize{12.0pt}{14.4pt}\\selectfont"

# Full-width rules for a kern-scaffold tabular of n_col + 1 columns (the row
# label column plus n_col data columns).
paper_overleaf_rule <- function(n_col, kern, weight) {
  sprintf(
    "\\%s\\cmidrule[\\%s](lr){1-%d}", kern, weight, n_col + 1L
  )
}
paper_overleaf_outer_rule <- function(n_col) {
  paper_overleaf_rule(n_col, "kernouter", "heavyrulewidth")
}
paper_overleaf_panel_rule <- function(n_col) {
  paper_overleaf_rule(n_col, "kernmid", "lightrulewidth")
}
paper_overleaf_inner_rule <- function(n_col) {
  sprintf("\\kerninner\\cmidrule(lr){1-%d}", n_col + 1L)
}

# A centered full-width panel title, e.g. "Panel A: Mean equation". Plain text,
# neither bold nor italic, which is the paper's convention.
paper_overleaf_panel_head <- function(n_col, title) {
  sprintf("\\multicolumn{%d}{c}{%s} \\\\", n_col + 1L, title)
}

# The two-tier column header: a spanner row grouping the data columns, its
# \cmidrule segments, then the per-column header row. `groups` is a list of
# list(label, n) whose n's must cover every data column. prefix and join carry
# the kern scaffold's \kernspan and its run-together segments; a booktabs caller
# passes "" and " ". The lone builder of a spanner row, so the segment ranges
# cannot drift between the kern tables and the plain ones.
paper_overleaf_column_headers <- function(groups, headers,
                                          prefix = "\\kernspan", join = "") {
  ns <- vapply(groups, function(g) g$n, integer(1))
  stopifnot(sum(ns) == length(headers))
  spans <- character(0)
  start <- 2L
  for (n in ns) {
    spans <- c(spans, sprintf("\\cmidrule(lr){%d-%d}", start, start + n - 1L))
    start <- start + n
  }
  c(
    paste0(
      " & ",
      paste(
        vapply(
          groups,
          function(g) sprintf("\\multicolumn{%d}{c}{%s}", g$n, g$label),
          character(1)
        ),
        collapse = " & "
      ),
      " \\\\"
    ),
    paste0(prefix, paste(spans, collapse = join)),
    paste0(" & ", paste(headers, collapse = " & "), " \\\\")
  )
}

# The rule set a tabular opens, separates its header with, and closes on.
# Booktabs is the default; the kern scaffold swaps in \cmidrule variants.
PAPER_BOOKTABS_RULES <- list(
  top = "\\toprule",
  header = "\\midrule",
  bottom = "\\bottomrule",
  span_prefix = "",
  span_join = " "
)

paper_overleaf_rules <- function(n_col) {
  list(
    top = paper_overleaf_outer_rule(n_col),
    header = paper_overleaf_inner_rule(n_col),
    bottom = paper_overleaf_outer_rule(n_col),
    span_prefix = "\\kernspan",
    span_join = ""
  )
}

# Wrap a tabular in the plain 12pt group.
paper_overleaf_plain_group <- function(tabular_lines) {
  c("\\begingroup", PAPER_OVERLEAF_FONTSIZE, tabular_lines, "\\endgroup")
}

# Wrap a tabular in the kern group, declaring the macros before it and restoring
# booktabs' kern after it. `fontsize` is emitted between the two. \kernmid is
# declared only for a table that separates panels with a light rule; a
# single-panel table never uses it and does not declare it.
paper_overleaf_kern_group <- function(tabular_lines, fontsize,
                                      panel_rule = FALSE) {
  c(
    "\\begingroup",
    PAPER_OVERLEAF_KERN_OUTER,
    PAPER_OVERLEAF_KERN_SPAN,
    if (isTRUE(panel_rule)) PAPER_OVERLEAF_KERN_MID,
    PAPER_OVERLEAF_KERN_INNER,
    fontsize,
    tabular_lines,
    PAPER_OVERLEAF_KERN_RESTORE,
    "\\endgroup"
  )
}
