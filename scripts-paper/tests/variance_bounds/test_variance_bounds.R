# Contract and edge-case checks for the variance-bounds figure and table builders.
# Run from the package root:
# Rscript scripts-paper/tests/variance_bounds/test_variance_bounds.R

source(file.path("scripts-paper", "config", "paths.R"))
source(paper_path("config", "artifacts.R"))
source(paper_path("variance_bounds", "figures", "plot.R"))
source(paper_path("variance_bounds", "tables", "build_summary.R"))

.pass <- 0L
.fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    .pass <<- .pass + 1L
    cat(sprintf("PASS  %s\n", label))
  } else {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s\n", label))
  }
}

fixture_df <- data.frame(
  Maturity = c(3, 6, 9),
  Variance_Bound = c(1e-9, 3e-9, 2e-9)
)
svg_path <- tempfile(fileext = ".svg")
variance_bounds_render_figure(fixture_df, svg_path)
check(
  "figure builder writes a non-empty SVG",
  file.exists(svg_path) && file.info(svg_path)$size > 0
)

empty_message <- tryCatch(
  {
    variance_bounds_render_figure(fixture_df[0, ], tempfile(fileext = ".svg"))
    ""
  },
  error = conditionMessage
)
check("figure builder rejects an empty frame", nzchar(empty_message))

bad_cols_message <- tryCatch(
  {
    variance_bounds_render_figure(
      data.frame(Maturity = 1, other = 2), tempfile(fileext = ".svg")
    )
    ""
  },
  error = conditionMessage
)
check("figure builder rejects a mis-columned frame", nzchar(bad_cols_message))

fixture_summary <- c(
  Mean = 2.48e-9, Median = 2.77e-9, Minimum = 1.29e-10,
  Maximum = 4.56e-9, "Standard Deviation" = 1.32e-9
)
table_lines <- variance_bounds_table_lines(fixture_summary)
check(
  "table carries all five statistic row labels",
  all(vapply(
    names(fixture_summary),
    function(lbl) any(grepl(lbl, table_lines, fixed = TRUE)),
    logical(1)
  ))
)
check(
  "table formats values in siunitx scientific notation",
  any(grepl("\\num{", table_lines, fixed = TRUE))
)
check(
  "table carries the caption and label",
  any(grepl("Variance Bounds Summary Statistics", table_lines, fixed = TRUE)) &&
    any(grepl("tab:variance_bounds_summary", table_lines, fixed = TRUE))
)

bad_len_message <- tryCatch(
  {
    variance_bounds_table_lines(c(1, 2, 3))
    ""
  },
  error = conditionMessage
)
check("table builder rejects a wrong-length summary", nzchar(bad_len_message))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
