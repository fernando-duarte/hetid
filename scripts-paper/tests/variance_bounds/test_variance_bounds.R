# Contract and edge-case checks for the variance-bounds figure and table builders.
# Run from the package root:
# Rscript scripts-paper/tests/variance_bounds/test_variance_bounds.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("variance_bounds", "figures", "plot.R"))
paper_source_once(paper_path("variance_bounds", "tables", "build_summary.R"))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

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

device_before <- grDevices::dev.cur()
failed_svg <- tempfile(fileext = ".svg")
svg_error <- try(
  write_svg(
    failed_svg,
    2,
    2,
    function() stop("fixture draw error")
  ),
  silent = TRUE
)
check(
  "SVG wrapper closes its device when drawing fails",
  inherits(svg_error, "try-error") &&
    identical(grDevices::dev.cur(), device_before)
)
unlink(failed_svg)

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
  "table formats values in plain-math scientific notation without siunitx",
  any(grepl("\\times 10^{", table_lines, fixed = TRUE)) &&
    !any(grepl("\\num{", table_lines, fixed = TRUE))
)
check(
  "table is a bare tabular fragment without a float or caption",
  identical(table_lines[[1L]], "\\begin{tabular}{lc}") &&
    identical(table_lines[[length(table_lines)]], "\\end{tabular}") &&
    !any(grepl("\\begin{table}", table_lines, fixed = TRUE)) &&
    !any(grepl("\\caption", table_lines, fixed = TRUE))
)

bad_len_message <- tryCatch(
  {
    variance_bounds_table_lines(c(1, 2, 3))
    ""
  },
  error = conditionMessage
)
check("table builder rejects a wrong-length summary", nzchar(bad_len_message))

.test$finish()
