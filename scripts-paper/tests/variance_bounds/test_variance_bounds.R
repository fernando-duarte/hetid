# Contract and edge-case checks for the variance-bounds figure and table
# builders (two series: SDF news min, expected-SDF min). Run from the package
# root:
# Rscript scripts-paper/tests/variance_bounds/test_variance_bounds.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("variance_bounds", "figures", "plot.R"))
paper_source_once(paper_path("variance_bounds", "tables", "build_summary.R"))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

# Deliberately distinct per-column values so a swapped column is visible in
# every downstream check
fixture_df <- data.frame(
  Maturity = c(3, 6, 9),
  Variance_Bound = c(1e-9, 3e-9, 2e-9),
  Expected_SDF_Bound = c(4e-6, 6e-6, 5e-6)
)

long_df <- variance_bounds_plot_data(fixture_df)
check(
  "plot data doubles the rows with one series level per bound",
  nrow(long_df) == 2L * nrow(fixture_df) &&
    identical(levels(long_df$Series), c("SDF news", "expected SDF"))
)
check(
  "plot data carries each source column into its own series",
  identical(
    long_df$Value[long_df$Series == "SDF news"],
    fixture_df$Variance_Bound
  ) &&
    identical(
      long_df$Value[long_df$Series == "expected SDF"],
      fixture_df$Expected_SDF_Bound
    )
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

for (dropped in c("Variance_Bound", "Expected_SDF_Bound")) {
  bad_cols_message <- tryCatch(
    {
      variance_bounds_render_figure(
        fixture_df[, setdiff(names(fixture_df), dropped)],
        tempfile(fileext = ".svg")
      )
      ""
    },
    error = conditionMessage
  )
  check(
    paste("figure builder rejects a frame missing", dropped),
    nzchar(bad_cols_message)
  )
}

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

fixture_summary <- cbind(
  "SDF news" = c(
    Mean = 2.0e-9, Median = 2.0e-9, Minimum = 1.0e-9,
    Maximum = 3.0e-9, "Standard Deviation" = 1.0e-9
  ),
  "Expected SDF" = c(
    Mean = 5.0e-6, Median = 5.0e-6, Minimum = 4.0e-6,
    Maximum = 6.0e-6, "Standard Deviation" = 1.0e-6
  )
)
table_lines <- variance_bounds_table_lines(fixture_summary)
check(
  "table carries all five statistic row labels",
  all(vapply(
    rownames(fixture_summary),
    function(lbl) any(grepl(lbl, table_lines, fixed = TRUE)),
    logical(1)
  ))
)
check(
  "table carries both series column headers",
  any(grepl("SDF news & Expected SDF", table_lines, fixed = TRUE))
)
check(
  "table formats values in plain-math scientific notation without siunitx",
  any(grepl("\\times 10^{", table_lines, fixed = TRUE)) &&
    !any(grepl("\\num{", table_lines, fixed = TRUE))
)
check(
  "the Mean body row is exact, with the columns in order",
  any(grepl(
    "Mean & $2.00 \\times 10^{-9}$ & $5.00 \\times 10^{-6}$ \\\\",
    table_lines,
    fixed = TRUE
  ))
)
check(
  "table is a bare tabular fragment without a float or caption",
  identical(table_lines[[1L]], "\\begin{tabular}{lcc}") &&
    identical(table_lines[[length(table_lines)]], "\\end{tabular}") &&
    !any(grepl("\\begin{table}", table_lines, fixed = TRUE)) &&
    !any(grepl("\\caption", table_lines, fixed = TRUE))
)

bad_shape_message <- tryCatch(
  {
    variance_bounds_table_lines(c(1, 2, 3))
    ""
  },
  error = conditionMessage
)
check("table builder rejects a non-matrix summary", nzchar(bad_shape_message))

swapped <- fixture_summary[, c("Expected SDF", "SDF news")]
bad_names_message <- tryCatch(
  {
    variance_bounds_table_lines(swapped)
    ""
  },
  error = conditionMessage
)
check(
  "table builder rejects reordered column names",
  nzchar(bad_names_message)
)

.test$finish()
