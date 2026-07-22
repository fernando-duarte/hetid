# Driver: regenerate the paper's quoted SDF approximation-error numbers from
# the completed variance-bounds frame and the quarterly ACM inputs, assert the
# quoted claims so a drifted number fails the pipeline loudly, and write the
# csv (diagnostics) and markdown note (reports) artifacts.

paper_source_once(paper_path(
  "variance_bounds", "quoted", "build_quoted_numbers.R"
))

stopifnot(
  "bounds frame must sit on the canonical quarterly maturity grid" =
    identical(
      variance_bounds_df$Maturity,
      seq(step_qtr, hetid::effective_max_maturity(step_qtr), by = step_qtr)
    )
)
quoted_y1q <- unname(quarterly_acm_inputs$yields[
  , hetid::acm_column_name("yields", step_qtr)
]) / 400
quoted_numbers <- variance_bounds_quoted_numbers(variance_bounds_df, quoted_y1q)
quoted_value <- function(nm) {
  v <- quoted_numbers$value[quoted_numbers$name == nm]
  stopifnot(
    "quoted name must match exactly one finite value" =
      length(v) == 1L && is.finite(v)
  )
  v
}

# the claims as quoted in the paper; fail loud if a regeneration breaks one
stopifnot(
  "draft quotes max news bound 4.19e-9" =
    abs(quoted_value("max_bound_sdf_news") - 4.19e-9) < 5e-12,
  "draft quotes max expected bound 3.19e-9" =
    abs(quoted_value("max_bound_expected_sdf") - 3.19e-9) < 5e-12,
  "error sd below 0.7 bp at every maturity" =
    quoted_value("max_error_sd_bp") < 0.7,
  "error sd below 3 percent of a typical quarterly yield move" =
    quoted_value("max_error_sd_over_typical_move_pct") < 3,
  "draft quotes typical quarterly one-quarter-yield move 0.00239" =
    abs(
      quoted_value("typical_quarterly_move_one_quarter_yield") - 0.00239
    ) < 5e-6
)

quoted_sample_txt <- sprintf(
  "quarterly, %s to %s, T = %d observations",
  format(min(quarterly_acm_inputs$dates), "%Y-%m"),
  format(max(quarterly_acm_inputs$dates), "%Y-%m"),
  length(quarterly_acm_inputs$dates)
)
# md lines are built before either write; the csv (untracked diagnostic) may
# still land if the later md write itself fails
quoted_md_lines <- variance_bounds_quoted_md_lines(
  quoted_numbers, variance_bounds_df, quoted_sample_txt
)
paper_write_typed_csv(
  quoted_numbers,
  artifact_path("quoted_numbers_csv"),
  "quoted numbers csv"
)
writeLines(quoted_md_lines, artifact_path("quoted_numbers_md"))

cat(
  "quoted numbers:", nrow(quoted_numbers), "values;",
  sprintf("max error sd %.3f bp\n", quoted_value("max_error_sd_bp"))
)

rm(
  quoted_y1q, quoted_numbers, quoted_value, quoted_sample_txt,
  quoted_md_lines
)
