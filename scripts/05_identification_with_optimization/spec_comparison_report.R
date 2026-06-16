# Human-facing report for the specification / instrument / tau comparison.
# Reads the grid computed by spec_comparison.R, classifies its coverage from
# the data alone (full grid / quick subgrid / partial), and writes paper
# artifacts whose headlines are computed from the grid -- never hard-coded
# prose that a quick run could falsify.
#
# Input: temp/identification_optimized/spec_comparison_<profile>.rds/.csv,
# preferring the full profile over quick. Override with
# HETID_SPEC_SOURCE=<path .rds|.csv> to report on a saved grid (e.g. a
# preserved full run) without recomputing. The pipeline runner sets
# HETID_SPEC_SOURCE to the quick grid its spec_comparison stage just
# computed, so each pipeline pass refreshes the _quick artifacts even when
# a full-run grid (the preferred default) exists.
# Artifact names carry the coverage suffix: full grid -> canonical names;
# quick subgrid -> _quick; anything else -> _partial. Full-grid artifacts are
# therefore never clobbered by the pipeline's quick-mode runs.

source(here::here("scripts/utils/common_settings.R"))
load_visualization_packages()
load_web_packages()
report_dir <- here::here("scripts/05_identification_with_optimization")
source(file.path(report_dir, "spec_comparison_design.R"))
source(file.path(report_dir, "spec_comparison_report_utils.R"))
source(file.path(report_dir, "spec_comparison_report_stats.R"))
source(file.path(report_dir, "spec_comparison_report_artifacts.R"))
source(file.path(report_dir, "spec_comparison_report_figures.R"))
source(file.path(report_dir, "spec_comparison_report_text.R"))

cli_h1("Spec-comparison report")

temp_dir <- file.path(OUTPUT_TEMP_DIR, "identification_optimized")
paper_dir <- file.path(OUTPUT_TEMP_DIR, "identification")
dir.create(paper_dir, recursive = TRUE, showWarnings = FALSE)

src <- Sys.getenv("HETID_SPEC_SOURCE")
if (!nzchar(src)) {
  candidates <- file.path(temp_dir, c(
    "spec_comparison_full.rds", "spec_comparison_full.csv",
    "spec_comparison_quick.rds", "spec_comparison_quick.csv"
  ))
  src <- c(candidates[file.exists(candidates)], NA_character_)[1]
}
if (is.na(src) || !file.exists(src)) {
  stop(
    "No spec_comparison grid found; run spec_comparison.R first ",
    "or point HETID_SPEC_SOURCE at a saved grid (.rds or .csv)"
  )
}
grid <- if (grepl("\\.rds$", src, ignore.case = TRUE)) {
  readRDS(src)
} else {
  utils::read.csv(src, stringsAsFactors = FALSE)
}
cli_alert_info("Grid source: {.path {src}} ({.val {nrow(grid)}} rows)")

grid <- classify_spec_outcomes(grid)
cov <- spec_coverage(grid)
bl <- spec_bottom_line(grid, cov)
agg <- aggregate_spec_stats(grid)
suffix <- cov$suffix

cli_alert_info(cov$line)
cli_h2("Bottom line")
cli_text(bl$long)

summary_path <- file.path(paper_dir, paste0("spec_comparison_summary", suffix, ".txt"))
writeLines(build_spec_summary_lines(grid, cov, bl, agg, suffix), summary_path)

write.csv(
  agg, file.path(temp_dir, paste0("spec_comparison_agg", suffix, ".csv")),
  row.names = FALSE
)
saveRDS(
  list(coverage = cov, bottom_line = bl, agg = agg),
  file.path(temp_dir, paste0("spec_comparison_report", suffix, ".rds"))
)

cli_h2("Writing tables, figures, and the publication panel")
write_spec_outcome_table(grid, cov, bl, paper_dir, suffix)
write_spec_benchmark_table(grid, cov, paper_dir, suffix)
write_spec_outcomes_figure(grid, cov, bl, paper_dir, suffix)
write_spec_widths_figure(grid, cov, paper_dir, suffix)
write_spec_panel(grid, cov, bl, paper_dir, suffix)

cli_h2("Export summary")
cli_ul(c(
  paste0("Open-this-first summary: spec_comparison_summary", suffix, ".txt"),
  paste0("Outcome table: spec_comparison_outcome_table", suffix, ".html/.tex"),
  paste0("Zero-slack benchmark: spec_comparison_benchmark_table", suffix, ".html/.tex"),
  paste0("Figures: spec_comparison_outcomes", suffix, ".*, spec_comparison_widths", suffix, ".*"),
  paste0("Publication panel: spec_comparison_panel", suffix, ".tex (+ standalone/PDF)"),
  paste0(
    "Aggregate + report objects (temp): spec_comparison_agg", suffix, ".csv, ",
    "spec_comparison_report", suffix, ".rds"
  )
))
cli_alert_success("Paper artifacts saved to {.path {paper_dir}}")
cli_alert_info("Working artifacts saved to {.path {temp_dir}}")
