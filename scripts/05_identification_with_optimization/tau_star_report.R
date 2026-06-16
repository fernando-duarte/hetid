# Human-facing report for the tau* identification-strength analysis.
# Reads the results computed by tau_star_comparison.R from temp and
# writes the paper artifacts: tau* tables, sanitized width sweeps, the
# overlay and blow-up figures, and the combined open-this-first summary.
# Headlines are computed from the saved results, never hard-coded prose that
# a stale run could falsify.
#
# Input: temp/identification_optimized/tau_star_comparison.rds.
# Re-runnable on its own (e.g. to iterate on wording or figure styling)
# without recomputing the sweeps.

source(here::here("scripts/utils/common_settings.R"))
load_visualization_packages()
report_dir <- here::here("scripts/05_identification_with_optimization")
source(file.path(report_dir, "tau_star_report_utils.R"))
source(file.path(report_dir, "tau_star_report_figures.R"))
source(file.path(report_dir, "tau_star_report_text.R"))

cli_h1("tau* report")

temp_dir <- file.path(OUTPUT_TEMP_DIR, "identification_optimized")
paper_dir <- file.path(OUTPUT_TEMP_DIR, "identification")
dir.create(paper_dir, recursive = TRUE, showWarnings = FALSE)

path <- file.path(temp_dir, "tau_star_comparison.rds")
if (!file.exists(path)) {
  stop(
    "Missing tau* results (", basename(path),
    "); run tau_star_comparison.R first"
  )
}
res <- readRDS(path)

write.csv(
  res$tau_stars[, c("gamma", "tau_star", "capped")],
  file.path(paper_dir, "tau_star_comparison.csv"),
  row.names = FALSE
)
write.csv(
  published_sweep(res$sweep),
  file.path(paper_dir, "tau_star_width_sweep.csv"),
  row.names = FALSE
)
ts_vfci <- res$tau_stars$tau_star[res$tau_stars$gamma == "VFCI (rank-1)"]
save_plot_pair(
  plot_tau_star_overlay(res$sweep, res$tau_stars),
  paper_dir, "tau_star_comparison"
)
if (length(ts_vfci) == 1) {
  save_plot_pair(
    plot_vfci_blowup(res$sweep, ts_vfci),
    paper_dir, "tau_star_vfci_blowup"
  )
} else {
  cli_alert_warning(
    "VFCI blow-up plot skipped: no 'VFCI (rank-1)' sweep in this run (custom baseline gamma)"
  )
}
cli_alert_success("Wrote tau* artifacts to {.path {paper_dir}}")

summary_path <- file.path(paper_dir, "tau_star_summary.txt")
writeLines(build_tau_star_summary(res), summary_path)
cli_alert_success("Open-this-first summary: {.path {summary_path}}")
