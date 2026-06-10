# Human-facing report for the tau* identification-strength analysis.
# Reads the per-mode results computed by tau_star_comparison.R from temp and
# writes the paper artifacts: tau* tables, sanitized width sweeps, the
# overlay and blow-up figures, and the combined open-this-first summary.
# Headlines are computed from the saved results, never hard-coded prose that
# a stale run could falsify.
#
# Input: temp/identification_optimized/tau_star_comparison_{mode}.rds.
# Re-runnable on its own (e.g. to iterate on wording or figure styling)
# without recomputing the sweeps.

source(here::here("scripts/utils/common_settings.R"))
load_visualization_packages()
report_dir <- here::here("scripts/05_identification_with_optimization")
source(file.path(report_dir, "tau_star_report_figures.R"))
source(file.path(report_dir, "tau_star_report_text.R"))

cli_h1("tau* report")

temp_dir <- file.path(OUTPUT_TEMP_DIR, "identification_optimized")
paper_dir <- file.path(OUTPUT_PAPER_DIR, "identification")
dir.create(paper_dir, recursive = TRUE, showWarnings = FALSE)

modes <- c("maturities", "factors")
paths <- file.path(temp_dir, paste0("tau_star_comparison_", modes, ".rds"))
if (!all(file.exists(paths))) {
  stop(
    "Missing tau* results (",
    paste(basename(paths)[!file.exists(paths)], collapse = ", "),
    "); run tau_star_comparison.R first"
  )
}
results <- lapply(paths, readRDS)
names(results) <- modes

for (res in results) {
  mode <- res$mode
  write.csv(
    res$tau_stars[, c("gamma", "tau_star")],
    file.path(paper_dir, paste0("tau_star_comparison_", mode, ".csv")),
    row.names = FALSE
  )
  write.csv(
    published_sweep(res$sweep),
    file.path(paper_dir, paste0("tau_star_width_sweep_", mode, ".csv")),
    row.names = FALSE
  )
  ts_vfci <- res$tau_stars$tau_star[res$tau_stars$gamma == "VFCI (rank-1)"]
  save_plot_pair(
    plot_tau_star_overlay(res$sweep, res$tau_stars, mode),
    paper_dir, paste0("tau_star_comparison_", mode)
  )
  save_plot_pair(
    plot_vfci_blowup(res$sweep, ts_vfci, mode),
    paper_dir, paste0("tau_star_vfci_blowup_", mode)
  )
  cli_alert_success("Wrote {mode}-mode tau* artifacts to {.path {paper_dir}}")
}

summary_path <- file.path(paper_dir, "tau_star_summary.txt")
writeLines(build_tau_star_summary(results), summary_path)
cli_alert_success("Open-this-first summary: {.path {summary_path}}")
