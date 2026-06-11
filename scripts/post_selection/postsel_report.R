# Human-facing report for the post-selection split study and its
# simulation gate. Reads temp/identification_postsel/split_study.rds
# (required) plus sim_results.rds (preferred) or
# sim_results_quick.rds (fallback; HETID_POSTSEL_SIM_SOURCE
# overrides). Artifact names carry an honesty suffix: canonical only
# when a full simulation was read; _quick-sim / _no-sim otherwise, so
# lesser runs never clobber a validated summary. A failing FULL
# simulation verdict fails this process (nonzero exit) AFTER all
# artifacts are written: report, then stop.

source(here::here("scripts/utils/common_settings.R"))
source(here::here("scripts/utils/postsel_split_utils.R"))
report_dir <- here::here("scripts/post_selection")
source(file.path(report_dir, "postsel_report_stats.R"))
source(file.path(report_dir, "postsel_report_text.R"))
source(file.path(report_dir, "postsel_report_caveats.R"))

cli_h1("Post-selection split report")

temp_dir <- file.path(OUTPUT_TEMP_DIR, "identification_postsel")
paper_dir <- file.path(OUTPUT_PAPER_DIR, "identification")
dir.create(paper_dir, recursive = TRUE, showWarnings = FALSE)

study_path <- file.path(temp_dir, "split_study.rds")
if (!file.exists(study_path)) {
  stop(
    "No split study found; run ",
    "scripts/post_selection/run_split_study.R first"
  )
}
study <- readRDS(study_path)

sim_src <- Sys.getenv("HETID_POSTSEL_SIM_SOURCE")
if (!nzchar(sim_src)) {
  cands <- file.path(
    temp_dir, c("sim_results.rds", "sim_results_quick.rds")
  )
  sim_src <- c(cands[file.exists(cands)], NA_character_)[1]
}
sim <- NULL
if (!is.na(sim_src) && file.exists(sim_src)) {
  sim <- readRDS(sim_src)
  cli_alert_info("Simulation source: {.path {sim_src}}")
} else {
  cli_alert_warning("No simulation artifacts found")
}

acceptance <- NULL
if (!is.null(sim) && !isTRUE(sim$settings$quick)) {
  acceptance <- sim_acceptance(aggregate_sim_coverage(sim$results))
}

suffix <- if (is.null(sim)) {
  "_no-sim"
} else if (isTRUE(sim$settings$quick)) {
  "_quick-sim"
} else {
  ""
}

lines <- build_postsel_summary_lines(study, sim, acceptance)
summary_path <- file.path(
  paper_dir, paste0("post_selection_split_summary", suffix, ".txt")
)
writeLines(lines, summary_path)

if (!is.null(sim)) {
  write.csv(
    aggregate_sim_coverage(sim$results),
    file.path(temp_dir, paste0("sim_coverage", suffix, ".csv")),
    row.names = FALSE
  )
}
saveRDS(
  list(suffix = suffix, acceptance = acceptance),
  file.path(temp_dir, paste0("postsel_report", suffix, ".rds"))
)

cat(lines, sep = "\n")
cli_alert_success("Summary written to {.path {summary_path}}")

# Report-then-stop: every artifact above is already written; a
# failing FULL-simulation verdict now fails the process so
# orchestration cannot silently proceed (decision D9/D11).
status <- postsel_exit_status(acceptance)
if (status != 0L) {
  cli_alert_danger(
    "Simulation acceptance FAILED; see the verdict in the summary"
  )
  quit(save = "no", status = status)
}
