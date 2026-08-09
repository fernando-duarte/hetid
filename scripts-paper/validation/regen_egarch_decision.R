#!/usr/bin/env Rscript
# Regenerate the printed fields for scripts-paper/config/decisions/egarch.R
# after switching PAPER_ANALYSIS_CONTRACT$input$instrument$active to a
# different choice. Every non-benchmark instrument produces a different
# (still non-rejecting, in every case tested so far) residual-dynamics gate,
# and scripts-paper/log_variance/extensions/egarch/route_core.R refuses to
# route past a gate whose recomputed scientific hash does not match the
# committed decision record -- by design, so a stale approval can never be
# silently reused under different data. Lives under validation/, not bare
# under scripts-paper/, because the source-topology gate
# (tests/support/topology_reference_checks.R) allows exactly two top-level
# entrypoints directly under scripts-paper/ (run_pipeline.R,
# reset_pipeline_state.R) and exempts validation/ entirely, matching
# validation/compare_output_tables.R's existing placement.
#
# Usage: run the pipeline once first with the instrument you want to
# switch to (any HETID_BOOT_REPS; this only needs the dynamics-gate RDS,
# written well before the bootstrap stage) IMMEDIATELY before running this
# tool -- the gate RDS carries no instrument identifier of its own, so a
# stale file from an earlier, different-instrument run would be silently
# misattributed to whatever is currently active. The freshness check below
# is a cheap guard against exactly that mistake, not a full binding.
#
#   HETID_BOOT_REPS=8 HETID_BOOT_CORES=1 HETID_ALLOW_DRAFT_RUN=1 \
#     Rscript scripts-paper/run_pipeline.R  # fails at the EGARCH gate; fine
#   Rscript scripts-paper/validation/regen_egarch_decision.R

source(normalizePath(
  file.path("scripts-paper", "config", "paths.R"),
  mustWork = TRUE
))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("config", "analysis.R"))
paper_source_once(paper_path(
  "log_variance", "extensions", "egarch", "decision_core.R"
))
paper_source_once(paper_path("config", "decisions", "egarch.R"))

gate_rds <- artifact_path("dynamics_gate")
if (!file.exists(gate_rds)) {
  stop(
    "No dynamics-gate record at ", gate_rds, ". Run scripts-paper/run_",
    "pipeline.R at least once first with the instrument you want to ",
    "switch to (it will likely fail at the EGARCH gate -- that is ",
    "expected and this tool is how you fix it).",
    call. = FALSE
  )
}
age_minutes <- as.numeric(
  difftime(Sys.time(), file.info(gate_rds)$mtime, units = "mins")
)
if (age_minutes > 30) {
  warning(sprintf(
    paste(
      "The dynamics-gate record is %.0f minutes old. If you switched the",
      "active instrument since that run, re-run the pipeline first --",
      "this file does not identify which instrument produced it."
    ),
    age_minutes
  ), call. = FALSE)
}
gate <- readRDS(gate_rds)
decided_at <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

new_rec <- logvar_egarch_decision_default(
  gate, decided_at,
  logvar_egarch_estimand_prompt, logvar_egarch_dependency_prompt
)

cat("Active instrument:", paste(z_col, collapse = ", "), "\n")
cat("Gate record age:", sprintf("%.1f", age_minutes), "minutes\n\n")
cat("Paste into scripts-paper/config/decisions/egarch.R:\n\n")
cat('  gate_science_sha256 =\n    "', new_rec$gate_science_sha256, '",\n', sep = "")
cat("  gate_record_path = LOGVAR_EGARCH_GATE_RECORD_PATH,\n")
cat('  sample_id = "', new_rec$sample_id, '",\n', sep = "")
cat("  gate_lag = ", new_rec$gate_lag, "L,\n", sep = "")
cat("  gate_alpha = ", new_rec$gate_alpha, ",\n", sep = "")
cat("  gate_q = ", sprintf("%a", new_rec$gate_q), ",\n", sep = "")
cat("  gate_p = ", sprintf("%a", new_rec$gate_p), ",\n", sep = "")
cat('  gate_verdict = "', new_rec$gate_verdict, '",\n\n', sep = "")
cat(
  "Also update decided_at_utc to \"", decided_at, "\" and the header ",
  "comment's lag-4 Ljung-Box p-value (", sprintf("%.3f", new_rec$gate_p),
  ") and instrument description.\n",
  sep = ""
)
