# Clears cached, gate/state, diagnostic, and (by default) tracked pipeline
# output so the next run_pipeline.R invocation has no memory of a prior run.
# Run from the package root: Rscript scripts-paper/reset_pipeline_state.R
# [--keep-tracked]

source(normalizePath(
  file.path("scripts-paper", "config", "paths.R"),
  mustWork = TRUE
))
paper_source_once(paper_path("config", "artifacts.R"))

keep_tracked <- "--keep-tracked" %in% commandArgs(trailingOnly = TRUE)
audits <- reset_pipeline_state(include_tracked = !keep_tracked)

.report_audit <- function(name, audit) {
  if (is.list(audit) && !is.null(audit$n_deleted)) {
    cat(sprintf(
      "%s: %d removed, all_absent=%s\n", name, audit$n_deleted, audit$all_absent
    ))
  } else {
    cat(sprintf("%s: %d removed\n", name, length(audit)))
  }
}

invisible(Map(.report_audit, names(audits), audits))
