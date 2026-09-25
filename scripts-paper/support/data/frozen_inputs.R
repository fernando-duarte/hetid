# Preconditions for the pipeline's frozen external inputs, checked in one place.
# run_pipeline.R calls the entry point ahead of the conditional cleanup, so a
# missing snapshot or a cache that no longer matches its pin stops the run while
# the output tree is still intact. Checking at the point of use instead would
# halt after the cleanup had already deleted the conditional artifacts, leaving
# a tree that needs restoring from git before the next attempt.
# Definitions only; sourced by run_pipeline.R and by the two builders that read
# these inputs.

paper_consumption_snapshot <- function() {
  paper_path("data", "pcecc96.csv")
}

# Release selection remains paper policy; storage and verification belong to hetid.
paper_ensure_pinned_acm_daily <- function() {
  hetid::download_term_premia(
    frequency = "daily", release = acm_daily_release,
    expected_sha256 = acm_daily_sha256, quiet = TRUE
  )
}

paper_check_source_switch <- function(value, name) {
  if (!value %in% c("frozen", "live")) {
    stop(sprintf(
      "%s must be \"frozen\" or \"live\", not \"%s\".", name, value
    ), call. = FALSE)
  }
  invisible(TRUE)
}

paper_verify_frozen_inputs <- function() {
  paper_check_source_switch(fred_source, "fred_source")
  paper_check_source_switch(acm_daily_source, "acm_daily_source")
  snapshot <- paper_consumption_snapshot()
  if (identical(fred_source, "frozen") && !file.exists(snapshot)) {
    stop(sprintf(paste0(
      "the frozen consumption snapshot is missing at %s.\n",
      "  Set fred_source <- \"live\" in config/analysis.R to download the ",
      "current vintage and rewrite it."
    ), snapshot), call. = FALSE)
  }
  if (identical(acm_daily_source, "frozen")) {
    paper_ensure_pinned_acm_daily()
  }
  invisible(TRUE)
}
