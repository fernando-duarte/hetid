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

paper_acm_daily_cache_path <- function() {
  file.path(
    tools::R_user_dir("hetid", "data"),
    "ACMTermPremium_replicated_daily_1m_120m.csv.gz"
  )
}

# Leave a known-good pinned asset in the package cache. A cached file that does
# not match the pin is refused rather than used or silently replaced: another
# session calling the package's own downloader resolves the release "latest"
# tag, so the file on disk can move to a newer vintage without anything here
# asking for it.
paper_ensure_pinned_acm_daily <- function() {
  path <- paper_acm_daily_cache_path()
  if (file.exists(path)) {
    found <- unname(tools::sha256sum(path))
    if (identical(found, acm_daily_sha256)) {
      return(invisible(path))
    }
    stop(sprintf(paste0(
      "the cached ACM daily asset does not match the pinned digest.\n",
      "  pinned (%s): %s\n",
      "  on disk           : %s\n",
      "  Delete %s to refetch the pinned release, or set acm_daily_source to ",
      "\"live\" in config/analysis.R to accept the upstream release."
    ), acm_daily_release, acm_daily_sha256, found, path), call. = FALSE)
  }
  url <- sprintf(
    "%s/%s/%s",
    "https://github.com/fernando-duarte/ACM_term_premium/releases/download",
    acm_daily_release, basename(path)
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::download.file(url, path, mode = "wb", quiet = TRUE)
  found <- unname(tools::sha256sum(path))
  if (!identical(found, acm_daily_sha256)) {
    unlink(path)
    stop(sprintf(paste0(
      "the ACM daily asset downloaded from %s failed digest verification ",
      "(got %s); the file was removed."
    ), acm_daily_release, found), call. = FALSE)
  }
  writeLines(
    c(
      paste0("sha256: ", found),
      paste0("source_url: ", url),
      paste0("retrieved: ", format(Sys.time(), "%Y-%m-%d"))
    ),
    paste0(path, ".meta")
  )
  invisible(path)
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
