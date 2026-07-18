# Compatibility API over the manifest-owned conditional lifecycle.

logvar_egarch_dynamic_artifacts <- function() {
  artifact_paths_by_status(
    PAPER_ARTIFACT_STATUS$conditional_egarch
  )
}

logvar_egarch_cleanup <- function() {
  cleanup_conditional_artifacts(
    PAPER_ARTIFACT_STATUS$conditional_egarch
  )
}
