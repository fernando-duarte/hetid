# Compatibility API over the manifest-owned conditional lifecycle.

logvar_egarch_dynamic_artifacts <- function() {
  artifact_paths_by_status("conditional_egarch")
}

logvar_egarch_cleanup <- function() {
  cleanup_conditional_artifacts("conditional_egarch")
}
