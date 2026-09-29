# Helper function: the structural-inference result, reused when its identity holds
# reuse mode returns the one object read from the cache and runs no draws; rerun
# mode, or a missing, unreadable, malformed or stale cache, fits the reference
# column and every draw and replaces the cache. The cache is written before any
# publication gate is read, so a run whose gates fail keeps its completed draws
# and diagnostics, and the renderer is the one that refuses to publish. With
# neither settings nor prepared inputs, the default block length follows the
# realised mean sample, as in every other paper bootstrap; a caller's settings
# keep their own block length
paper_structural_inference_run <- function(
  settings = NULL, workers = boot_cores,
  path = artifact_path("structural_inference_draws"), mode = PAPER_BOOT_MODE,
  prepared = NULL
) {
  stopifnot(
    is.character(mode), length(mode) == 1L, mode %in% c("reuse", "rerun"),
    is.character(path), length(path) == 1L, nzchar(path)
  )
  if (is.null(settings) && is.null(prepared)) {
    # the provisional length of 1 only lets a short sample be built
    prepared <- paper_structural_inference_prepare(
      structural_inference_settings(block_length = 1L)
    )
    settings <- structural_inference_settings(
      block_length = paper_mbb_block_len(prepared$n_obs)
    )
    stopifnot(settings$block_length <= prepared$n_obs)
    prepared$settings <- settings
  }
  if (is.null(settings)) settings <- prepared$settings
  if (is.null(prepared)) prepared <- paper_structural_inference_prepare(settings)
  identity <- structural_inference_identity(prepared, settings)
  if (mode == "reuse") {
    cached <- structural_inference_cache_read(path, identity)
    if (!is.null(cached)) {
      return(cached)
    }
  } else {
    message("structural inference: rerun mode, ignoring any cache at ", path)
  }
  reference <- structural_inference_reference(prepared, settings)
  bootstrap <- structural_inference_bootstrap(prepared, settings, workers = workers)
  result <- structural_inference_cache_payload(prepared, reference, bootstrap, identity)
  rm(reference, bootstrap)
  structural_inference_cache_write(result, path)
  result
}
