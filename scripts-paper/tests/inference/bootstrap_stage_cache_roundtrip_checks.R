# Real B=2 candidate, transactional promotion, and zero-callback reuse.

bsr_roundtrip_spec <- bsr_stage_spec
bsr_roundtrip_spec$design <- list(
  seed = bsr_family$seed,
  failure_control = PAPER_INFERENCE_SEARCH_CONTROL$bootstrap,
  primary = list(
    family = bsr_family$family,
    n_draws = bsr_family$n_draws,
    block_length = bsr_family$block_length
  ),
  sensitivity = list(
    family = bsr_sensitivity_family$family,
    n_draws = bsr_sensitivity_family$n_draws,
    block_length = bsr_sensitivity_family$block_length
  )
)
bsr_roundtrip_provenance <- function() {
  bootstrap_stage_provenance(
    bsr_roundtrip_spec,
    bsr_family,
    bsr_sensitivity_family
  )
}
bsr_roundtrip_path <- tempfile(
  "bootstrap-stage-roundtrip-",
  fileext = ".rds"
)
bsr_roundtrip_calls <- 0L
bsr_roundtrip_rerun <- bootstrap_stage_cached_or_run(
  bsr_roundtrip_path,
  "rerun",
  bsr_roundtrip_spec,
  bsr_roundtrip_provenance,
  function() {
    bsr_roundtrip_calls <<- bsr_roundtrip_calls + 1L
    bootstrap_stage_candidate(
      bsr_roundtrip_spec,
      bsr_family,
      bsr_sensitivity_family,
      1L
    )
  }
)
bsr_roundtrip_reuse <- bootstrap_stage_cached_or_run(
  bsr_roundtrip_path,
  "reuse",
  bsr_roundtrip_spec,
  bsr_roundtrip_provenance,
  function() stop("round-trip reuse executed a scientific callback")
)
check(
  "real B=2 cache rerun promotes once and reuse executes no callback",
  identical(bsr_roundtrip_calls, 1L) &&
    identical(bsr_roundtrip_rerun$source, "rerun") &&
    identical(bsr_roundtrip_reuse$source, "reuse") &&
    paper_scientific_equal(
      bsr_roundtrip_rerun$stage,
      bsr_roundtrip_reuse$stage
    )
)
unlink(bsr_roundtrip_path)
rm(
  bsr_roundtrip_spec,
  bsr_roundtrip_provenance,
  bsr_roundtrip_path,
  bsr_roundtrip_calls,
  bsr_roundtrip_rerun,
  bsr_roundtrip_reuse
)
