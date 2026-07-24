bootstrap_stage_provenance_axes <- function(stage_spec) {
  list(
    mean_coefs = stage_spec$mean$coefs,
    mean_taus = stage_spec$tau$display,
    volatility_coefs = stage_spec$log_variance$coefs,
    volatility_taus = stage_spec$tau$union,
    volatility_estimators = stage_spec$log_variance$estimator_ids
  )
}

bootstrap_stage_provenance_validate <- function(
  value, stage_spec, expected
) {
  design <- stage_spec$design
  hashes <- bootstrap_stage_spec_hashes(stage_spec)
  protocol <- paper_mbb_protocol()
  semantic <- list(
    resampler = protocol$resampler,
    sample_size = stage_spec$frame$sample_size,
    b_reps = design$primary$n_draws,
    block = design$primary$block_length,
    seed = as.integer(design$seed),
    rng_kind = protocol$rng_kind,
    block_rule = paper_mbb_block_rule(),
    sens_block = design$sensitivity$block_length,
    sens_reps = design$sensitivity$n_draws,
    axes = bootstrap_stage_provenance_axes(stage_spec),
    input_sha = hashes$input_sha,
    draw_spec_sha = hashes$draw_spec_sha,
    code_sha = paper_boot_code_sha(bootstrap_stage_code_manifest()),
    runtime_sha = paper_boot_runtime_sha(),
    cache_schema_version = BOOTSTRAP_STAGE_CACHE_SCHEMA
  )
  expected[names(semantic)] <- semantic
  bootstrap_stage_provenance_record_validate(
    value, BOOTSTRAP_STAGE_PROVENANCE_FIELDS, expected,
    BOOTSTRAP_STAGE_SHA_FIELDS, BOOTSTRAP_STAGE_SHA_PATTERN
  )
}

bootstrap_stage_provenance <- function(
  stage_spec, primary_family, sensitivity_family
) {
  families <- list(
    primary = primary_family,
    sensitivity = sensitivity_family
  )
  design <- stage_spec$design
  protocol <- paper_mbb_protocol()
  valid <- bootstrap_stage_family_pair_validate(
    families, protocol$family_names[c("primary", "sensitivity")],
    list(primary = design$primary, sensitivity = design$sensitivity),
    stage_spec$frame$sample_size, as.integer(design$seed),
    protocol$rng_kind, .paper_mbb_index_family_validate,
    BOOTSTRAP_STAGE_SHA_PATTERN
  )
  if (!isTRUE(valid)) stop("bootstrap stage: ", valid, call. = FALSE)
  primary <- primary_family
  sensitivity <- sensitivity_family
  hashes <- bootstrap_stage_spec_hashes(stage_spec)
  value <- list(
    resampler = protocol$resampler,
    sample_size = primary$sample_size,
    b_reps = primary$n_draws,
    block = primary$block_length,
    seed = primary$seed,
    rng_kind = unname(primary$rng_kind),
    block_rule = paper_mbb_block_rule(),
    index_sha256 = primary$index_sha256,
    post_index_rng_sha256 = primary$post_index_rng_sha256,
    sens_block = sensitivity$block_length,
    sens_reps = sensitivity$n_draws,
    sens_index_sha256 = sensitivity$index_sha256,
    sens_post_index_rng_sha256 =
      sensitivity$post_index_rng_sha256,
    axes = bootstrap_stage_provenance_axes(stage_spec),
    input_sha = hashes$input_sha,
    draw_spec_sha = hashes$draw_spec_sha,
    code_sha = paper_boot_code_sha(bootstrap_stage_code_manifest()),
    runtime_sha = paper_boot_runtime_sha(),
    cache_schema_version = BOOTSTRAP_STAGE_CACHE_SCHEMA
  )
  stopifnot(isTRUE(
    bootstrap_stage_provenance_validate(value, stage_spec, value)
  ))
  value
}
