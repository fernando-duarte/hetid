bootstrap_stage_mean_provenance <- function(stage) {
  stage$provenance[c(
    "resampler", "sample_size", "b_reps", "block", "seed",
    "rng_kind", "block_rule", "index_sha256"
  )]
}

bootstrap_stage_logvar_provenance <- function(stage) {
  stage$provenance[c(
    "resampler", "sample_size", "b_reps", "block", "seed",
    "rng_kind", "block_rule", "index_sha256",
    "sens_block", "sens_reps"
  )]
}

bootstrap_stage_display_layout <- function(stage_spec) {
  taus <- stage_spec$tau$display
  slots <- seq_along(taus) + 1L
  stopifnot(
    identical(stage_spec$tau$union, c(0, taus)),
    identical(stage_spec$tau$union[slots], taus)
  )
  list(
    taus = taus,
    keys = vapply(taus, paper_tau_key, character(1)),
    slots = slots
  )
}

bootstrap_stage_anchor_frames <- function(anchor, logvar_spec) {
  frames <- lapply(logvar_spec$estimator_ids, function(estimator_id) {
    lapply(anchor[[estimator_id]], function(record) {
      data.frame(
        coef = logvar_spec$coefs,
        set_lower = record$lower,
        set_upper = record$upper,
        lower_status = record$lower_status,
        upper_status = record$upper_status,
        stringsAsFactors = FALSE
      )
    })
  })
  names(frames) <- logvar_spec$estimator_ids
  frames
}

bootstrap_stage_envelopes <- function(
  collected, full, estimator_ids, layout, alpha, stability
) {
  envelopes <- lapply(estimator_ids, function(estimator_id) {
    result <- lapply(seq_along(layout$taus), function(index) {
      slot <- layout$slots[[index]]
      logvar_endpoint_envelope(
        collected[[estimator_id]][[slot]],
        full[[estimator_id]][[slot]],
        alpha = alpha,
        stability = stability
      )
    })
    names(result) <- layout$keys
    result
  })
  names(envelopes) <- estimator_ids
  envelopes
}
