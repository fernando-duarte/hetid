paper_source_once(paper_path(
  "support", "inference_post", "endpoint_target_cells.R"
))

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
    slots = slots,
    # derived from the same axis the collector reads, so a consumer of the point
    # fields never has to name the slot itself
    tau0_slot = bootstrap_stage_logvar_tau0_slot(stage_spec$tau$union)
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

# The volatility panel's display-tau cells, through the same shared builder the
# mean panel uses, so both panels quantile one reference distribution. Iterates
# layout$slots, which skips the tau = 0 slot: that slot is a point evaluation and
# its statistic is built by logvar_boot_point_t, never here.
bootstrap_stage_envelopes <- function(
  collected, full, estimator_ids, layout, alpha
) {
  envelopes <- lapply(estimator_ids, function(estimator_id) {
    result <- lapply(seq_along(layout$taus), function(index) {
      slot <- layout$slots[[index]]
      endpoint_target_table(
        collected[[estimator_id]][[slot]],
        full[[estimator_id]][[slot]],
        alpha = alpha
      )
    })
    names(result) <- layout$keys
    result
  })
  names(envelopes) <- estimator_ids
  envelopes
}
