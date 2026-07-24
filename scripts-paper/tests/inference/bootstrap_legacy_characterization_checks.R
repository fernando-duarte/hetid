# Fixed-index characterization of the two separate legacy callbacks. The
# fixture frame and real full-data volatility draw are already loaded.

blc_schedule <- paper_run_mbb_draws(
  n_draws = 2L,
  sample_size = nrow(lbd_dat),
  block_length = paper_mbb_block_len(nrow(lbd_dat)),
  seed = 20260722L,
  draw = function(index, draw_id) index
)
blc_indices <- blc_schedule$indices
check(
  "fixed callback indices are the runner's stored index objects",
  identical(blc_schedule$draws, blc_indices)
)

blc_reference_est <- estimate_set_id_system(lbd_dat, lbd_spec)
blc_mean_spec <- c(
  list(
    coefs = c(
      names(blc_reference_est$beta1r),
      rownames(blc_reference_est$beta2r)
    ),
    taus = lbd_spec$taus[-1L],
    tau_grid = seq(0, max(lbd_spec$taus), by = 0.05)
  ),
  lbd_spec[c(
    "gamma", "y1_col", "x_cols", "y2_cols", "z_col", "impose_null"
  )]
)
blc_dummy_builder <- function(...) list(characterization = TRUE)
blc_vol_spec <- lbd_spec
blc_vol_spec$builders <- stats::setNames(
  rep(list(blc_dummy_builder), length(blc_vol_spec$estimator_ids)),
  blc_vol_spec$estimator_ids
)

blc_expected <- lapply(blc_indices, function(index) {
  dat <- lbd_dat[index, , drop = FALSE]
  est <- estimate_set_id_system(dat, blc_mean_spec)
  list(
    mean = paper_legacy_mean_from_est(est, blc_mean_spec),
    volatility = paper_legacy_logvar_from_est(dat, est, blc_vol_spec)
  )
})
blc_separate <- paper_characterize_estimate_calls(function() {
  lapply(blc_indices, function(index) {
    dat <- lbd_dat[index, , drop = FALSE]
    list(
      mean = set_id_boot_draw(dat, blc_mean_spec),
      volatility = logvar_set_boot_draw(dat, blc_vol_spec)
    )
  })
})
check(
  "legacy wrappers match frozen from-estimate outputs on fixed indices",
  identical(blc_separate$value, blc_expected)
)
check(
  "two legacy callbacks estimate twice for every shared index",
  identical(blc_separate$n_calls, 2L * length(blc_indices))
)

blc_mean_est <- estimate_set_id_system(
  lbd_dat[blc_indices[[1L]], , drop = FALSE],
  blc_mean_spec
)
blc_vol_est <- estimate_set_id_system(
  lbd_dat[blc_indices[[1L]], , drop = FALSE],
  blc_vol_spec
)
check(
  "the legacy branch specifications produce identical shared estimates",
  identical(blc_mean_est, blc_vol_est)
)

blc_full_volatility <- paper_legacy_logvar_from_est(
  lbd_dat,
  blc_reference_est,
  lbd_spec
)
check(
  "the real legacy volatility callback matches the frozen decomposition",
  identical(lbd_draw, blc_full_volatility)
)

blc_primary <- list(ppml = list(tag = "ppml"), harvey = list(tag = "harvey"))
blc_public <- paper_legacy_logvar_result(
  primary = blc_primary,
  b_reps = 10L,
  block = 5L,
  seed = 99L,
  sens_block = 10L,
  sens_reps = 10L,
  inference_contract = list(alpha = 0.05),
  c_sim = list(ppml = 1, harvey = 1),
  tau0 = list(ppml = TRUE, harvey = TRUE),
  provenance = list(index_sha256 = "fixture")
)
check(
  "the volatility public object has the exact legacy field order",
  identical(
    names(blc_public),
    c(
      "ppml", "harvey", "b_reps", "block", "seed", "sens_block",
      "sens_reps", "inference_contract", "coverage_target", "c_sim",
      "tau0", "provenance"
    )
  )
)
check(
  "the volatility public assembler preserves primary objects exactly",
  identical(blc_public[names(blc_primary)], blc_primary)
)

rm(
  blc_schedule,
  blc_indices,
  blc_reference_est,
  blc_mean_spec,
  blc_dummy_builder,
  blc_vol_spec,
  blc_expected,
  blc_separate,
  blc_mean_est,
  blc_vol_est,
  blc_full_volatility,
  blc_primary,
  blc_public
)
