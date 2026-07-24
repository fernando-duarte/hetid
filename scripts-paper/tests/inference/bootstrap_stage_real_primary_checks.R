# Real unified primary callback: call count, legacy parity, and fork parity.

bsr_counted <- paper_characterize_estimate_calls(function() {
  paper_run_indexed_draws(
    bsr_family,
    bsr_primary_callback,
    cores = 1L
  )
})
bsr_serial <- bsr_counted$value
check(
  "the real unified primary callback estimates exactly once per fixed index",
  identical(bsr_counted$n_calls, bsr_family$n_draws)
)
bsr_legacy_mean <- lapply(bsr_family$indices, function(index) {
  set_id_boot_draw(lbd_dat[index, , drop = FALSE], bsr_mean_spec)
})
bsr_legacy_volatility <- lapply(bsr_family$indices, function(index) {
  logvar_set_boot_draw(lbd_dat[index, , drop = FALSE], bsr_logvar_spec)
})
bsr_unified_mean <- set_id_boot_collect(
  bootstrap_stage_project_raw(bsr_serial$draws, "mean"),
  bsr_collect_specs$mean
)
bsr_unified_volatility <- logvar_set_boot_collect(
  bootstrap_stage_project_raw(bsr_serial$draws, "volatility"),
  bsr_collect_specs$log_variance
)
check(
  "real unified fixed-index collections equal both legacy compatibility wrappers",
  paper_scientific_equal(
    bsr_unified_mean,
    set_id_boot_collect(bsr_legacy_mean, bsr_collect_specs$mean)
  ) && paper_scientific_equal(
    bsr_unified_volatility,
    logvar_set_boot_collect(bsr_legacy_volatility, bsr_collect_specs$log_variance)
  )
)
if (.Platform$OS.type == "windows") {
  skip("real unified primary callback serial and two-core draws match", "no fork")
  skip("real sensitivity callback serial and two-core draws match", "no fork")
} else {
  bsr_parallel <- paper_run_indexed_draws(
    bsr_family,
    bsr_primary_callback,
    cores = 2L
  )
  check(
    "real unified primary callback agrees under serial and two-core execution",
    paper_scientific_equal(
      bsr_serial$draws,
      bsr_parallel$draws
    ) &&
      paper_scientific_equal(
        bsr_serial$indices,
        bsr_parallel$indices
      )
  )
  bsr_sensitivity_serial <- paper_run_indexed_draws(
    bsr_sensitivity_family,
    bsr_sensitivity_callback,
    cores = 1L
  )
  bsr_sensitivity_parallel <- paper_run_indexed_draws(
    bsr_sensitivity_family,
    bsr_sensitivity_callback,
    cores = 2L
  )
  check(
    "real sensitivity callback agrees under serial and two-core execution",
    paper_scientific_equal(
      bsr_sensitivity_serial$draws,
      bsr_sensitivity_parallel$draws
    ) &&
      paper_scientific_equal(
        bsr_sensitivity_serial$indices,
        bsr_sensitivity_parallel$indices
      )
  )
}
