# Real unified primary callback: call count, branch consistency, and fork parity.

bsr_counted <- local({
  original <- estimate_set_id_system
  n_calls <- 0L
  assign("estimate_set_id_system", function(dat, spec) {
    n_calls <<- n_calls + 1L
    original(dat, spec)
  }, envir = .GlobalEnv)
  on.exit(assign("estimate_set_id_system", original, envir = .GlobalEnv), add = TRUE)
  value <- paper_run_indexed_draws(bsr_family, bsr_primary_callback, cores = 1L)
  list(value = value, n_calls = n_calls)
})
bsr_serial <- bsr_counted$value
check(
  "the real unified primary callback estimates exactly once per fixed index",
  identical(bsr_counted$n_calls, bsr_family$n_draws)
)
bsr_direct_mean <- lapply(bsr_family$indices, function(index) {
  set_id_boot_draw(lbd_dat[index, , drop = FALSE], bsr_mean_spec)
})
bsr_direct_volatility <- lapply(bsr_family$indices, function(index) {
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
  "real unified fixed-index collections agree with the individual branch evaluators",
  bootstrap_test_equal(
    bsr_unified_mean,
    set_id_boot_collect(bsr_direct_mean, bsr_collect_specs$mean)
  ) && bootstrap_test_equal(
    bsr_unified_volatility,
    logvar_set_boot_collect(bsr_direct_volatility, bsr_collect_specs$log_variance)
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
    bootstrap_test_equal(
      bsr_serial$draws,
      bsr_parallel$draws
    ) &&
      bootstrap_test_equal(
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
    bootstrap_test_equal(
      bsr_sensitivity_serial$draws,
      bsr_sensitivity_parallel$draws
    ) &&
      bootstrap_test_equal(
        bsr_sensitivity_serial$indices,
        bsr_sensitivity_parallel$indices
      )
  )
}
