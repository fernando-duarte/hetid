# Cores=1 vs cores=2 equality for the REAL vol set-endpoint draw callback
# (logvar_set_boot_draw), not the toy summation callback mbb_checks.R uses.
# Reuses the fixture frame/spec set_bootstrap_core_checks.R already built
# (lbd_dat, lbd_spec) so the full re-estimation chain (PPML + Harvey warm
# start) that paper_run_mbb_draws dispatches per draw in production runs
# identically under serial dispatch and the mclapply chunking cores=2 uses.

if (.Platform$OS.type == "windows") {
  skip("cores=1 vs cores=2 real-callback draws match", "no fork on windows")
} else {
  real_draw <- function(index, draw_id) {
    logvar_set_boot_draw(lbd_dat[index, , drop = FALSE], lbd_spec)
  }
  real_block <- paper_mbb_block_len(nrow(lbd_dat))
  real_run_serial <- paper_run_mbb_draws(
    n_draws = 4L, sample_size = nrow(lbd_dat), block_length = real_block,
    seed = 909L, cores = 1L, draw = real_draw
  )
  real_run_parallel <- paper_run_mbb_draws(
    n_draws = 4L, sample_size = nrow(lbd_dat), block_length = real_block,
    seed = 909L, cores = 2L, draw = real_draw
  )
  check(
    "cores=1 vs cores=2 agree on the real logvar_set_boot_draw callback",
    paper_scientific_equal(
      real_run_serial$draws,
      real_run_parallel$draws
    ) &&
      paper_scientific_equal(
        real_run_serial$indices,
        real_run_parallel$indices
      )
  )
}
