# Cores=1 vs cores=2 equality for the REAL vol set-endpoint draw callback
# (logvar_set_boot_draw), not the toy summation callback mbb_checks.R uses.
# Reuses the fixture frame/spec set_bootstrap_draw_checks.R already built
# (lbd_dat, lbd_spec) so the full re-estimation chain (PPML + Harvey warm
# start) runs identically under indexed serial dispatch and the mclapply
# chunking cores=2 uses.

if (.Platform$OS.type == "windows") {
  skip("cores=1 vs cores=2 real-callback draws match", "no fork on windows")
} else {
  real_draw <- function(index, draw_id) {
    logvar_set_boot_draw(lbd_dat[index, , drop = FALSE], lbd_spec)
  }
  real_block <- paper_mbb_block_len(nrow(lbd_dat))
  real_family <- paper_mbb_index_family(
    4L, nrow(lbd_dat), real_block, 909L, "primary"
  )
  real_run_serial <- paper_run_indexed_draws(real_family, real_draw, cores = 1L)
  real_run_parallel <- paper_run_indexed_draws(real_family, real_draw, cores = 2L)
  check(
    "cores=1 vs cores=2 agree on the real logvar_set_boot_draw callback",
    bootstrap_test_equal(
      real_run_serial$draws,
      real_run_parallel$draws
    ) &&
      bootstrap_test_equal(
        real_run_serial$indices,
        real_run_parallel$indices
      )
  )
}
