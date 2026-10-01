# Deterministic moving-block draw orchestration. Indices are generated before
# any draw executes, so solver-side RNG consumption cannot perturb resampling.

paper_run_indexed_draws <- function(
  index_family,
  draw,
  cores = 1L,
  progress = NULL,
  is_failure = is.character
) {
  .paper_mbb_index_family_validate(index_family)
  execution <- .paper_mbb_execution_args(draw, cores, progress, is_failure)
  ambient <- .paper_mbb_rng_capture()
  on.exit(.paper_mbb_rng_restore(ambient), add = TRUE)
  .paper_mbb_rng_install(index_family$rng_kind, index_family$draw_rng_state)
  .paper_run_indexed_draws_core(index_family, execution)
}

# Standard console heartbeat for a paper_run_indexed_draws() progress callback:
# the %% report_every guard reports every draw when serial and every chunk
# when parallel, since draw_id already carries that meaning in both regimes.
paper_mbb_console_progress <- function(report_every, label) {
  function(draw_id, n_draws, started_at) {
    if (draw_id %% report_every == 0L) {
      cat(sprintf(
        "  %s draw %d of %d (%.1f min elapsed)\n",
        label, draw_id, n_draws,
        as.numeric(difftime(Sys.time(), started_at, units = "mins"))
      ))
    }
  }
}
