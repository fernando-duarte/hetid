# Deterministic moving-block draw orchestration. Indices are generated before
# any draw executes, so solver-side RNG consumption cannot perturb resampling.

paper_run_mbb_draws <- function(
  n_draws,
  sample_size,
  block_length,
  draw,
  seed,
  cores = 1L,
  progress = NULL,
  is_failure = is.character
) {
  stopifnot(
    length(n_draws) == 1L,
    is.finite(n_draws),
    n_draws >= 1L,
    n_draws == as.integer(n_draws),
    length(sample_size) == 1L,
    is.finite(sample_size),
    sample_size >= 1L,
    length(block_length) == 1L,
    is.finite(block_length),
    block_length >= 1L,
    length(cores) == 1L,
    is.finite(cores),
    cores >= 1L,
    cores == as.integer(cores),
    is.function(draw),
    is.null(progress) || is.function(progress),
    is.function(is_failure)
  )
  design <- .paper_mbb_design(
    as.integer(n_draws),
    as.integer(sample_size),
    as.integer(block_length)
  )
  cores <- as.integer(cores)
  protocol <- paper_mbb_protocol()
  ambient <- .paper_mbb_rng_capture()
  on.exit(.paper_mbb_rng_restore(ambient), add = TRUE)
  do.call(RNGkind, as.list(protocol$rng_kind))
  set.seed(seed)
  index_family <- .paper_mbb_index_family_build(
    design,
    seed,
    protocol$family_names[["compatibility"]]
  )
  .paper_run_indexed_draws_core(
    index_family,
    list(
      draw = draw,
      cores = cores,
      progress = progress,
      is_failure = is_failure
    )
  )
}

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

# Standard console heartbeat for a paper_run_mbb_draws() progress callback:
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
