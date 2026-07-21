# Deterministic moving-block draw orchestration. Indices are generated before
# any draw executes, so solver-side RNG consumption cannot perturb resampling.

paper_run_mbb_draws <- function(
  n_draws,
  sample_size,
  block_length,
  draw,
  seed,
  truncation = sample_size,
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
    length(truncation) == 1L,
    is.finite(truncation),
    truncation >= 1L,
    truncation <= sample_size,
    length(cores) == 1L,
    is.finite(cores),
    cores >= 1L,
    cores == as.integer(cores),
    is.function(draw),
    is.null(progress) || is.function(progress),
    is.function(is_failure)
  )
  n_draws <- as.integer(n_draws)
  sample_size <- as.integer(sample_size)
  block_length <- as.integer(block_length)
  truncation <- as.integer(truncation)
  cores <- as.integer(cores)

  # the runner owns the generator so every bootstrap draws indices under one
  # documented RNG regardless of ambient state
  old_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE)
  RNGkind("Mersenne-Twister")
  # the kind the indices were drawn under, recorded before the exit restore
  rng_kind <- RNGkind()
  set.seed(seed)
  indices <- lapply(seq_len(n_draws), function(draw_id) {
    mbb_index(sample_size, block_length)[seq_len(truncation)]
  })
  started_at <- Sys.time()
  run_one <- function(draw_id) {
    tryCatch(
      draw(indices[[draw_id]], draw_id),
      error = function(error) conditionMessage(error)
    )
  }
  if (cores > 1L) {
    # dispatch in chunks (about 20 heartbeats per run) so progress can
    # report under parallelism too; results match a single mclapply call
    # because the indices above are already fixed before any chunk runs
    chunk_size <- max(cores, ceiling(n_draws / 20L))
    chunks <- split(seq_len(n_draws), ceiling(seq_len(n_draws) / chunk_size))
    draws <- vector("list", n_draws)
    n_done <- 0L
    for (chunk in chunks) {
      draws[chunk] <- parallel::mclapply(
        chunk,
        run_one,
        mc.cores = cores,
        mc.set.seed = TRUE
      )
      n_done <- n_done + length(chunk)
      if (!is.null(progress)) {
        progress(n_done, n_draws, started_at)
      }
    }
  } else {
    draws <- vector("list", n_draws)
    for (draw_id in seq_len(n_draws)) {
      draws[[draw_id]] <- run_one(draw_id)
      if (!is.null(progress)) {
        progress(draw_id, n_draws, started_at)
      }
    }
  }
  failed <- vapply(draws, is_failure, logical(1))
  list(
    draws = draws,
    indices = indices,
    started_at = started_at,
    elapsed_minutes = as.numeric(
      difftime(Sys.time(), started_at, units = "mins")
    ),
    n_failed = sum(failed),
    failed = failed,
    rng_kind = rng_kind
  )
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
