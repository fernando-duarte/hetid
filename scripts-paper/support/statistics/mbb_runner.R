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
    stopifnot(is.null(progress))
    draws <- parallel::mclapply(
      seq_len(n_draws),
      run_one,
      mc.cores = cores,
      mc.set.seed = TRUE
    )
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
    failed = failed
  )
}
