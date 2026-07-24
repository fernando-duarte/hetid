# Execution over one already-created MBB index family; this owns no indices.

.paper_mbb_execution_args <- function(draw, cores, progress, is_failure) {
  stopifnot(
    length(cores) == 1L, is.finite(cores), cores >= 1L,
    cores == as.integer(cores), is.function(draw),
    is.null(progress) || is.function(progress), is.function(is_failure)
  )
  list(
    draw = draw,
    cores = as.integer(cores),
    progress = progress,
    is_failure = is_failure
  )
}

.paper_run_indexed_draws_core <- function(index_family, execution) {
  n_draws <- index_family$n_draws
  indices <- index_family$indices
  draw <- execution$draw
  cores <- execution$cores
  progress <- execution$progress
  is_failure <- execution$is_failure
  started_at <- Sys.time()
  run_one <- function(draw_id) {
    tryCatch(
      draw(indices[[draw_id]], draw_id),
      error = function(error) conditionMessage(error)
    )
  }
  if (cores > 1L) {
    chunk_size <- max(cores, ceiling(n_draws / 20L))
    chunks <- split(seq_len(n_draws), ceiling(seq_len(n_draws) / chunk_size))
    draws <- vector("list", n_draws)
    n_done <- 0L
    for (chunk in chunks) {
      draws[chunk] <- parallel::mclapply(
        chunk, run_one,
        mc.cores = cores, mc.set.seed = TRUE
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
    elapsed_minutes = as.numeric(difftime(Sys.time(), started_at, units = "mins")),
    n_failed = sum(failed),
    failed = failed,
    rng_kind = index_family$rng_kind
  )
}
