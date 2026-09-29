# Helper function: resample mean rows in circular blocks and calibrate point and range statistics
structural_inference_bootstrap <- function(prepared, settings, full = NULL, workers = 1L) {
  stopifnot(
    identical(settings, prepared$settings), is.numeric(workers),
    length(workers) == 1L, is.finite(workers), workers >= 1, workers == floor(workers)
  )
  workers <- as.integer(workers)
  # forked workers, so more than one needs a unix platform
  stopifnot(workers == 1L || .Platform$OS.type == "unix")
  if (is.null(full)) full <- structural_inference_fit(prepared, settings)
  stopifnot(identical(full$frame$coef, structural_inference_axis(prepared, settings)$coef))
  # one index vector per draw, shared by both panels and every tau. the kind is
  # fixed so the seed reproduces, and the caller's RNG state, an absent seed
  # included, comes back on exit. withr::with_seed would leave the kind changed
  # when the caller had no seed, and restoring the kind recreates a seed, so the
  # seed is removed after it
  rng_kind <- paper_mbb_protocol()$rng_kind
  caller_rng <- .paper_mbb_rng_capture()
  on.exit(.paper_mbb_rng_restore(caller_rng), add = TRUE)
  do.call(RNGkind, as.list(rng_kind))
  set.seed(settings$seed)
  indices <- hetid::circular_mbb_indices(
    prepared$n_obs, settings$block_length,
    settings$n_draws
  )
  names(indices) <- sprintf("draw_%04d", seq_along(indices))
  # every draw is fitted in bounded batches under its global draw id, then the
  # finished frames are replayed through hetid's runner, which validates and
  # collects them in draw order. the indices are drawn above, the fit fixes its
  # RNG kind and hetid seeds its direction search itself and restores the stream,
  # so a forked worker returns the same frame the serial loop would
  fit_draw <- function(draw_id) {
    evaluated <- structural_inference_fit(prepared, settings, indices[[draw_id]], draw_id,
      retain = FALSE
    )
    attr(evaluated$frame, "diagnostics") <- evaluated$diagnostics
    evaluated$frame
  }
  n_draws <- length(indices)
  batch_size <- 10L * workers
  starts <- seq(1L, n_draws, by = batch_size)
  # each finished batch waits in an uncompressed temporary file, so the parent
  # forks with at most one batch of diagnostics in memory rather than every draw's
  # PPML candidate fits, about 1 MB a draw. the replay reads them back whole
  spool <- tempfile("structural_inference_bootstrap_")
  dir.create(spool)
  on.exit(unlink(spool, recursive = TRUE), add = TRUE)
  batch_file <- file.path(spool, sprintf("batch_%05d.rds", seq_along(starts)))
  for (b in seq_along(starts)) {
    batch <- starts[b]:min(starts[b] + batch_size - 1L, n_draws)
    if (workers == 1L) {
      # an unexpected error leaves at once, with no further fits
      fitted <- lapply(batch, fit_draw)
    } else {
      fitted <- parallel::mclapply(batch, fit_draw,
        mc.cores = workers,
        mc.preschedule = FALSE, mc.set.seed = FALSE
      )
      # a worker's unexpected error is rethrown unchanged, a worker that died
      # delivers nothing, and both stop the run rather than count as failed draws
      for (k in seq_along(batch)) {
        if (inherits(fitted[[k]], "try-error")) stop(attr(fitted[[k]], "condition"))
        if (is.null(fitted[[k]])) stop("bootstrap draw ", batch[k], " returned no result")
      }
    }
    saveRDS(fitted, batch_file[b],
      compress = FALSE,
      version = PAPER_SERIALIZATION_CONTROL$rds_version
    )
    rm(fitted)
    invisible(gc())
    message(sprintf(
      "structural inference bootstrap: %d of %d draws fitted", max(batch),
      n_draws
    ))
  }
  # the runner visits draws in order, so one batch is read at a time
  loaded <- list(batch = 0L, frames = NULL)
  replay <- function(index, draw_id) {
    b <- findInterval(draw_id, starts)
    if (loaded$batch != b) loaded <<- list(batch = b, frames = readRDS(batch_file[b]))
    loaded$frames[[draw_id - starts[b] + 1L]]
  }
  draws <- hetid::bootstrap_endpoint_draws(full$frame, indices, replay)
  if (any(draws$callback_failed)) {
    first_failed <- which(draws$callback_failed)[1]
    stop(
      "replaying bootstrap draw ", first_failed, " failed: ",
      draws$errors[[first_failed]]$message
    )
  }
  calibrated <- structural_inference_calibrate(full, draws, settings)
  c(
    list(full = full, draws = draws), calibrated,
    list(metadata = list(
      settings = settings, rng_kind = rng_kind,
      package_version = as.character(utils::packageVersion("hetid")),
      package_sha = utils::packageDescription("hetid")$RemoteSha,
      sample = prepared$sample,
      resampling = paste(
        "all mean-row tuples in circular blocks; variance-complete rows",
        "selected within each draw; fixed PC rotations"
      ),
      variance_centering = "recomputed within each repetition",
      numerical_target = "attained mean ranges and sampled PPML coefficient ranges"
    ))
  )
}
