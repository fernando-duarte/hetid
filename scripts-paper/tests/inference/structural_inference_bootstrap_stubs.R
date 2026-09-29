# Stand-ins for the structural-inference inputs and the per-draw fit, so the
# bootstrap's batching, RNG handling and publication gates run through hetid's
# real endpoint runner and calibration without the numerical system.

fail <- function(...) stop(..., call. = FALSE)

# Helper function: the settings fields the bootstrap and calibration read
stub_settings <- function(n_draws = 4L) {
  list(
    n_draws = as.integer(n_draws), seed = 20260708L, block_length = 10L,
    taus = c(0.05, 0.10, 0.20), alpha = 0.10,
    min_reps = as.integer(ceiling(0.5 * n_draws)), stability = 0.85,
    maximum_failed_share = 0.25, interval_target = "pointwise",
    interval_control = list(tolerance = 1e-4, max_evals = .Machine$integer.max)
  )
}

# Helper function: a prepared sample carrying only its size and term names
stub_prepared <- function(settings) {
  list(
    settings = settings, n_obs = 80L, sample = list(n_mean = 80L),
    terms = list(
      mean = c("(Intercept)", paste0("expected_sdf_pc", 1:3), paste0("sdf_news_pc", 1:3)),
      variance = c("(Intercept)", paste0("pc", 1:4))
    )
  )
}

# Helper function: the coefficient axis, one row per panel, term and tau
structural_inference_axis <- function(prepared, settings) {
  rows <- lapply(names(prepared$terms), function(panel) {
    do.call(rbind, lapply(c(0, settings$taus), function(tau) {
      term <- prepared$terms[[panel]]
      data.frame(
        coef = paste(panel, term, sprintf("tau=%.2f", tau), sep = "|"),
        panel = panel, term = term, tau = tau, lower = NA_real_, upper = NA_real_,
        lower_status = "failed", upper_status = "failed",
        n_attempted = NA_integer_, n_failed = NA_integer_, stringsAsFactors = FALSE
      )
    }))
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Helper function: swap the fit the bootstrap calls, restoring it on exit
with_fit <- function(replacement, expression) {
  original <- get0("structural_inference_fit", envir = globalenv(), inherits = FALSE)
  on.exit(assign("structural_inference_fit", original, envir = globalenv()), add = TRUE)
  assign("structural_inference_fit", replacement, envir = globalenv())
  force(expression)
}

# Helper function: list the bootstrap's temporary batch directories still on disk
spools <- function() list.files(tempdir(), pattern = "^structural_inference_bootstrap_")

# Helper function: capture the RNG kind and seed, recording an absent seed as NULL
rng_state <- function() {
  list(kind = RNGkind(), seed = get0(".Random.seed", envir = globalenv(), inherits = FALSE))
}

# every endpoint of a draw carries a fingerprint of the index it was given, and
# alter() rewrites chosen cells of chosen draws. like the real fit, the stand-in
# reads the stream under the protocol kind and hands the caller its state back,
# so its diagnostics record which stream each draw saw. each call records how
# many spooled batch files it could see
calls <- new.env()
alter <- function(frame, draw_id) frame
fake_fit <- function(prepared, settings, index = NULL, draw_id = 0L, retain = TRUE) {
  calls$index <- c(calls$index, list(index))
  calls$spooled <- c(calls$spooled, length(list.files(tempdir(),
    pattern = "^batch_.*[.]rds$", recursive = TRUE
  )))
  caller_rng <- .paper_mbb_rng_capture()
  on.exit(.paper_mbb_rng_restore(caller_rng), add = TRUE)
  do.call(RNGkind, as.list(paper_mbb_protocol()$rng_kind))
  frame <- structural_inference_axis(prepared, settings)
  shift <- if (is.null(index)) 0 else sum(index * seq_along(index)) / 1e7
  frame$lower <- shift
  frame$upper <- shift + (frame$tau > 0)
  frame$lower_status <- frame$upper_status <- "bounded"
  diagnostics <- list(draw_id = draw_id, stream = stats::runif(1L), fits = shift * 1:5)
  list(frame = alter(frame, draw_id), diagnostics = diagnostics, retained = NULL)
}
