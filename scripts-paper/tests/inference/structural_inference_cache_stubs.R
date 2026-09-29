# Synthetic structural-inference results with the real axis and cache schema,
# small enough to write and read in a test. No numerical system is solved.

# Helper function: settings with the fields the axis and the cache read
cache_stub_settings <- function(n_draws = 4L) {
  list(n_draws = as.integer(n_draws), seed = 20260708L, taus = c(0.05, 0.10, 0.20))
}

# Helper function: a prepared tuple on twelve origins, the first without PC_R
cache_stub_prepared <- function(settings, shift = 0) {
  n <- 12L
  block <- function(prefix) {
    matrix(seq_len(2L * n) / 7 + shift, n, 2L, dimnames = list(NULL, paste0(prefix, 1:2)))
  }
  list(
    y = seq_len(n) / 3 + shift, x = block("expected_sdf_pc"), y2 = block("sdf_news_pc"),
    z = block("fz")[, 1L, drop = FALSE], x_var = block("pc"),
    variance = c(FALSE, rep(TRUE, n - 1L)),
    dates = seq(as.Date("2000-03-31"), by = "quarter", length.out = n),
    n_obs = n, settings = settings
  )
}

# Helper function: a reference column that still carries its fitted lm and PPML
cache_stub_reference <- function(prepared, settings) {
  data <- data.frame(y = prepared$y, x = prepared$x[, 1L])
  list(
    frame = data.frame(panel = c("mean", "variance"), term = "(Intercept)", estimate = 1:2),
    ols = stats::lm(y ~ x, data), ppml = list(coef = 1, closure = function() 1),
    variance_errors = data.frame(term = "(Intercept)", hac = 0.5),
    metadata = list(hac_lags = 4L)
  )
}

# Helper function: a completed bootstrap whose publication gate failed
# the full fit keeps a closure, as hetid's retained evidence does
cache_stub_bootstrap <- function(prepared, settings, workers = 1L) {
  axis <- structural_inference_axis(prepared, settings)
  n <- settings$n_draws
  cells <- list(sprintf("draw_%04d", seq_len(n)), axis$coef)
  endpoints <- matrix(seq_len(n * nrow(axis)) / 10, n, nrow(axis), dimnames = cells)
  status <- matrix("bounded", n, nrow(axis), dimnames = cells)
  indices <- rep(list(seq_len(prepared$n_obs)), n)
  names(indices) <- cells[[1L]]
  frames <- rep(list(structure(axis, diagnostics = list(fits = 1:3))), n)
  zero <- axis$tau == 0
  list(
    full = list(
      frame = axis, diagnostics = list(draw_id = 0L),
      retained = list(evidence = function(theta) theta)
    ),
    draws = list(
      lower = endpoints, upper = endpoints + 1, lower_status = status, upper_status = status,
      full = axis, indices = indices, results = frames, errors = vector("list", n),
      callback_failed = rep(FALSE, n), n_callback_failed = 0L
    ),
    point_summary = data.frame(coef = axis$coef[zero], stringsAsFactors = FALSE),
    intervals = list(summary = data.frame(coef = axis$coef[!zero], stringsAsFactors = FALSE)),
    failure_gates = data.frame(coef = axis$coef, passed = FALSE, stringsAsFactors = FALSE),
    publication_ok = FALSE, metadata = list(settings = settings)
  )
}

# Helper function: run with counted stand-ins for the reference and the draws
calls <- new.env()
calls$draws <- 0L
real_reference <- structural_inference_reference
real_bootstrap <- structural_inference_bootstrap
structural_inference_reference <- cache_stub_reference
structural_inference_bootstrap <- function(prepared, settings, workers = 1L) {
  calls$draws <- calls$draws + 1L
  cache_stub_bootstrap(prepared, settings, workers)
}
stub_run <- function(settings, prepared, path, mode = "reuse") {
  suppressMessages(paper_structural_inference_run(
    settings,
    workers = 1L, path = path, mode = mode, prepared = prepared
  ))
}

# Helper function: the message a run prints, to see why it recomputed
run_message <- function(settings, prepared, path) {
  said <- character(0)
  withCallingHandlers(
    paper_structural_inference_run(settings, 1L, path, "reuse", prepared),
    message = function(condition) {
      said <<- c(said, conditionMessage(condition))
      invokeRestart("muffleMessage")
    }
  )
  paste(said, collapse = "")
}
