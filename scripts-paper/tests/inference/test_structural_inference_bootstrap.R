#!/usr/bin/env Rscript
# Offline checks for the structural-inference bootstrap orchestration: seeded
# indices, the caller's RNG state, spooled batches, serial against forked
# workers, and the failure and calibration publication gates. The per-draw fit
# is a stand-in, so no numerical system is solved. Run from root:
#   Rscript scripts-paper/tests/inference/test_structural_inference_bootstrap.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("support", "statistics", "mbb_protocol_authority.R"))
paper_source_once(paper_path("support", "statistics", "mbb_rng_state.R"))
paper_source_once(paper_path("support", "structural_inference", "calibration.R"))
paper_source_once(paper_path("support", "structural_inference", "bootstrap.R"))
paper_source_once(paper_path("tests", "inference", "structural_inference_bootstrap_stubs.R"))

settings <- stub_settings()
prepared <- stub_prepared(settings)
axis <- structural_inference_axis(prepared, settings)
fake_full <- with_fit(fake_fit, structural_inference_fit(prepared, settings))
boot <- function(..., full = fake_full) {
  suppressMessages(with_fit(fake_fit, structural_inference_bootstrap(..., full = full)))
}

# Seeded indices and the caller's RNG state -------------------------------
RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
set.seed(17)
caller <- rng_state()
calls$index <- NULL
first <- boot(prepared, settings)
stopifnot(identical(rng_state(), caller))
do.call(RNGkind, as.list(paper_mbb_protocol()$rng_kind))
set.seed(settings$seed)
expected <- hetid::circular_mbb_indices(prepared$n_obs, settings$block_length, settings$n_draws)
.paper_mbb_rng_install(caller$kind, caller$seed)
if (!identical(unname(first$draws$indices), expected)) {
  fail("indices are not the seeded Mersenne-Twister circular blocks")
}
second <- boot(prepared, settings)
stopifnot(
  identical(first, second), identical(rng_state(), caller),
  identical(first$metadata$rng_kind, paper_mbb_protocol()$rng_kind),
  identical(first$metadata$sample, prepared$sample),
  identical(names(first), c(
    "full", "draws", "point_summary", "intervals", "failure_gates", "publication_ok",
    "metadata"
  ))
)
cat("ok   seeded indices repeat and the caller's RNG state comes back\n")

# one fit per draw, and every panel and tau of that draw read its index
stopifnot(
  length(calls$index) == 2L * settings$n_draws,
  identical(calls$index[seq_len(settings$n_draws)], unname(first$draws$indices)),
  identical(colnames(first$draws$lower), axis$coef), first$publication_ok
)
for (d in seq_len(settings$n_draws)) {
  index <- first$draws$indices[[d]]
  if (!all(first$draws$lower[d, ] == sum(index * seq_along(index)) / 1e7)) {
    fail("draw ", d, " mixes resampled rows")
  }
}
cat("ok   one index per draw shared by both panels and every tau\n")

# Unexpected errors -------------------------------------------------------
contract <- structure(list(message = "Unexpected contract", call = NULL),
  class = c("structural_test_contract", "error", "condition")
)
n_calls <- 0L
broken <- function(...) {
  n_calls <<- n_calls + 1L
  stop(contract)
}
caught <- with_fit(broken, tryCatch(
  structural_inference_bootstrap(prepared, settings, fake_full),
  error = identity
))
stopifnot(identical(caught, contract), n_calls == 1L, identical(rng_state(), caller))
rm(".Random.seed", envir = globalenv())
unseeded <- boot(prepared, settings)
stopifnot(
  is.null(rng_state()$seed), identical(RNGkind(), caller$kind),
  identical(unseeded$draws$indices, first$draws$indices), length(spools()) == 0L
)
cat("ok   an unexpected error stops after one fit and is rethrown unchanged\n")

# Parallel workers --------------------------------------------------------
# forked workers must return exactly what the serial loop does, draw for draw,
# across the spooled batch boundary, and every draw sees the same stream
set.seed(5)
caller <- rng_state()
many <- stub_settings(45L)
many_prepared <- stub_prepared(many)
many_full <- with_fit(fake_fit, structural_inference_fit(many_prepared, many))
calls$spooled <- NULL
batched <- lapply(c(1L, 2L), function(workers) {
  boot(many_prepared, many, full = many_full, workers = workers)
})
# serial batches hold ten draws, so draw 21 is fitted with two batches spooled
stopifnot(identical(calls$spooled[1:45], rep(0:4, each = 10)[1:45]))
diagnostics <- lapply(batched[[2]]$draws$results, attr, "diagnostics")
stopifnot(
  identical(batched[[1]], batched[[2]]), identical(rng_state(), caller),
  identical(unname(vapply(diagnostics, `[[`, integer(1), "draw_id")), 1:45),
  identical(names(batched[[2]]$draws$results), sprintf("draw_%04d", 1:45))
)
# every draw, forked or not, starts from the stream the index draw left behind
do.call(RNGkind, as.list(paper_mbb_protocol()$rng_kind))
set.seed(many$seed)
invisible(hetid::circular_mbb_indices(many_prepared$n_obs, many$block_length, many$n_draws))
stream <- fake_fit(many_prepared, many)$diagnostics$stream
.paper_mbb_rng_install(caller$kind, caller$seed)
for (d in 1:45) {
  index <- batched[[2]]$draws$indices[[d]]
  expected <- fake_fit(many_prepared, many, index, d)$diagnostics
  expected$stream <- stream
  if (!identical(diagnostics[[d]], expected)) {
    fail("draw ", d, " lost its diagnostics across batches")
  }
}
cat("ok   batches keep the global draw id and order, serial and forked alike\n")

# an unexpected error in one worker stops the run and is rethrown unchanged,
# with or without a caller seed
late <- function(prepared, settings, index = NULL, draw_id = 0L, ...) {
  if (draw_id == 3L) stop(contract)
  fake_fit(prepared, settings, index, draw_id, ...)
}
forked_error <- function() {
  with_fit(late, tryCatch(
    suppressWarnings(suppressMessages(
      structural_inference_bootstrap(prepared, settings, fake_full, 2L)
    )),
    error = identity
  ))
}
stopifnot(identical(forked_error(), contract), identical(rng_state(), caller))
rm(".Random.seed", envir = globalenv())
stopifnot(
  identical(forked_error(), contract), is.null(rng_state()$seed),
  identical(boot(prepared, settings, workers = 2L), first), length(spools()) == 0L
)
cat("ok   a forked worker's unexpected error is fatal and the caller's RNG comes back\n")

paper_source_once(paper_path(
  "tests", "inference", "structural_inference_bootstrap_gate_checks.R"
))
cat("Structural inference bootstrap checks passed.\n")
