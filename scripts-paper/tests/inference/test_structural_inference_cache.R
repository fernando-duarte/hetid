#!/usr/bin/env Rscript
# Offline checks for the structural-inference cache: reuse without draws,
# invalidation by data, settings and code, rejection of malformed payloads, and
# a failed write that leaves the previous cache in place. The results are
# synthetic, with the real axis and schema; a small real fit follows when the
# frozen inputs are present. Run from root:
#   Rscript scripts-paper/tests/inference/test_structural_inference_cache.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("support", "structural_inference", "api.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
paper_source_once(paper_path("tests", "inference", "structural_inference_cache_stubs.R"))
.test <- paper_test_harness()
check <- .test$check

folder <- tempfile("structural-cache-")
dir.create(folder)
path <- file.path(folder, "structural_inference_draws.rds")
settings <- cache_stub_settings()
prepared <- cache_stub_prepared(settings)
hash <- function() unname(tools::md5sum(path))
leftovers <- function() list.files(folder, pattern = "[.]tmp-", all.files = TRUE)

# Reuse and rerun -----------------------------------------------------------
first <- stub_run(settings, prepared, path)
check("a missing cache runs the draws once and writes them", {
  calls$draws == 1L && file.exists(path) && !length(leftovers())
})
check("the cache keeps completed draws whose publication gate failed", {
  identical(first$bootstrap$publication_ok, FALSE) &&
    identical(dim(first$bootstrap$draws$lower), c(4L, nrow(first$bootstrap$full$frame)))
})
check("closures and fitted lm objects are pruned, every other field kept", {
  identical(names(first$bootstrap$full), c("frame", "diagnostics")) &&
    identical(names(first$reference), c("frame", "variance_errors", "metadata")) &&
    is.null(structural_inference_find_reference(first)) &&
    identical(readRDS(path), first)
})
second <- stub_run(settings, prepared, path)
check("a valid cache is returned as read and runs no draws", {
  calls$draws == 1L && identical(second, first)
})
check("rerun mode ignores a valid cache and replaces it", {
  before <- file.mtime(path)
  Sys.sleep(1.1)
  again <- stub_run(settings, prepared, path, mode = "rerun")
  calls$draws == 2L && identical(again, first) && file.mtime(path) > before
})

# Invalidation --------------------------------------------------------------
# each change is undone and the baseline cache rewritten before the next, so
# every message names only the field that changed
recomputes <- function(label, settings, prepared, field) {
  said <- run_message(settings, prepared, path)
  check(label, grepl(paste0("stale in ", field, ";"), said, fixed = TRUE))
}
baseline <- function() invisible(stub_run(settings, prepared, path))
recomputes(
  "changed data invalidates the cache", settings,
  cache_stub_prepared(settings, shift = 1e-12), "input_sha"
)
baseline()
wider <- cache_stub_settings(n_draws = 5L)
recomputes(
  "changed settings invalidate the cache", wider, cache_stub_prepared(wider),
  "settings"
)
baseline()
files <- STRUCTURAL_INFERENCE_CODE_FILES
STRUCTURAL_INFERENCE_CODE_FILES <- head(files, -1L)
recomputes(
  "a changed numerical source file set invalidates the cache", settings, prepared,
  "code_sha"
)
STRUCTURAL_INFERENCE_CODE_FILES <- files
baseline()
original_nw <- paper_newey_west_statistics
paper_newey_west_statistics <- function(...) original_nw(...)
recomputes(
  "a changed Newey-West helper invalidates the cache", settings, prepared,
  "function_sha"
)
paper_newey_west_statistics <- original_nw
baseline()
check("the hetid code hash survives a hetid call that sets call flags", {
  before <- structural_inference_code_sha(structural_inference_namespace()$functions)
  invisible(hetid:::recession_direction(list(A_i = list(-diag(2)))))
  identical(structural_inference_code_sha(structural_inference_namespace()$functions), before)
})
check("the identity carries no macro path, docs file or RemoteSha", {
  text <- paste(deparse(structural_inference_identity(prepared, settings)), collapse = "")
  !grepl("macro|docs/|RemoteSha", text) && !any(grepl("^docs|macro", files))
})

# Malformed payloads --------------------------------------------------------
current <- structural_inference_identity(prepared, settings)
rejects <- function(label, change, pattern) {
  value <- first
  value <- change(value)
  reason <- structural_inference_cache_check(value, current)
  saveRDS(value, path)
  before <- calls$draws
  said <- run_message(settings, prepared, path)
  check(label, is.character(reason) && grepl(pattern, reason) &&
    calls$draws == before + 1L && grepl(pattern, said))
}
rejects("a missing field is rejected", function(v) v[-2L], "result fields")
rejects("a missing draw is rejected", function(v) {
  v$bootstrap$draws$lower <- v$bootstrap$draws$lower[-1L, ]
  v
}, "draw matrices")
rejects("a renamed coefficient axis is rejected", function(v) {
  colnames(v$bootstrap$draws$upper)[1L] <- "mean|other|tau=0.00"
  v
}, "draw matrices")
rejects("a short index list is rejected", function(v) {
  v$bootstrap$draws$indices <- v$bootstrap$draws$indices[-1L]
  v
}, "complete draws")
rejects("prepared inputs that disagree with the identity are rejected", function(v) {
  v$prepared$y[1L] <- v$prepared$y[1L] + 1
  v
}, "prepared inputs")
rejects("a stored environment is rejected", function(v) {
  v$bootstrap$full$diagnostics$env <- new.env()
  v
}, "environment or function")
rejects("a changed calibrated summary is rejected", function(v) {
  v$bootstrap$point_summary <- v$bootstrap$point_summary[-1L, , drop = FALSE]
  v
}, "calibrated summaries")
writeBin(as.raw(1:64), path)
check("an unreadable cache is reported and recomputed", {
  before <- calls$draws
  said <- run_message(settings, prepared, path)
  calls$draws == before + 1L && grepl("could not be read", said)
})

paper_source_once(paper_path("tests", "inference", "structural_inference_cache_write_checks.R"))
paper_source_once(paper_path("tests", "inference", "structural_inference_default_block_checks.R"))
unlink(folder, recursive = TRUE)
.test$finish()
