# Failed cache writes leave the previous cache byte for byte, and a small real
# fit survives the exact round trip. Sourced by test_structural_inference_cache.R.

valid <- stub_run(settings, prepared, path)
kept <- hash()
payload <- valid
payload$bootstrap$metadata$note <- "a newer run"
fails_closed <- function(label, ..., value = payload) {
  said <- tryCatch(structural_inference_cache_write(value, path, ...), error = conditionMessage)
  check(label, is.character(said) && identical(hash(), kept) && !length(leftovers()))
}
fails_closed("a writer error keeps the previous cache", writer = function(value, file, prefix) {
  saveRDS("partial", file)
  stop(prefix, ": disk full", call. = FALSE)
})
fails_closed("a round trip that is not exact keeps the previous cache", writer = function(...) {
  paper_write_exact_rds(..., control = list(rds_version = 3L))
  stop("structural inference cache: RDS round trip is not identical", call. = FALSE)
})
fails_closed("a failed rename keeps the previous cache", promoter = function(from, to) FALSE)
check("a failed rename says the previous cache is unchanged", {
  said <- tryCatch(
    structural_inference_cache_write(payload, path, promoter = function(from, to) FALSE),
    error = conditionMessage
  )
  grepl("previous cache, if any, is unchanged", said, fixed = TRUE)
})
broken <- payload
broken$bootstrap$full$retained <- function() 1
fails_closed("a payload with a closure is refused before any file is written",
  writer = function(...) stop("the writer must not run", call. = FALSE), value = broken
)
broken <- payload
broken$bootstrap$draws$results <- broken$bootstrap$draws$results[-1L]
fails_closed("a payload missing a draw is refused before any file is written",
  writer = function(...) stop("the writer must not run", call. = FALSE), value = broken
)
check("a verified write replaces the cache and leaves no temporary", {
  newer <- valid
  newer$bootstrap$metadata$note <- "a newer run"
  structural_inference_cache_write(newer, path)
  identical(readRDS(path), newer) && !identical(hash(), kept) && !length(leftovers())
})

# Small real fit ------------------------------------------------------------
frozen <- tryCatch(
  {
    paper_source_once(paper_path("support", "data", "acm_inputs.R"))
    paper_source_once(paper_path("support", "data", "frozen_inputs.R"))
    paper_verify_frozen_inputs()
    TRUE
  },
  error = function(error) FALSE
)
real_check <- .test$optional_check(frozen, "frozen paper inputs unavailable")
if (frozen) {
  quarterly_acm_inputs <- suppressWarnings(paper_load_quarterly_acm(all_mats))
  for (name in c(
    "build_sdf_series", "build_consumption_growth", "build_yield_volatility",
    "build_asset_return_pcs", "build_sdf_pcs"
  )) {
    paper_source_once(paper_path("data_preparation", paste0(name, ".R")))
  }
  # the real reference and bootstrap, not the stand-ins
  structural_inference_reference <- real_reference
  structural_inference_bootstrap <- real_bootstrap
  real_settings <- structural_inference_settings(n_draws = 2L, n_grid = 41L, n_points = 20L)
  real_path <- file.path(folder, "real.rds")
  real <- suppressMessages(paper_structural_inference_run(real_settings, 1L, real_path, "rerun"))
}
real_check("a small real fit round-trips exactly without environments", {
  loaded <- readRDS(real_path)
  identical(loaded, real) && is.null(structural_inference_find_reference(loaded)) &&
    nrow(real$bootstrap$draws$lower) == 2L && is.list(real$bootstrap$full$diagnostics$positive)
})
structural_inference_bootstrap <- function(...) stop("no draws expected", call. = FALSE)
real_check("a small real fit is reused from its cache", {
  identical(suppressMessages(paper_structural_inference_run(real_settings, 1L, real_path)), real)
})
