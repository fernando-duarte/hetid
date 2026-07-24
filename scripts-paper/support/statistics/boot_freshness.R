# Freshness fingerprints for the bootstrap draw caches. A cache is reusable only
# when every fingerprint matches the current run, so a change to the data, the
# draw spec, the expensive-path code, or the runtime forces a fresh resample.

paper_boot_runtime_sha <- function() {
  pkgs <- c("hetid", "nloptr", "quantreg", "rugarch", "sandwich")
  versions <- vapply(
    pkgs,
    function(package) as.character(utils::packageVersion(package)),
    character(1)
  )
  external <- extSoftVersion()
  paper_sha256_object(list(
    r_version = as.character(getRversion()),
    platform = R.version$platform,
    packages = stats::setNames(versions, pkgs),
    blas = unname(external[["BLAS"]]),
    lapack = unname(external[["LAPACK"]]),
    controls = list(
      analysis = get0("PAPER_ANALYSIS_CONTRACT", inherits = TRUE),
      inference = get0("PAPER_INFERENCE_SEARCH_CONTROL", inherits = TRUE),
      reporting = get0("PAPER_REPORTING_CONTROL", inherits = TRUE),
      serialization = get0("PAPER_SERIALIZATION_CONTROL", inherits = TRUE),
      cache_schema = get0("BOOTSTRAP_STAGE_CACHE_SCHEMA", inherits = TRUE)
    )
  ))
}

paper_boot_code_sha <- function(paths) {
  paths <- sort(unique(paths))
  stopifnot(
    is.character(paths), length(paths) > 0L, !anyNA(paths),
    all(nzchar(paths)), !any(grepl("^/|(^|/)\\.\\.(/|$)", paths))
  )
  contents <- lapply(paths, function(relative_path) {
    parts <- strsplit(relative_path, "/", fixed = TRUE)[[1L]]
    path <- do.call(paper_path, as.list(parts))
    size <- file.info(path)$size
    list(
      path = relative_path,
      bytes = readBin(path, what = "raw", n = size)
    )
  })
  paper_sha256_object(contents)
}

paper_boot_index_sha <- function(sample_size, block, b_reps, seed) {
  run <- paper_run_mbb_draws(b_reps, sample_size, block,
    function(index, draw_id) 0L,
    seed = seed
  )
  paper_sha256_object(run$indices)
}

paper_boot_freshness_matches <- function(cached_prov, current_prov, fields) {
  for (f in fields) {
    if (!identical(cached_prov[[f]], current_prov[[f]])) {
      return(f)
    }
  }
  TRUE
}
