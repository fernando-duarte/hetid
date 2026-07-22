# Freshness fingerprints for the bootstrap draw caches. A cache is reusable only
# when every fingerprint matches the current run, so a change to the data, the
# draw spec, the expensive-path code, or the runtime forces a fresh resample.

paper_boot_runtime_sha <- function() {
  pkgs <- c("hetid", "nloptr", "quantreg", "rugarch", "sandwich")
  vers <- vapply(pkgs, function(p) as.character(utils::packageVersion(p)), character(1))
  paper_sha256_object(list(r = as.character(getRversion()), pkgs = stats::setNames(vers, pkgs)))
}

paper_boot_code_sha <- function(paths) {
  contents <- lapply(sort(unique(paths)), function(rel) {
    parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
    readLines(do.call(paper_path, as.list(parts)), warn = FALSE)
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
