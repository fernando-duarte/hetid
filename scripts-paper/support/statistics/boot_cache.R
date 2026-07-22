# Reuse-or-run dispatcher for the expensive bootstrap draw caches. Reuse loads a
# cache only when it deserializes, validates structurally, and matches every
# freshness fingerprint; otherwise it warns and reruns. Only cache problems fall
# back; failures inside run_fn (gates, computation, write) propagate.

paper_boot_cache_freshness_ok <- function(freshness, fields) {
  is.list(freshness) && length(freshness) &&
    all(fields %in% names(freshness)) &&
    all(vapply(freshness[fields], function(v) length(v) == 1L && !is.na(v), logical(1)))
}

paper_boot_cached_or_run_at <- function(path, mode, freshness, fields,
                                        run_fn, validate_fn, warn_label,
                                        writer, reader) {
  if (!paper_boot_cache_freshness_ok(freshness, fields)) {
    stop(sprintf("%s: cannot compute a complete freshness fingerprint", warn_label),
      call. = FALSE
    )
  }
  do_run <- function(tag) {
    obj <- run_fn()
    writer(obj, freshness)
    list(draws = obj, source = tag)
  }
  if (identical(mode, "reuse")) {
    reason <- NULL
    if (!file.exists(path)) {
      reason <- "no cache file"
    } else {
      cached <- tryCatch(reader(), error = function(e) NULL)
      if (is.null(cached)) {
        reason <- "cache unreadable"
      } else {
        v <- validate_fn(cached)
        if (!isTRUE(v)) {
          reason <- if (is.character(v)) v else "cache failed validation"
        } else {
          m <- paper_boot_freshness_matches(cached$provenance, freshness, fields)
          if (!isTRUE(m)) reason <- sprintf("%s changed", m)
        }
      }
    }
    if (is.null(reason)) {
      return(list(draws = reader(), source = "reuse"))
    }
    warning(sprintf("%s: reusing cache not possible (%s); rerunning", warn_label, reason),
      call. = FALSE
    )
    return(do_run("fallback-rerun"))
  }
  do_run("rerun")
}

paper_boot_cached_or_run <- function(mode, artifact_key, freshness, fields,
                                     run_fn, validate_fn, warn_label) {
  path <- artifact_path(artifact_key)
  writer <- function(obj, prov) {
    payload <- c(obj, list(provenance = prov))
    tmp <- paste0(path, ".tmp")
    paper_write_exact_rds(payload, tmp, sprintf("%s cache", warn_label))
    if (!file.rename(tmp, path)) {
      unlink(tmp)
      stop(sprintf("%s: cache write could not be promoted", warn_label), call. = FALSE)
    }
  }
  reader <- function() readRDS(path)
  paper_boot_cached_or_run_at(
    path, mode, freshness, fields, run_fn, validate_fn,
    warn_label, writer, reader
  )
}
