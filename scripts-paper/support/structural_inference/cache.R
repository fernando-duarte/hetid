# Dedicated cache for the structural-inference draws, kept apart from the
# unified bootstrap stage's cache. The full diagnostics run to about 20 GB, so
# the writer never holds more than the payload and one exact-RDS round trip:
# no backup reload, and nothing is read back after the rename.

# Helper function: the persisted result, without the fitted closures and lm objects
# the full-sample fit retains hetid's evidence closures and the reference keeps
# its lm and PPML fits; every number they carry is already in the frames and
# diagnostics, and the closures would not survive an exact round trip
structural_inference_cache_payload <- function(prepared, reference, bootstrap, identity) {
  bootstrap$full$retained <- NULL
  reference$ols <- NULL
  reference$ppml <- NULL
  list(prepared = prepared, reference = reference, bootstrap = bootstrap, identity = identity)
}

# Helper function: the cached result for this identity, or NULL and why it is not reused
structural_inference_cache_read <- function(path, identity) {
  if (!file.exists(path)) {
    message("structural inference: no cache at ", path, "; computing the draws")
    return(NULL)
  }
  value <- tryCatch(readRDS(path), error = function(error) error)
  valid <- if (inherits(value, "error")) {
    paste("it could not be read:", conditionMessage(value))
  } else {
    tryCatch(structural_inference_cache_check(value, identity),
      error = function(error) paste("its check failed:", conditionMessage(error))
    )
  }
  if (!isTRUE(valid)) {
    message(
      "structural inference: not reusing ", path, " because ", valid,
      "; recomputing the draws"
    )
    return(NULL)
  }
  message("structural inference: reusing the draws in ", path)
  value
}

# Helper function: verify beside the target, then promote with one rename
# the previous cache stays in place until the rename, so any failure before it
# leaves that file as it was. Dropbox is told to ignore both names before they
# carry data
structural_inference_cache_write <- function(value, path, writer = paper_write_exact_rds,
                                             promoter = file.rename) {
  valid <- structural_inference_cache_check(value, value$identity)
  if (!isTRUE(valid)) {
    stop("The structural inference cache was not written: ", valid, call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(path), ".tmp-"), tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  if (!file.create(temporary)) {
    stop("Could not create the temporary cache file ", temporary, call. = FALSE)
  }
  paper_flag_fileprovider_ignored(temporary)
  writer(value, temporary, "structural inference cache")
  if (!isTRUE(promoter(temporary, path))) {
    stop("Could not move the verified structural inference cache to ", path,
      "; the previous cache, if any, is unchanged",
      call. = FALSE
    )
  }
  paper_flag_fileprovider_ignored(path)
  invisible(path)
}
