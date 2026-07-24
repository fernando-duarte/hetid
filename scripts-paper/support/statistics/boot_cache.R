# Atomic validated replacement for the unified bootstrap cache.

paper_boot_transactional_replace <- function(
  payload, path, validator, reader, writer, promoter = file.rename,
  remover = unlink, copier = file.copy
) {
  valid <- validator(payload)
  if (!isTRUE(valid)) {
    stop("invalid in-memory cache: ", valid, call. = FALSE)
  }
  backup <- NULL
  if (file.exists(path)) {
    previous <- tryCatch(reader(path), error = identity)
    previous_valid <- if (inherits(previous, "error")) {
      FALSE
    } else {
      tryCatch(isTRUE(validator(previous)), error = function(error) FALSE)
    }
    if (previous_valid) {
      backup <- tempfile(
        paste0(".", basename(path), ".backup-"),
        tmpdir = dirname(path)
      )
      if (!isTRUE(copier(path, backup, overwrite = TRUE))) {
        stop("prior valid cache could not be backed up", call. = FALSE)
      }
    }
  }
  temporary <- tempfile(
    paste0(".", basename(path), ".tmp-"),
    tmpdir = dirname(path)
  )
  cleanup_backup <- TRUE
  on.exit(if (file.exists(temporary)) remover(temporary), add = TRUE)
  on.exit(if (cleanup_backup && !is.null(backup) && file.exists(backup)) {
    remover(backup)
  }, add = TRUE)
  writer(payload, temporary)
  roundtrip <- reader(temporary)
  if (!identical(roundtrip, payload)) {
    stop("temporary cache round trip changed", call. = FALSE)
  }
  valid <- validator(roundtrip)
  if (!isTRUE(valid)) {
    stop("invalid temporary cache: ", valid, call. = FALSE)
  }
  if (!isTRUE(promoter(temporary, path))) {
    stop("atomic cache promotion failed", call. = FALSE)
  }
  installed <- tryCatch(
    {
      value <- reader(path)
      if (!identical(value, roundtrip)) {
        stop("installed cache object changed", call. = FALSE)
      }
      valid <- validator(value)
      if (!isTRUE(valid)) {
        stop("invalid installed cache: ", valid, call. = FALSE)
      }
      value
    },
    error = identity
  )
  if (inherits(installed, "error")) {
    recovered <- if (is.null(backup)) {
      !file.exists(path) || remover(path) == 0L
    } else {
      cleanup_backup <- FALSE
      copied <- tryCatch(
        isTRUE(copier(backup, path, overwrite = TRUE)),
        error = function(error) FALSE
      )
      if (!copied) {
        FALSE
      } else {
        restored <- tryCatch(reader(path), error = identity)
        !inherits(restored, "error") &&
          identical(restored, previous) &&
          isTRUE(tryCatch(
            validator(restored),
            error = function(error) FALSE
          ))
      }
    }
    if (recovered) cleanup_backup <- TRUE
    if (!recovered) {
      stop(
        conditionMessage(installed),
        "; prior cache recovery failed; valid backup retained at ",
        backup,
        call. = FALSE
      )
    }
    stop(conditionMessage(installed), call. = FALSE)
  }
  list(value = installed, recovery_backup = NULL)
}
