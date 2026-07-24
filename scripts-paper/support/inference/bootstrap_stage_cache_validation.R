bootstrap_stage_payload_validate <- function(value, fields, validators) {
  if (!bootstrap_stage_cache_exact_list(value, fields)) {
    return("payload fields or class changed")
  }
  for (name in names(validators)) {
    result <- tryCatch(validators[[name]](value), error = identity)
    if (inherits(result, "error")) {
      return(paste0(name, ": validation error: ", conditionMessage(result)))
    }
    if (!isTRUE(result)) {
      return(paste0(name, ": ", result))
    }
  }
  TRUE
}

bootstrap_stage_read_validated <- function(path, validator, reader = readRDS) {
  value <- tryCatch(reader(path), error = identity)
  if (inherits(value, "error")) {
    return(list(value = NULL, reason = "cache unreadable"))
  }
  valid <- tryCatch(validator(value), error = identity)
  if (inherits(valid, "error")) {
    return(list(
      value = NULL,
      reason = paste0("cache validation error: ", conditionMessage(valid))
    ))
  }
  if (!isTRUE(valid)) {
    return(list(value = NULL, reason = valid))
  }
  list(value = value, reason = NULL)
}

bootstrap_stage_remove_validated <- function(
  paths, cache_path, validator, reader = readRDS, remover = unlink
) {
  authenticated <- bootstrap_stage_read_validated(
    cache_path, validator, reader
  )
  if (!is.null(authenticated$reason)) {
    stop("validated removal refused: ", authenticated$reason, call. = FALSE)
  }
  present <- paths[file.exists(paths)]
  if (length(present)) remover(present)
  if (any(file.exists(present))) {
    stop("validated removal left one or more exact paths", call. = FALSE)
  }
  present
}

bootstrap_stage_manifest_expand <- function(directories, files, path_fn) {
  expanded <- unlist(lapply(directories, function(relative) {
    parts <- strsplit(relative, "/", fixed = TRUE)[[1L]]
    root <- do.call(path_fn, as.list(parts))
    file.path(relative, list.files(root, pattern = "[.]R$"))
  }), use.names = FALSE)
  sort(unique(c(expanded, files)))
}

bootstrap_stage_cache_exact_list <- function(value, fields) {
  is.list(value) && identical(attributes(value), list(names = fields))
}

bootstrap_stage_cache_count_ok <- function(value, upper) {
  is.integer(value) && length(value) == 1L && !is.na(value) &&
    is.null(attributes(value)) && value >= 0L && value <= upper
}

bootstrap_stage_cache_matrix_ok <- function(value, n_rows, coefs, type) {
  is.matrix(value) && identical(typeof(value), type) &&
    identical(dim(value), c(as.integer(n_rows), length(coefs))) &&
    identical(dimnames(value), list(NULL, coefs))
}

bootstrap_stage_cache_status_ok <- function(value, status_values) {
  is.character(value) && !anyNA(value) && all(value %in% status_values)
}

bootstrap_stage_cache_table_ok <- function(value) {
  is.integer(value) && inherits(value, "table")
}
