# Accessors and invariants for the log-variance estimator registry.

paper_logvar_result_exists <- function(
  key,
  envir = .GlobalEnv
) {
  exists(
    paper_logvar_estimator_spec(key)$result_object,
    envir = envir,
    inherits = FALSE
  )
}

paper_logvar_result <- function(
  key,
  envir = .GlobalEnv,
  required = TRUE
) {
  object <- paper_logvar_estimator_spec(key)$result_object
  if (!exists(object, envir = envir, inherits = FALSE)) {
    if (!isTRUE(required)) {
      return(NULL)
    }
    stop(
      sprintf("Missing pipeline result for estimator %s", key),
      call. = FALSE
    )
  }
  get(object, envir = envir, inherits = FALSE)
}

paper_logvar_assign_result <- function(
  key,
  value,
  envir = .GlobalEnv
) {
  assign(
    paper_logvar_estimator_spec(key)$result_object,
    value,
    envir = envir
  )
  invisible(value)
}

.logvar_keys <- names(PAPER_LOGVAR_ESTIMATORS)
stopifnot(
  !anyDuplicated(.logvar_keys),
  identical(
    .logvar_keys,
    unname(vapply(
      PAPER_LOGVAR_ESTIMATORS,
      `[[`,
      character(1),
      "key"
    ))
  ),
  all(vapply(
    PAPER_LOGVAR_ESTIMATORS,
    function(spec) all(spec$dependencies %in% .logvar_keys),
    logical(1)
  ))
)
rm(.logvar_keys)
