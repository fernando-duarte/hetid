# Schema, axis and identity checks for a cached structural-inference result.
# A check returns TRUE or one sentence naming the first problem it found, so the
# reader can say why it recomputes and the writer can refuse a bad payload.

STRUCTURAL_INFERENCE_CACHE_FIELDS <- c("prepared", "reference", "bootstrap", "identity")
STRUCTURAL_INFERENCE_REFERENCE_FIELDS <- c("frame", "variance_errors", "metadata")
STRUCTURAL_INFERENCE_BOOTSTRAP_FIELDS <- c(
  "full", "draws", "point_summary", "intervals", "failure_gates", "publication_ok",
  "metadata"
)
STRUCTURAL_INFERENCE_DRAW_FIELDS <- c(
  "lower", "upper", "lower_status", "upper_status", "full", "indices", "results",
  "errors", "callback_failed", "n_callback_failed"
)

# Helper function: the path to the first environment, function or pointer, or NULL
structural_inference_find_reference <- function(value, path = "result") {
  if (is.environment(value) || is.function(value) ||
    typeof(value) %in% c("externalptr", "weakref", "promise")) {
    return(path)
  }
  for (name in names(attributes(value))) {
    found <- structural_inference_find_reference(
      attr(value, name, exact = TRUE), paste0(path, "@", name)
    )
    if (!is.null(found)) {
      return(found)
    }
  }
  if (is.recursive(value)) {
    children <- as.list(value)
    for (k in seq_along(children)) {
      found <- structural_inference_find_reference(children[[k]], paste0(path, "[[", k, "]]"))
      if (!is.null(found)) {
        return(found)
      }
    }
  }
  NULL
}

# Helper function: TRUE for a cached result that answers this identity, else why not
structural_inference_cache_check <- function(value, identity) {
  if (!is.list(value) || !identical(names(value), STRUCTURAL_INFERENCE_CACHE_FIELDS)) {
    return("it does not hold the structural inference result fields")
  }
  if (!is.list(value$identity) || !identical(names(value$identity), names(identity))) {
    return("its identity has a different schema")
  }
  stale <- names(identity)[!vapply(names(identity), function(field) {
    identical(value$identity[[field]], identity[[field]])
  }, logical(1))]
  if (length(stale)) {
    return(paste("it is stale in", paste(stale, collapse = ", ")))
  }
  settings <- identity$settings
  prepared <- value$prepared
  if (!is.list(prepared) || !identical(prepared$settings, settings) ||
    !identical(structural_inference_input_sha(prepared), identity$input_sha)) {
    return("its prepared inputs do not match its identity")
  }
  reference <- value$reference
  if (!is.list(reference) ||
    !identical(names(reference), STRUCTURAL_INFERENCE_REFERENCE_FIELDS) ||
    !is.data.frame(reference$frame) ||
    !all(c("mean", "variance") %in% reference$frame$panel)) {
    return("its reference column is malformed")
  }
  boot <- value$bootstrap
  if (!is.list(boot) || !identical(names(boot), STRUCTURAL_INFERENCE_BOOTSTRAP_FIELDS) ||
    !identical(names(boot$full), c("frame", "diagnostics")) ||
    !identical(boot$metadata$settings, settings) ||
    !is.logical(boot$publication_ok) || length(boot$publication_ok) != 1L ||
    is.na(boot$publication_ok)) {
    return("its bootstrap result is malformed")
  }
  axis <- structural_inference_axis(prepared, settings)$coef
  if (!is.data.frame(boot$full$frame) || !identical(boot$full$frame$coef, axis)) {
    return("its full-sample frame is not on the coefficient axis")
  }
  valid <- structural_inference_draws_check(boot$draws, axis, settings, prepared$n_obs)
  if (!isTRUE(valid)) {
    return(valid)
  }
  zero <- boot$full$frame$tau == 0
  if (!identical(boot$point_summary$coef, axis[zero]) ||
    !identical(boot$intervals$summary$coef, axis[!zero]) ||
    !identical(boot$failure_gates$coef, axis)) {
    return("its calibrated summaries are not on the coefficient axis")
  }
  found <- structural_inference_find_reference(value)
  if (!is.null(found)) {
    return(paste("it holds an environment or function at", found))
  }
  TRUE
}

# Helper function: TRUE for draws with the settings' count on the axis, else why not
structural_inference_draws_check <- function(draws, axis, settings, n_obs) {
  if (!is.list(draws) || !identical(names(draws), STRUCTURAL_INFERENCE_DRAW_FIELDS)) {
    return("its draws are malformed")
  }
  n_draws <- settings$n_draws
  shaped <- vapply(STRUCTURAL_INFERENCE_DRAW_FIELDS[1:4], function(field) {
    is.matrix(draws[[field]]) && identical(dim(draws[[field]]), c(n_draws, length(axis))) &&
      identical(colnames(draws[[field]]), axis)
  }, logical(1))
  if (!all(shaped)) {
    return(paste(
      "its draw matrices are not", n_draws, "draws by the coefficient axis:",
      paste(names(shaped)[!shaped], collapse = ", ")
    ))
  }
  if (length(draws$indices) != n_draws || length(draws$results) != n_draws ||
    length(draws$errors) != n_draws || length(draws$callback_failed) != n_draws ||
    !all(lengths(draws$indices) == n_obs)) {
    return(paste("it does not hold", n_draws, "complete draws"))
  }
  if (!identical(draws$full$coef, axis)) {
    return("its draws' full-sample frame is not on the coefficient axis")
  }
  TRUE
}
