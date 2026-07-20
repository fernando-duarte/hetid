# Shared serialization, hashing, condition, and evaluation-capture primitives.

PAPER_SERIALIZATION_CONTROL <- list(
  rds_version = 3L,
  numeric_format = "%.17g",
  roundtrip_tolerance = 1e-9,
  na_token = "NA",
  character_escape = "\\",
  encoding = "UTF-8",
  yearquarter_format = "%Y Q%q",
  yearquarter_pattern = "^[0-9]{4} Q[1-4]$"
)

paper_numeric_key <- function(value) {
  sprintf(PAPER_SERIALIZATION_CONTROL$numeric_format, value)
}

paper_md5_rds <- function(object) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(
    object,
    path,
    version = PAPER_SERIALIZATION_CONTROL$rds_version
  )
  unname(tools::md5sum(path))
}

paper_sha256_raw <- function(bytes) {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeBin(bytes, path)
  unname(tools::sha256sum(path))
}

paper_sha256_object <- function(object) {
  paper_sha256_raw(serialize(
    object,
    NULL,
    version = PAPER_SERIALIZATION_CONTROL$rds_version
  ))
}

paper_sha256_string <- function(value) {
  paper_sha256_raw(charToRaw(enc2utf8(value)))
}

paper_stop_condition <- function(reason, parent_class, detail = "",
                                 message = NULL, fields = list()) {
  if (is.null(message)) {
    message <- if (nzchar(detail)) {
      paste0(reason, ": ", detail)
    } else {
      reason
    }
  }
  payload <- c(list(message = message, call = NULL), fields)
  stop(structure(
    payload,
    class = c(reason, parent_class, "error", "condition")
  ))
}

paper_capture_conditions <- function(expression, error_prefix = "error: ") {
  warnings <- character(0)
  messages <- character(0)
  captured_error <- NULL
  value <- tryCatch(
    withCallingHandlers(
      expression,
      warning = function(condition) {
        warnings <<- c(warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      },
      message = function(condition) {
        messages <<- c(messages, conditionMessage(condition))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(condition) {
      captured_error <<- condition
      NULL
    }
  )
  list(
    value = value,
    warnings = warnings,
    messages = messages,
    error = captured_error,
    error_class = if (is.null(captured_error)) {
      NA_character_
    } else {
      class(captured_error)[[1L]]
    },
    error_message = if (is.null(captured_error)) {
      NA_character_
    } else {
      paste0(error_prefix, conditionMessage(captured_error))
    }
  )
}

# read-only git HEAD provenance stamp, fully captured so it never reaches the
# console regression; NA when the git call is unavailable
paper_git_head_or_na <- function() {
  tryCatch(
    suppressWarnings(
      system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)
    ),
    error = function(e) NA_character_
  )
}
