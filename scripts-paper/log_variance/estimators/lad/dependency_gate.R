# Offline tri-state reader for the quantreg dependency gate used by the LAD
# median map. It parses the Debian-control decision record, validates
# the closed enums, and reports whether the analysis layer may source LAD code.
# Package availability and the installed version are injected by the caller so
# every gate state is reachable without quantreg present; package presence is
# never read as approval, which lives only in the record's decision field.
# Base R only; no new dependencies. Definitions only, so sourcing this module is
# side-effect free and the pre-LAD pipeline stays byte-identical.
#
# Interface:
#   logvar_lad_gate_read(path, available, installed_version)
#     -> list(decision, ready, source_lad, install_consent, quantreg_version)
# A missing record reads as "unanswered". The reader stops on a malformed line,
# a duplicate, unknown, or missing field, a schema or gate mismatch, an approved
# gate whose package is unavailable, or a recorded version that is stale against
# the installed one.

# The exact required field set, expected identifiers, and closed value enums.
logvar_lad_gate_fields <- c(
  "schema_version", "gate", "decision", "decided_at",
  "rationale", "install_consent", "quantreg_version"
)
logvar_lad_gate_schema <- "1.0.0"
logvar_lad_gate_name <- "logvar-lad-quantreg"
logvar_lad_gate_decisions <- c("unanswered", "declined", "approved")
logvar_lad_gate_consents <- c("not_asked", "not_needed", "declined", "approved")

# Raise a classed condition so callers can dispatch on the gate error family.
logvar_lad_gate_stop <- function(message, subclass) {
  paper_stop_condition(
    subclass,
    "logvar_lad_gate_error",
    message,
    message = message
  )
}

# Assemble the deterministic result list returned to the caller.
logvar_lad_gate_result <- function(decision, ready, source_lad,
                                   install_consent, quantreg_version) {
  list(
    decision = decision,
    ready = ready,
    source_lad = source_lad,
    install_consent = install_consent,
    quantreg_version = quantreg_version
  )
}

# Parse "key: value" lines into a named list, rejecting a malformed line, a
# duplicate field, an unknown field, or any missing required field.
logvar_lad_gate_parse <- function(path) {
  raw <- readLines(path, warn = FALSE)
  raw <- raw[nzchar(trimws(raw))]
  parts <- regmatches(raw, regexec("^([^:]+):[[:space:]]*(.*)$", raw))
  if (!all(vapply(parts, length, integer(1)) == 3L)) {
    logvar_lad_gate_stop(
      "logvar_lad_gate_parse: a record line is not 'key: value'",
      "logvar_lad_gate_malformed"
    )
  }
  keys <- trimws(vapply(parts, `[[`, character(1), 2L))
  values <- vapply(parts, `[[`, character(1), 3L)
  if (anyDuplicated(keys) != 0L) {
    logvar_lad_gate_stop(
      "logvar_lad_gate_parse: a field is duplicated",
      "logvar_lad_gate_duplicate"
    )
  }
  unknown <- setdiff(keys, logvar_lad_gate_fields)
  if (length(unknown) != 0L) {
    logvar_lad_gate_stop(
      sprintf("logvar_lad_gate_parse: unknown field(s): %s", toString(unknown)),
      "logvar_lad_gate_unknown"
    )
  }
  missing <- setdiff(logvar_lad_gate_fields, keys)
  if (length(missing) != 0L) {
    logvar_lad_gate_stop(
      sprintf("logvar_lad_gate_parse: missing field(s): %s", toString(missing)),
      "logvar_lad_gate_incomplete"
    )
  }
  stats::setNames(as.list(values), keys)
}

# Reject an unexpected schema or gate, an out-of-enum decision or consent, or a
# recorded quantreg version present when the decision is not approved.
logvar_lad_gate_check_static <- function(fields) {
  if (!identical(fields$schema_version, logvar_lad_gate_schema)) {
    logvar_lad_gate_stop(
      "logvar_lad_gate: unexpected schema_version",
      "logvar_lad_gate_schema"
    )
  }
  if (!identical(fields$gate, logvar_lad_gate_name)) {
    logvar_lad_gate_stop(
      "logvar_lad_gate: unexpected gate identifier",
      "logvar_lad_gate_mismatch"
    )
  }
  if (!(fields$decision %in% logvar_lad_gate_decisions)) {
    logvar_lad_gate_stop(
      "logvar_lad_gate: decision outside the closed enum",
      "logvar_lad_gate_decision"
    )
  }
  if (!(fields$install_consent %in% logvar_lad_gate_consents)) {
    logvar_lad_gate_stop(
      "logvar_lad_gate: install_consent outside the closed enum",
      "logvar_lad_gate_consent"
    )
  }
  if (!identical(fields$decision, "approved") &&
    nzchar(fields$quantreg_version)) {
    logvar_lad_gate_stop(
      "logvar_lad_gate: quantreg_version recorded without an approved decision",
      "logvar_lad_gate_version"
    )
  }
  invisible(fields)
}

# Read the gate record and derive readiness from the injected availability and
# installed version. A missing file is "unanswered"; declined and unanswered
# source no LAD code; approved is ready (and sources LAD code) only with the
# package available and the recorded version matching the installed one.
logvar_lad_gate_read <- function(path, available, installed_version) {
  if (!file.exists(path)) {
    return(logvar_lad_gate_result("unanswered", FALSE, FALSE, "not_asked", ""))
  }
  fields <- logvar_lad_gate_parse(path)
  logvar_lad_gate_check_static(fields)
  decision <- fields$decision
  version <- fields$quantreg_version
  if (!identical(decision, "approved")) {
    return(logvar_lad_gate_result(
      decision, FALSE, FALSE, fields$install_consent, version
    ))
  }
  if (!isTRUE(available)) {
    logvar_lad_gate_stop(
      paste0(
        "logvar_lad_gate: approved but quantreg is unavailable; install it ",
        "with install.packages(\"quantreg\"), then refresh the decision record"
      ),
      "logvar_lad_gate_unavailable"
    )
  }
  if (!identical(version, as.character(installed_version))) {
    logvar_lad_gate_stop(
      paste0(
        "logvar_lad_gate: recorded quantreg version '", version,
        "' differs from installed '", as.character(installed_version),
        "'; refresh the decision record"
      ),
      "logvar_lad_gate_stale"
    )
  }
  logvar_lad_gate_result("approved", TRUE, TRUE, fields$install_consent, version)
}
