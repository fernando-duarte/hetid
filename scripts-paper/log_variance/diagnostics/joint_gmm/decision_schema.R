# Pure decision-record helpers for the joint moment-compatibility gate (joint-GMM,
# logvar-joint-gmm): the canonical spec-ID, the checked-in no-answer default, and
# the exact-schema validator the driver runs before any substantive work. No
# side effects, no I/O beyond the canonical paper_md5_rds spec-ID helper
# (mirrors logvar_sample_id). Sourced by scripts-paper/config/decisions/joint_gmm.R
# and by the tests before the record itself. Definitions only.

paper_source_once(paper_path(
  "support", "statistics", "normalizations.R"
))

LOGVAR_JOINT_DECISION_SCHEMA_VERSION <- "1.0.0"

# The ordered schema: the schema version, the canonical spec ID, the six scientific
# choice fields, and the provenance/date/rationale bookkeeping.
logvar_joint_decision_fields <- c(
  "schema_version", "decision_spec_id", "enable_z", "enable_log_ppml",
  "intercept_target", "stage_c_requested", "moment_delta",
  "gaussian_gap_restriction", "decided_on", "decision_provenance", "rationale"
)
# The six choice fields that (with the schema version) define the spec ID. Neither
# the date nor the rationale enters the ID, so the stamp is stable across runs.
logvar_joint_decision_choice_fields <- c(
  "enable_z", "enable_log_ppml", "intercept_target", "stage_c_requested",
  "moment_delta", "gaussian_gap_restriction"
)
logvar_joint_decision_rationale_keys <- c(
  "z", "log_ppml", "intercept", "stage_c", "moment_delta", "gaussian_gap"
)

# Canonical spec ID over the schema version and the six choices. Deltas serialize
# as sorted-unique 17-significant-digit strings so the ID is representation-stable;
# the payload is written with the pinned RDS version and hashed with base tools.
logvar_joint_decision_spec_id <- function(fields) {
  delta <- sort(unique(as.numeric(fields$moment_delta)))
  payload <- list(
    schema_version = as.character(fields$schema_version),
    enable_z = as.logical(fields$enable_z),
    enable_log_ppml = as.logical(fields$enable_log_ppml),
    intercept_target = as.character(fields$intercept_target),
    stage_c_requested = as.logical(fields$stage_c_requested),
    moment_delta = paper_numeric_key(delta),
    gaussian_gap_restriction = as.logical(fields$gaussian_gap_restriction)
  )
  paper_md5_rds(payload)
}

# The checked-in no-answer default: an all-FALSE record with the recommended a_L
# intercept target, an empty delta grid, and explicit rationale. decided_on is a
# fixed ISO date (never Sys.Date(), which would mutate the record each run). The
# spec ID is computed from the choices so the record is self-consistent.
logvar_joint_decision_default <- function(decided_on) {
  rec <- list(
    schema_version = LOGVAR_JOINT_DECISION_SCHEMA_VERSION,
    decision_spec_id = NA_character_,
    enable_z = FALSE, enable_log_ppml = FALSE, intercept_target = "a_L",
    stage_c_requested = FALSE, moment_delta = numeric(0),
    gaussian_gap_restriction = FALSE, decided_on = decided_on,
    decision_provenance = "no_answer_default",
    rationale = c(
      z = paste(
        "E[Z-tilde xi] = 0 is a new variance-equation exclusion restriction, not",
        "implied by Lewbel's moments; unratified, so the z block stays off."
      ),
      log_ppml = paste(
        "The joint log/PPML overidentifying block is a substantive assumption;",
        "unratified, so only the just-identified graph replication runs."
      ),
      intercept = paste(
        "a_L is the benchmark mean-log normalization (primary reported target);",
        "a_P and the estimated Jensen gap are companions, not the baseline."
      ),
      stage_c = "No projected region requested; stop at Stage A + graph replication.",
      moment_delta = paste(
        "No ratified tolerance grid, so no sample moment-compatibility region is",
        "defined (a tolerance is never relabeled an identified set)."
      ),
      gaussian_gap = paste(
        sprintf(
          "The a_P - a_L = %s Gaussian-gap restriction is a distributional",
          logvar_normal_gap_text(9L)
        ),
        "robustness exercise, never the baseline, and is not implemented this round."
      )
    )
  )
  rec$decision_spec_id <- logvar_joint_decision_spec_id(rec)
  rec
}

# Stop with a classed joint-gmm decision error (mirrors the house structured
# conditions) so tests can dispatch on the exact reason string.
logvar_joint_decision_stop <- function(reason, detail = "") {
  paper_stop_condition(
    reason,
    "logvar_joint_decision_error",
    detail
  )
}

# Exact-schema validation the driver runs before any substantive work: field
# names in order, scalar types, the closed intercept/provenance enums, sorted
# finite nonnegative deltas, the scalar-FALSE gaussian-gap reservation, and a
# fresh spec-ID recomputation. Returns the record invisibly on success.
logvar_joint_decision_validate <- function(rec) {
  if (!is.list(rec) || !identical(names(rec), logvar_joint_decision_fields)) {
    logvar_joint_decision_stop("malformed_decision_record", "field names/order")
  }
  if (!identical(
    rec$schema_version,
    LOGVAR_JOINT_DECISION_SCHEMA_VERSION
  )) {
    logvar_joint_decision_stop("unsupported_schema_version", rec$schema_version)
  }
  for (f in c(
    "enable_z", "enable_log_ppml", "stage_c_requested",
    "gaussian_gap_restriction"
  )) {
    if (!is.logical(rec[[f]]) || length(rec[[f]]) != 1L || is.na(rec[[f]])) {
      logvar_joint_decision_stop("malformed_decision_record", f)
    }
  }
  if (!isFALSE(rec$gaussian_gap_restriction)) {
    logvar_joint_decision_stop(
      "unsupported_gaussian_gap_restriction",
      "the constrained-gap solver is not implemented this round"
    )
  }
  if (!rec$intercept_target %in% c("a_L", "a_P", "both")) {
    logvar_joint_decision_stop("malformed_decision_record", "intercept_target")
  }
  if (!rec$decision_provenance %in% c("user_ratified", "no_answer_default")) {
    logvar_joint_decision_stop("malformed_decision_record", "decision_provenance")
  }
  d <- rec$moment_delta
  if (!is.numeric(d) || anyNA(d) || any(!is.finite(d)) || any(d < 0) ||
    is.unsorted(d, strictly = TRUE)) {
    logvar_joint_decision_stop(
      "malformed_moment_delta",
      "deltas must be sorted-unique finite nonnegative"
    )
  }
  if (!is.character(rec$rationale) ||
    !identical(names(rec$rationale), logvar_joint_decision_rationale_keys)) {
    logvar_joint_decision_stop("malformed_decision_record", "rationale keys")
  }
  fresh <- logvar_joint_decision_spec_id(rec)
  if (!identical(rec$decision_spec_id, fresh)) {
    logvar_joint_decision_stop(
      "stale_decision_spec_id",
      sprintf("recorded %s vs fresh %s", rec$decision_spec_id, fresh)
    )
  }
  invisible(rec)
}
