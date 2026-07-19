# Base-R decision core for the gated EGARCH-X extension (EGARCH routing protocol): the
# pinned provenance constants, the canonical SHA-256 helpers, and the checked-in
# default record builder. This module is the security scaffolding that runs
# before any dynamic-stage dependency access, so it names no heavy package: the
# core is dependency-agnostic (generic "dependency" naming, the two approval
# prompt texts are supplied by the committed record, and only their pinned hex
# SHA-256 constants live here). The strict validator and the pure branch router
# are chain-sourced from route_core.R below. Definitions only;
# sourced by scripts-paper/config/decisions/egarch.R, by the route driver, and by
# the tests before any record is built or routed.
#
# The ladder (Design Decision 1): the base-R gate runs first; a non-reject or an
# unreliable verdict stops the dynamic workstream as a complete success with both
# ordered decisions `not_asked`. Only a rejection opens the two separate user
# approvals (the changed estimand, then the heavy dependency), and only both
# approvals plus a present, version-matched package let run_dynamic be TRUE.

# Pinned provenance/version constants and the closed enums -------------------
paper_source_once(paper_path(
  "log_variance", "diagnostics", "protocols.R"
))

LOGVAR_EGARCH_SCHEMA_VERSION <- "1.1.0"
LOGVAR_EGARCH_PLAN_SHA256 <-
  "978939283a5b6541adf0e448789c77863961359dbb008c4f40f0728500759fb9"
LOGVAR_EGARCH_GATE_RECORD_PATH <-
  artifact_path("dynamics_gate")
LOGVAR_EGARCH_UPSTREAM_PLANS_HASH <-
  "046d845cf1d8a2cda86e0a2946894cae8817ca31622dfed8a20185a13e823d38"
# the approved dependency version (kept as a bare version literal, never a
# namespace/require, so the core stays package-agnostic and grep-clean)
LOGVAR_EGARCH_DEP_VERSION <- "1.5-5"
LOGVAR_EGARCH_DECISION_VALUES <-
  c("approved", "declined", "no_answer", "not_asked")
LOGVAR_EGARCH_PROVENANCE_VALUES <-
  c("not_asked_default", "user_response", "no_answer_default")
LOGVAR_EGARCH_TERMINAL_VALUES <- c(
  "gate_non_reject", "gate_unreliable", "estimand_declined",
  "dependency_declined", "no_answer", "reject_awaiting_answer",
  "approved_pending_dynamic", "approved_but_missing_package"
)
# canonical SHA-256 of the two verbatim approval prompt texts (the texts
# themselves are in config/decisions/egarch.R); pinning
# the hashes here binds the questions the approvals answer without importing the
# dependency name into the core
LOGVAR_EGARCH_ESTIMAND_PROMPT_SHA256 <-
  "e49332b8b5879ffe0caf0a5a9534af39ffb98e99f94e76ac5c861c6f83f01442"
LOGVAR_EGARCH_DEPENDENCY_PROMPT_SHA256 <-
  "f2fdf49bf1a45e7c6899299798db93512597dc096b39b2494d9b3d888134a4c8"

# The ordered decision schema: version, the frozen gate binding (canonical hash
# and path plus the copied gate/sample/verdict fields), the plan/upstream
# provenance hashes, the two prompt texts and their SHA-256, the ordered
# decisions, the response provenance, and the fixed UTC timestamp.
logvar_egarch_decision_fields <- c(
  "schema_version", "gate_science_sha256", "gate_record_path", "sample_id",
  "gate_lag", "gate_alpha", "gate_q", "gate_p", "gate_verdict",
  "plan_sha256", "upstream_plans_hash", "estimand_prompt",
  "estimand_prompt_sha256", "dependency_prompt", "dependency_prompt_sha256",
  "decisions", "decision_provenance", "decided_at_utc"
)

# Canonical hashes delegate to the paper runtime primitives.
logvar_egarch_object_sha256 <- function(obj) {
  paper_sha256_object(obj)
}
logvar_egarch_string_sha256 <- function(s) {
  paper_sha256_string(s)
}
# decision-evidence hash: exclude benchmark_commit (provenance) and the
# descriptive sensitivity witnesses, which never affect the tau = 0 verdict
logvar_egarch_gate_science_sha256 <- function(gate_record) {
  scientific <- gate_record
  scientific$benchmark_commit <- NULL
  scientific$sensitivity <- NULL
  logvar_egarch_object_sha256(scientific)
}

# Raise a classed condition so the driver and the tests can dispatch on the exact
# failure family; every validation and routing failure uses this.
logvar_egarch_decision_stop <- function(reason, detail = "") {
  paper_stop_condition(
    reason,
    "logvar_egarch_decision_error",
    detail
  )
}

# The canonical provenance a decision pair implies, which the validator enforces:
# no question -> not_asked_default; an unanswered question -> no_answer_default;
# any user answer (approved or declined) -> user_response.
logvar_egarch_expected_provenance <- function(decisions) {
  if (all(decisions == "not_asked")) {
    "not_asked_default"
  } else if (any(decisions == "no_answer")) {
    "no_answer_default"
  } else {
    "user_response"
  }
}

# Deterministic builder for tests and future approval updates. It freezes the
# supplied gate's scientific hash and fields, validates canonical prompts, and
# defaults both ordered decisions to `not_asked`.
logvar_egarch_decision_default <- function(gate_record, decided_at_utc,
                                           estimand_prompt, dependency_prompt,
                                           gate_record_path =
                                             LOGVAR_EGARCH_GATE_RECORD_PATH,
                                           plan_sha256 =
                                             LOGVAR_EGARCH_PLAN_SHA256) {
  e_sha <- logvar_egarch_string_sha256(estimand_prompt)
  d_sha <- logvar_egarch_string_sha256(dependency_prompt)
  if (!identical(e_sha, LOGVAR_EGARCH_ESTIMAND_PROMPT_SHA256) ||
    !identical(d_sha, LOGVAR_EGARCH_DEPENDENCY_PROMPT_SHA256)) {
    logvar_egarch_decision_stop(
      "noncanonical_prompt_text",
      "a supplied approval prompt does not match its pinned canonical SHA-256"
    )
  }
  lag_name <- sprintf("lag%d", as.integer(gate_record$gate_lag))
  list(
    schema_version = LOGVAR_EGARCH_SCHEMA_VERSION,
    gate_science_sha256 =
      logvar_egarch_gate_science_sha256(gate_record),
    gate_record_path = gate_record_path, sample_id = gate_record$sample_id,
    gate_lag = as.integer(gate_record$gate_lag),
    gate_alpha = gate_record$gate_alpha,
    gate_q = unname(gate_record$q_stats[[lag_name]]),
    gate_p = unname(gate_record$p_values[[lag_name]]),
    gate_verdict = gate_record$verdict,
    plan_sha256 = plan_sha256,
    upstream_plans_hash = LOGVAR_EGARCH_UPSTREAM_PLANS_HASH,
    estimand_prompt = estimand_prompt, estimand_prompt_sha256 = e_sha,
    dependency_prompt = dependency_prompt, dependency_prompt_sha256 = d_sha,
    decisions = c(estimand = "not_asked", dependency = "not_asked"),
    decision_provenance = "not_asked_default", decided_at_utc = decided_at_utc
  )
}

# The routing status manifest: the gate context plus the routing outcome, the
# ordered decisions and provenance, the four-artifact absence proof, and the
# pre-gate cleanup audit. Overwrites the gate driver manifest so downstream
# stages and the ledger read a single post-routing status.
logvar_egarch_route_status <- function(gate_record, route, decision,
                                       dynamic_artifacts_absent,
                                       cleanup_audit = NULL) {
  lag_name <- sprintf("lag%d", as.integer(gate_record$gate_lag))
  protocol <- gate_record$protocol
  if (is.null(protocol)) {
    protocol <- LOGVAR_DYNAMICS_GATE_PROTOCOL
  }
  list(
    schema_version = LOGVAR_EGARCH_SCHEMA_VERSION,
    protocol = protocol,
    stage = "egarch_route",
    plan = "logvar-egarch-x", sample_id = gate_record$sample_id,
    benchmark_commit = gate_record$benchmark_commit,
    gate_verdict = gate_record$verdict,
    gate_lag = as.integer(gate_record$gate_lag),
    gate_alpha = gate_record$gate_alpha,
    gate_q = unname(gate_record$q_stats[[lag_name]]),
    gate_p = unname(gate_record$p_values[[lag_name]]),
    min_abs_eps = gate_record$min_abs_eps,
    crossing_status = gate_record$crossing_status,
    estimand_decision = decision$decisions[["estimand"]],
    dependency_decision = decision$decisions[["dependency"]],
    decision_provenance = decision$decision_provenance,
    decided_at_utc = decision$decided_at_utc,
    run_dynamic = isTRUE(route$run_dynamic),
    terminal_status = route$terminal_status, skip_reason = route$skip_reason,
    decision_pending = identical(route$terminal_status, "reject_awaiting_answer"),
    workstream_status = route$terminal_status,
    dynamic_artifacts_absent = isTRUE(dynamic_artifacts_absent),
    cleanup_audit = cleanup_audit,
    gate_record_path = LOGVAR_EGARCH_GATE_RECORD_PATH
  )
}

# the strict decision/gate-binding validator and the pure branch router
paper_source_once(paper_path("log_variance", "extensions", "egarch", "route_core.R"))
