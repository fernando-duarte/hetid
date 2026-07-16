# Enforcement half of the EGARCH-X decision core (EGARCH routing protocol): the strict
# decision/gate-binding validator, the ordered-decision ladder check, the pure
# branch router, and the routing status-manifest builder. Chain-sourced by
# decision_core.R after the record builder. Base R only and
# dependency-agnostic: the router receives dependency availability and version as
# plain arguments and never calls requireNamespace() or `::`, so this file is the
# security gate that runs and fails before any package access. Definitions only.

# Field-shape check for the ordered decisions, the provenance enum, and the
# timestamp. The decisions are the named length-two vector (estimand,
# dependency) with values in the closed enum; the ladder forbids answering the
# dependency question after anything but an approved estimand (Design Decision 2).
logvar_egarch_validate_decisions <- function(rec) {
  d <- rec$decisions
  if (!is.character(d) || !identical(names(d), c("estimand", "dependency"))) {
    logvar_egarch_decision_stop("malformed_decision_record", "decisions shape")
  }
  if (!all(d %in% LOGVAR_EGARCH_DECISION_VALUES)) {
    logvar_egarch_decision_stop("malformed_decision_record", "decision enum")
  }
  if (!identical(d[["estimand"]], "approved") &&
    !identical(d[["dependency"]], "not_asked")) {
    logvar_egarch_decision_stop(
      "inconsistent_decision_ladder",
      "the dependency was answered without an approved estimand"
    )
  }
  # verdict ladder: approvals exist only on a rejecting gate, so any non-reject
  # or unreliable verdict must carry both decisions not_asked (the ladder is law)
  if (!identical(rec$gate_verdict, "reject") && !all(d == "not_asked")) {
    logvar_egarch_decision_stop(
      "inconsistent_decision_ladder", "approvals recorded but the gate did not reject"
    )
  }
  if (!rec$decision_provenance %in% LOGVAR_EGARCH_PROVENANCE_VALUES) {
    logvar_egarch_decision_stop("malformed_decision_record", "provenance enum")
  }
  # provenance must match the decisions (single-sourced with the builder's stamp)
  if (!identical(rec$decision_provenance, logvar_egarch_expected_provenance(d))) {
    logvar_egarch_decision_stop("inconsistent_provenance", rec$decision_provenance)
  }
  if (!is.character(rec$decided_at_utc) || length(rec$decided_at_utc) != 1L ||
    !nzchar(rec$decided_at_utc)) {
    logvar_egarch_decision_stop("malformed_decision_record", "decided_at_utc")
  }
  invisible(rec)
}

# Match the copied gate/sample/verdict fields against the fresh gate object.
logvar_egarch_check_gate_fields <- function(rec, fresh_gate_record) {
  lag_name <- sprintf("lag%d", as.integer(fresh_gate_record$gate_lag))
  fresh <- list(
    sample_id = fresh_gate_record$sample_id,
    gate_lag = as.integer(fresh_gate_record$gate_lag),
    gate_alpha = fresh_gate_record$gate_alpha,
    gate_q = unname(fresh_gate_record$q_stats[[lag_name]]),
    gate_p = unname(fresh_gate_record$p_values[[lag_name]]),
    gate_verdict = fresh_gate_record$verdict,
    benchmark_commit = fresh_gate_record$benchmark_commit
  )
  for (f in names(fresh)) {
    if (!identical(rec[[f]], fresh[[f]])) {
      logvar_egarch_decision_stop("gate_field_mismatch", f)
    }
  }
  invisible(rec)
}

# Strict decision/gate binding, run before any dependency access. Recompute the
# gate record's canonical hash from the fresh gate object and match it; match the
# copied gate fields; recompute and match the plan hash, the upstream-plans hash,
# and each prompt's self-consistent and canonical SHA-256; then check the schema
# and the decision shapes. Any mismatch raises a classed condition, so both
# approvals are invalidated before the router or any requireNamespace() call.
logvar_egarch_decision_validate <- function(rec, fresh_gate_record,
                                            plan_sha256 =
                                              logvar_egarch_file_sha256(
                                                LOGVAR_EGARCH_PLAN_PATH
                                              ),
                                            upstream_plans_hash =
                                              LOGVAR_EGARCH_UPSTREAM_PLANS_HASH) {
  if (!is.list(rec) || !identical(names(rec), logvar_egarch_decision_fields)) {
    logvar_egarch_decision_stop("malformed_decision_record", "field names/order")
  }
  if (!identical(rec$schema_version, LOGVAR_EGARCH_SCHEMA_VERSION)) {
    logvar_egarch_decision_stop("unsupported_schema_version", rec$schema_version)
  }
  if (!identical(rec$gate_record_path, LOGVAR_EGARCH_GATE_RECORD_PATH)) {
    logvar_egarch_decision_stop("gate_record_path_mismatch", rec$gate_record_path)
  }
  if (!identical(
    rec$gate_record_sha256, logvar_egarch_object_sha256(fresh_gate_record)
  )) {
    logvar_egarch_decision_stop(
      "gate_record_hash_mismatch", "committed gate hash != fresh gate record"
    )
  }
  logvar_egarch_check_gate_fields(rec, fresh_gate_record)
  if (!identical(rec$plan_sha256, plan_sha256)) {
    logvar_egarch_decision_stop("stale_plan_sha256", "current-plan hash changed")
  }
  if (!identical(rec$upstream_plans_hash, upstream_plans_hash)) {
    logvar_egarch_decision_stop(
      "stale_upstream_plans_hash", "upstream-plans hash changed"
    )
  }
  check_prompt <- function(sha, text, pinned, reason) {
    if (!identical(sha, logvar_egarch_string_sha256(text)) ||
      !identical(sha, pinned)) {
      logvar_egarch_decision_stop(reason, "prompt edited")
    }
  }
  check_prompt(
    rec$estimand_prompt_sha256, rec$estimand_prompt,
    LOGVAR_EGARCH_ESTIMAND_PROMPT_SHA256, "estimand_prompt_hash_mismatch"
  )
  check_prompt(
    rec$dependency_prompt_sha256, rec$dependency_prompt,
    LOGVAR_EGARCH_DEPENDENCY_PROMPT_SHA256, "dependency_prompt_hash_mismatch"
  )
  logvar_egarch_validate_decisions(rec)
  invisible(rec)
}

# Pure branch router: given a validated decision and the injected dependency
# availability/version, return exactly list(run_dynamic, terminal_status,
# skip_reason, decision). run_dynamic is TRUE only when the gate rejected, both
# ordered decisions are `approved`, and the dependency is present at the approved
# version; every other branch is a recorded stop. An approved decision whose
# package is missing or version-mismatched is a hard fail (classed condition),
# never a silent skip. No package access happens here.
logvar_egarch_route <- function(decision, dep_available, dep_version,
                                approved_version = LOGVAR_EGARCH_DEP_VERSION) {
  out <- function(run, status, reason) {
    list(
      run_dynamic = run, terminal_status = status, skip_reason = reason,
      decision = decision
    )
  }
  verdict <- decision$gate_verdict
  d1 <- decision$decisions[["estimand"]]
  d2 <- decision$decisions[["dependency"]]
  if (identical(verdict, "unreliable")) {
    return(out(FALSE, "gate_unreliable", paste(
      "The gate could not be computed honestly at the tau = 0 point; the",
      "dynamic workstream stops after the diagnostic."
    )))
  }
  if (!identical(verdict, "reject")) {
    return(out(FALSE, "gate_non_reject", paste(
      "The predeclared lag-4 screen did not reject; the dynamic workstream stops",
      "as a complete deliverable and no approval is requested."
    )))
  }
  if (identical(d1, "no_answer") || identical(d2, "no_answer")) {
    return(out(FALSE, "no_answer", paste(
      "A scope question received no answer; defaulting to stop after the",
      "diagnostic."
    )))
  }
  if (identical(d1, "declined")) {
    return(out(FALSE, "estimand_declined", paste(
      "The changed-estimand approval was declined; the deliverable is the gate",
      "diagnostic plus this recorded decision."
    )))
  }
  if (identical(d1, "approved") && identical(d2, "declined")) {
    return(out(FALSE, "dependency_declined", paste(
      "The dependency approval was declined; the hand-rolled fallback is built",
      "only on an explicit further request with validation time budgeted."
    )))
  }
  if (!identical(d1, "approved") || !identical(d2, "approved")) {
    return(out(FALSE, "reject_awaiting_answer", paste(
      "The gate rejected but the ordered approvals are not both recorded yet;",
      "the dynamic stage stays closed until they are."
    )))
  }
  version_ok <- isTRUE(dep_available) && !is.na(dep_version) && identical(
    as.character(numeric_version(dep_version, strict = FALSE)),
    as.character(numeric_version(approved_version))
  )
  if (!version_ok) {
    logvar_egarch_decision_stop("approved_but_missing_package", sprintf(
      "both approvals landed but the approved version %s is required; found %s",
      approved_version,
      if (isTRUE(dep_available)) as.character(dep_version) else "absent"
    ))
  }
  out(TRUE, "approved_pending_dynamic", "")
}
