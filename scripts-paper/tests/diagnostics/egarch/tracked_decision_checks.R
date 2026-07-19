# Output-independent checks for the immutable tracked EGARCH decision and every
# field in its validation contract. Executed by test_approval.R after loading the
# production record and the matching scientific-gate fixture.

expected_plan_sha <-
  "978939283a5b6541adf0e448789c77863961359dbb008c4f40f0728500759fb9"
expected_estimand_prompt_sha <-
  "e49332b8b5879ffe0caf0a5a9534af39ffb98e99f94e76ac5c861c6f83f01442"
expected_dependency_prompt_sha <-
  "f2fdf49bf1a45e7c6899299798db93512597dc096b39b2494d9b3d888134a4c8"
expected_gate_science_sha <-
  "ccb92931d63a33e01c157006dd1bb6bc9800ff68455d325d7079247faad76b7d"

check("tracked plan digest is the exact approved digest", identical(
  LOGVAR_EGARCH_PLAN_SHA256,
  expected_plan_sha
))
check("tracked record carries the exact approved plan digest", identical(
  logvar_egarch_decision$plan_sha256,
  expected_plan_sha
))
check("dependency version is the exact approved version", identical(
  LOGVAR_EGARCH_DEP_VERSION,
  "1.5-5"
))
check("estimand prompt is the exact committed prompt", identical(
  logvar_egarch_decision$estimand_prompt,
  logvar_egarch_estimand_prompt
) && identical(
  logvar_egarch_string_sha256(logvar_egarch_estimand_prompt),
  expected_estimand_prompt_sha
))
check("dependency prompt is the exact committed prompt", identical(
  logvar_egarch_decision$dependency_prompt,
  logvar_egarch_dependency_prompt
) && identical(
  logvar_egarch_string_sha256(logvar_egarch_dependency_prompt),
  expected_dependency_prompt_sha
))

committed_gate <- egarch_committed_gate_fixture()
check("scientific fixture has the exact committed digest", identical(
  logvar_egarch_gate_science_sha256(committed_gate),
  expected_gate_science_sha
))
check("tracked record carries the exact scientific digest", identical(
  logvar_egarch_decision$gate_science_sha256,
  expected_gate_science_sha
))
check("tracked decision validates against the matching synthetic gate", {
  logvar_egarch_decision_validate(logvar_egarch_decision, committed_gate)
  TRUE
})

tampered_values <- list(
  schema_version = "tampered",
  gate_science_sha256 = paste(rep("0", 64L), collapse = ""),
  gate_record_path = paste0(LOGVAR_EGARCH_GATE_RECORD_PATH, ".tampered"),
  sample_id = paste0(logvar_egarch_decision$sample_id, "_tampered"),
  gate_lag = logvar_egarch_decision$gate_lag + 1L,
  gate_alpha = logvar_egarch_decision$gate_alpha + 0.01,
  gate_q = logvar_egarch_decision$gate_q + 1,
  gate_p = logvar_egarch_decision$gate_p + 0.01,
  gate_verdict = "reject",
  plan_sha256 = paste(rep("1", 64L), collapse = ""),
  upstream_plans_hash = paste(rep("2", 64L), collapse = ""),
  estimand_prompt = paste(logvar_egarch_decision$estimand_prompt, "tampered"),
  estimand_prompt_sha256 = paste(rep("3", 64L), collapse = ""),
  dependency_prompt = paste(logvar_egarch_decision$dependency_prompt, "tampered"),
  dependency_prompt_sha256 = paste(rep("4", 64L), collapse = ""),
  decisions = unname(logvar_egarch_decision$decisions),
  decision_provenance = "tampered",
  decided_at_utc = ""
)
stopifnot(identical(names(tampered_values), logvar_egarch_decision_fields))
tamper_reasons <- c(
  schema_version = "unsupported_schema_version",
  gate_science_sha256 = "gate_record_hash_mismatch",
  gate_record_path = "gate_record_path_mismatch",
  sample_id = "gate_field_mismatch",
  gate_lag = "gate_field_mismatch",
  gate_alpha = "gate_field_mismatch",
  gate_q = "gate_field_mismatch",
  gate_p = "gate_field_mismatch",
  gate_verdict = "gate_field_mismatch",
  plan_sha256 = "stale_plan_sha256",
  upstream_plans_hash = "stale_upstream_plans_hash",
  estimand_prompt = "estimand_prompt_hash_mismatch",
  estimand_prompt_sha256 = "estimand_prompt_hash_mismatch",
  dependency_prompt = "dependency_prompt_hash_mismatch",
  dependency_prompt_sha256 = "dependency_prompt_hash_mismatch",
  decisions = "malformed_decision_record",
  decision_provenance = "malformed_decision_record",
  decided_at_utc = "malformed_decision_record"
)
stopifnot(identical(names(tamper_reasons), logvar_egarch_decision_fields))

for (field in logvar_egarch_decision_fields) {
  tampered <- logvar_egarch_decision
  tampered[[field]] <- tampered_values[[field]]
  check(
    sprintf("tracked binding rejects independently tampered %s", field),
    expect_stop(
      logvar_egarch_decision_validate(tampered, committed_gate),
      tamper_reasons[[field]]
    )
  )
}
