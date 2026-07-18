# Selector-provenance contract checks. The coverage fixtures and completed
# apply results are defined in coverage_checks.R, sourced immediately before
# this file.

check(
  "coverage metadata is derived from verified engine selector diagnostics",
  identical(
    pcov_a$metadata$version,
    LOGVAR_PPML_COVERAGE_PROTOCOL$schema_version
  ) &&
    identical(
      pcov_a$metadata$selector_id,
      LOGVAR_PPML_COVERAGE_PROTOCOL$selector_id
    ) &&
    identical(pcov_a$metadata$selector_provenance_status, "verified") &&
    identical(pcov_a$metadata$selector_runs_verified, 1L)
)

check(
  "an all-failed coverage run records no invented selector provenance",
  is.na(pcov_b$metadata$selector_id) &&
    is.na(pcov_b$metadata$selector_traversal) &&
    identical(pcov_b$metadata$selector_provenance_status, "all_failed") &&
    identical(pcov_b$metadata$selector_runs_verified, 0L)
)

pcov_selector_error <- function(coverage) {
  tryCatch(
    {
      logvar_ppml_apply_coverage(
        pcov_primary, coverage,
        selector_protocol = LOGVAR_PPML_COVERAGE_PROTOCOL
      )
      NA_character_
    },
    error = conditionMessage
  )
}
pcov_tampered <- pcov_find
pcov_tampered[[pcov_key]]$res$diagnostics$selector$selector_id <- "tampered"
pcov_absent <- setNames(list(list(
  ok = TRUE, res = pcov_res(pcov_schema(
    c(-1, -2), c(1, 2), "bounded", "bounded", 0.05
  ))
)), pcov_key)
check(
  "completed coverage runs reject absent or mismatched selector provenance",
  grepl("absent", pcov_selector_error(pcov_absent), fixed = TRUE) &&
    grepl("does not match", pcov_selector_error(pcov_tampered), fixed = TRUE)
)
