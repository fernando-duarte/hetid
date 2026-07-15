# Thin, unconditionally-sourced router driver for the gated EGARCH-X extension
# (Plan 5, Task 3). It validates the committed scope decision against the freshly
# regenerated gate record, calls the pure router, prints every terminal status
# and skip reason in a marker-wrapped block, rewrites the status manifest with
# the routing outcome and the pre-gate cleanup audit, asserts that all four
# dynamic-only artifacts are absent, and exposes logvar_egarch_run_dynamic for
# run_all.R. It sources no dynamic module: Task 5 adds the conditional source
# lines that consume logvar_egarch_run_dynamic. Dependency availability and
# version are read from optional variables (Task 5 sets them only on the approved
# branch), so this driver never touches the heavy package. Sourcing offline
# without a gate record present is a safe no-op.

.egarch_out <- if (exists("out_dir")) out_dir else "scripts-paper/output"
.egarch_gate_rds <- file.path(.egarch_out, "log_var_eq_dynamics_gate.rds")
.egarch_status_rds <- file.path(.egarch_out, "log_var_eq_egarch_status.rds")

if (file.exists(.egarch_gate_rds)) {
  if (!exists("logvar_egarch_route")) {
    source("scripts-paper/log_var_eq_egarch_decision_core.R")
  }
  if (!exists("logvar_egarch_dynamic_artifacts")) {
    source("scripts-paper/log_var_eq_egarch_cleanup.R")
  }
  source("scripts-paper/logvar_egarch_decision.R")

  # validate the committed decision against the fresh gate record; a mismatch
  # raises a classed condition here, before any routing or dependency lookup
  .egarch_gate <- readRDS(.egarch_gate_rds)
  logvar_egarch_decision_validate(logvar_egarch_decision, .egarch_gate)

  # dependency availability/version are consulted only on the approved branch;
  # Task 5 sets these variables, so this run leaves them at the safe default and
  # never touches the package
  .egarch_dep_available <- if (exists("logvar_egarch_dep_available")) {
    isTRUE(logvar_egarch_dep_available)
  } else {
    FALSE
  }
  .egarch_dep_version <- if (exists("logvar_egarch_dep_version")) {
    as.character(logvar_egarch_dep_version)
  } else {
    NA_character_
  }

  logvar_egarch_route_result <- logvar_egarch_route(
    logvar_egarch_decision, .egarch_dep_available, .egarch_dep_version
  )
  logvar_egarch_run_dynamic <- isTRUE(logvar_egarch_route_result$run_dynamic)

  # prove the four dynamic-only artifacts are absent (cleanup removed them and no
  # dynamic module has run); a stray artifact is a hard stop
  .egarch_dyn_paths <- logvar_egarch_dynamic_artifacts(.egarch_out)
  .egarch_absent <- !file.exists(.egarch_dyn_paths)
  stopifnot(all(.egarch_absent))

  .egarch_audit <- if (exists("logvar_egarch_cleanup_audit")) {
    logvar_egarch_cleanup_audit
  } else {
    NULL
  }
  log_var_eq_egarch_status <- logvar_egarch_route_status(
    .egarch_gate, logvar_egarch_route_result, logvar_egarch_decision,
    dynamic_artifacts_absent = all(.egarch_absent), cleanup_audit = .egarch_audit
  )

  cat("[BEGIN LOGVAR EGARCH ROUTE]\n")
  cat(sprintf("  gate verdict: %s\n", logvar_egarch_decision$gate_verdict))
  cat(sprintf(
    "  decisions: estimand=%s dependency=%s (provenance %s)\n",
    logvar_egarch_decision$decisions[["estimand"]],
    logvar_egarch_decision$decisions[["dependency"]],
    logvar_egarch_decision$decision_provenance
  ))
  cat(sprintf(
    "  terminal status: %s\n", logvar_egarch_route_result$terminal_status
  ))
  cat(sprintf("  run_dynamic: %s\n", logvar_egarch_run_dynamic))
  cat(sprintf("  skip reason: %s\n", logvar_egarch_route_result$skip_reason))
  cat(sprintf("  dynamic artifacts absent: %s\n", all(.egarch_absent)))
  cat("[END LOGVAR EGARCH ROUTE]\n")

  unlink(.egarch_status_rds)
  saveRDS(log_var_eq_egarch_status, .egarch_status_rds, version = 3)

  rm(
    .egarch_gate, .egarch_dep_available, .egarch_dep_version,
    .egarch_dyn_paths, .egarch_absent, .egarch_audit
  )
}
rm(.egarch_out, .egarch_gate_rds, .egarch_status_rds)
