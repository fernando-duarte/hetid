# Shared fixtures for the EGARCH-X decision and approval checks: the
# premature-package-access sentinel and synthetic gate/decision builders.
# Production prompts and approval metadata come only from the tracked decision
# configuration sourced by test_approval.R. Base R only.

# TRUE iff forcing expr raises an error (optionally of the given condition class)
expect_stop <- function(expr, subclass = NULL) {
  caught <- FALSE
  right <- TRUE
  tryCatch(force(expr), error = function(e) {
    caught <<- TRUE
    if (!is.null(subclass)) right <<- inherits(e, subclass)
  })
  caught && right
}

# Sentinel: sourced-script calls to requireNamespace resolve to this override, so
# any "rugarch" lookup during validation or routing flips the flag. The pure
# validator and router never call it, so the flag must stay FALSE.
.rugarch_touched <- FALSE
requireNamespace <- function(package, ...) {
  if (identical(package, "rugarch")) .rugarch_touched <<- TRUE
  base::requireNamespace(package, ...)
}

# A synthetic gate record carrying only the fields the builder and validator read.
mk_gate <- function(verdict, sample_id = "sample_A", commit = "abc123",
                    q = 12.3, p = 0.004, lag = 4L, alpha = 0.05) {
  nm <- sprintf("lag%d", c(1L, lag, 8L))
  list(
    schema_version = LOGVAR_DYNAMICS_GATE_PROTOCOL$version,
    protocol = LOGVAR_DYNAMICS_GATE_PROTOCOL,
    sample_id = sample_id, benchmark_commit = commit,
    gate_lag = lag, gate_alpha = alpha,
    q_stats = stats::setNames(c(1, q, 2), nm),
    p_values = stats::setNames(c(0.5, p, 0.6), nm), verdict = verdict, n = 200L
  )
}
# Build the default record from a gate, then override the ordered decisions and
# provenance to reach any ladder branch.
mk_dec <- function(gate, d1 = "not_asked", d2 = "not_asked",
                   prov = "not_asked_default") {
  rec <- logvar_egarch_decision_default(
    gate,
    "2026-07-15T00:00:00Z",
    logvar_egarch_estimand_prompt,
    logvar_egarch_dependency_prompt
  )
  rec$decisions <- c(estimand = d1, dependency = d2)
  rec$decision_provenance <- prov
  rec
}
