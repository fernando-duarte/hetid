# Shared fixtures for the EGARCH-X decision and approval checks:
# the house pass/fail counters and check helpers, the premature-package-access
# sentinel, the canonical approval prompts, and the synthetic gate/decision
# builders. Sourced by test_approval.R after the decision core so
# the builders can call it. Base R only. Definitions and counters only.

.pass <- 0L
.fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    .pass <<- .pass + 1L
    cat(sprintf("PASS  %s\n", label))
  } else {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s\n", label))
  }
}
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

# The canonical approval prompts, identical to the committed record and anchored
# to the pinned hashes so this test and the record cannot silently diverge.
est_prompt <- paste(
  "The residual-dynamics gate rejected: the tau = 0 log-variance residual shows",
  "quarterly serial correlation. The EGARCH(1,1)-X recursion",
  "log h_{t+1} = omega + R_t-prime delta + alpha z_t + gamma(|z_t| - E|z|) +",
  "beta log h_t conditions on the volatility state, so the lagged return-PC",
  "coefficients delta are dynamic conditional partial effects while the static",
  "theta_R mixes direct effects, persistence, and state correlations. This is a",
  "changed estimand, a different population object, justified only by the gate",
  "evidence of serial dependence in the static innovation. Approve estimating and",
  "reporting these dynamic conditional partial effects (theta_R^EGARCH = delta)?"
)
dep_prompt <- paste(
  "The dynamic EGARCH-X stage requires the rugarch package (heavy: it imports",
  "Rsolnp, xts, zoo, numDeriv, and nloptr; CRAN 1.5-5 as of 2026-03-15) and is an",
  "explicit user decision, never installed silently. If declined, the deliverable",
  "is the gate diagnostic plus this recorded decision; the hand-rolled recursion",
  "fallback is built only if the dependency is declined and the user explicitly",
  "requests it, with validation time budgeted. Approve adding the rugarch 1.5-5",
  "dependency for the dynamic stage?"
)

# A fixed plan-hash constant so the validator tests never read the plan file.
test_plan_sha <- "test_plan_sha256_constant"

# A synthetic gate record carrying only the fields the builder and validator read.
mk_gate <- function(verdict, sample_id = "sample_A", commit = "abc123",
                    q = 12.3, p = 0.004, lag = 4L, alpha = 0.05) {
  nm <- sprintf("lag%d", c(1L, lag, 8L))
  list(
    schema_version = "1.0.0", sample_id = sample_id, benchmark_commit = commit,
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
    gate, "2026-07-15T00:00:00Z", est_prompt, dep_prompt,
    plan_sha256 = test_plan_sha
  )
  rec$decisions <- c(estimand = d1, dependency = d2)
  rec$decision_provenance <- prov
  rec
}
