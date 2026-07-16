# The committed EGARCH-X scope decision for this run (EGARCH routing protocol), mirroring
# the joint-GMM joint_gmm.R committed-record pattern. The base-R
# residual-dynamics gate returned `non_reject` (lag-4 Ljung-Box p = 0.184), which
# per the ladder stops the dynamic workstream as a complete success: neither the
# changed-estimand nor the dependency approval is asked, both ordered decisions
# are `not_asked`, and no heavy package is touched. The record still binds the
# fresh gate record's canonical SHA-256, the sample id and gate fields, the
# current-plan and upstream-plans hashes, and the two verbatim prompt texts with
# their SHA-256, so a future rejecting run cannot reuse a stale approval or
# silently reword a question the approvals answered.
#
# The two verbatim approval prompt texts live here (not in the dependency-
# agnostic core) and are passed to the builder, which fails fast unless each
# matches its pinned canonical hash. decided_at_utc is a fixed literal (never
# Sys.time()) so the record is stable across runs. If a future run rejects and
# both approvals are obtained, build the record from the real answers with
# decision_provenance = "user_response" rather than sourcing this default.

source(paper_path("log_variance", "extensions", "egarch", "decision_core.R"))

# The exact changed-estimand question (recorded for the binding; shown only on a
# rejecting gate). Mirrors the plan's section-1 recursion and changed-estimand
# framing.
logvar_egarch_estimand_prompt <- paste(
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

# The exact dependency question (the one production string that names the heavy
# package, and only as recorded content -- never a namespace/require). Mirrors
# the plan's Tech-stack and Global-Constraints dependency framing.
logvar_egarch_dependency_prompt <- paste(
  "The dynamic EGARCH-X stage requires the rugarch package (heavy: it imports",
  "Rsolnp, xts, zoo, numDeriv, and nloptr; CRAN 1.5-5 as of 2026-03-15) and is an",
  "explicit user decision, never installed silently. If declined, the deliverable",
  "is the gate diagnostic plus this recorded decision; the hand-rolled recursion",
  "fallback is built only if the dependency is declined and the user explicitly",
  "requests it, with validation time budgeted. Approve adding the rugarch 1.5-5",
  "dependency for the dynamic stage?"
)

logvar_egarch_decision <- logvar_egarch_decision_default(
  gate_record = readRDS(LOGVAR_EGARCH_GATE_RECORD_PATH),
  decided_at_utc = "2026-07-15T04:35:40Z",
  estimand_prompt = logvar_egarch_estimand_prompt,
  dependency_prompt = logvar_egarch_dependency_prompt
)
