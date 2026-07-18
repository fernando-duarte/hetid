# The committed EGARCH-X scope decision for this run (EGARCH routing protocol), mirroring
# the joint-GMM joint_gmm.R committed-record pattern. The base-R
# residual-dynamics gate returned `non_reject` (lag-4 Ljung-Box p = 0.184), which
# per the ladder stops the dynamic workstream as a complete success: neither the
# changed-estimand nor the dependency approval is asked, both ordered decisions
# are `not_asked`, and no heavy package is touched. The record still binds the
# fresh gate record's scientific SHA-256, the sample id and gate fields, the
# approved-plan and upstream-plans hashes, and the two prompt texts with
# their SHA-256, so a future rejecting run cannot reuse a stale approval or
# silently reword a question the approvals answered.
#
# The immutable record is self-contained and never reads an ignored plan or a
# generated gate while it is constructed. The validator binds it to the freshly
# generated gate before routing.

paper_source_once(paper_path("log_variance", "extensions", "egarch", "decision_core.R"))

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
  sprintf(
    "Rsolnp, xts, zoo, numDeriv, and nloptr; CRAN %s as of 2026-03-15) and is an",
    LOGVAR_EGARCH_DEP_VERSION
  ),
  "explicit user decision, never installed silently. If declined, the deliverable",
  "is the gate diagnostic plus this recorded decision; the hand-rolled recursion",
  "fallback is built only if the dependency is declined and the user explicitly",
  sprintf(
    "requests it, with validation time budgeted. Approve adding the rugarch %s",
    LOGVAR_EGARCH_DEP_VERSION
  ),
  "dependency for the dynamic stage?"
)

logvar_egarch_decision <- list(
  schema_version = LOGVAR_EGARCH_SCHEMA_VERSION,
  gate_science_sha256 =
    "7890f2f95b7ac1358788fefdbbdc6f9d3edd41ced70ae40187f3b753f0dd5c22",
  gate_record_path = LOGVAR_EGARCH_GATE_RECORD_PATH,
  sample_id = "n255_1962 Q2_2025 Q4_24138f4ddc2adda6de40372667337c10",
  gate_lag = 4L,
  gate_alpha = 0.05,
  gate_q = 0x1.8d50411d73a96p+2,
  gate_p = 0x1.791f928a88b58p-3,
  gate_verdict = "non_reject",
  plan_sha256 = LOGVAR_EGARCH_PLAN_SHA256,
  upstream_plans_hash = LOGVAR_EGARCH_UPSTREAM_PLANS_HASH,
  estimand_prompt = logvar_egarch_estimand_prompt,
  estimand_prompt_sha256 = LOGVAR_EGARCH_ESTIMAND_PROMPT_SHA256,
  dependency_prompt = logvar_egarch_dependency_prompt,
  dependency_prompt_sha256 = LOGVAR_EGARCH_DEPENDENCY_PROMPT_SHA256,
  decisions = c(
    estimand = "not_asked",
    dependency = "not_asked"
  ),
  decision_provenance = "not_asked_default",
  decided_at_utc = "2026-07-15T04:35:40Z"
)
stopifnot(identical(
  names(logvar_egarch_decision),
  logvar_egarch_decision_fields
))
