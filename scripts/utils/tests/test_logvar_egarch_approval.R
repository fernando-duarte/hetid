# Offline branch and stale-decision checks for the gated EGARCH-X decision core
# (Plan 5, Task 3). Every ladder branch of the pure router is exercised
# (non-reject, unreliable, no-answer, estimand decline, dependency decline, both
# approved with the package present, and the approved-but-missing / version-
# mismatch hard fails), together with the strict validator (changed gate hash,
# tampered sample id, stale plan and upstream hashes, edited prompt, broken
# ladder). A runtime sentinel proves no "rugarch" namespace is touched during
# validation or routing, and a static grep proves no production file references
# the package as a namespace/require. Fixtures and helpers live in the sourced
# fixtures file. Run from the worktree root:
#   Rscript scripts/utils/tests/test_logvar_egarch_approval.R

source(here::here("scripts-paper/log_var_eq_egarch_decision_core.R"))
source(here::here("scripts-paper/log_var_eq_egarch_cleanup.R"))
source(here::here("scripts/utils/tests/logvar_egarch_approval_fixtures.R"))

check("inline estimand prompt is canonical", identical(
  logvar_egarch_string_sha256(est_prompt), LOGVAR_EGARCH_ESTIMAND_PROMPT_SHA256
))
check("inline dependency prompt is canonical", identical(
  logvar_egarch_string_sha256(dep_prompt), LOGVAR_EGARCH_DEPENDENCY_PROMPT_SHA256
))

# Router ladder branches (pure; no package access) --------------------------
g_nr <- mk_gate("non_reject", q = 3, p = 0.30)
d_nr <- mk_dec(g_nr)
r_nr <- logvar_egarch_route(d_nr, FALSE, NA_character_)
check("non_reject: decisions both not_asked", identical(
  unname(d_nr$decisions), c("not_asked", "not_asked")
))
check("non_reject: run_dynamic FALSE", isFALSE(r_nr$run_dynamic))
check("non_reject: terminal gate_non_reject", identical(
  r_nr$terminal_status, "gate_non_reject"
))
check("non_reject: skip reason present", nzchar(r_nr$skip_reason))

g_un <- mk_gate("unreliable", q = NA_real_, p = NA_real_)
r_un <- logvar_egarch_route(mk_dec(g_un), FALSE, NA_character_)
check("unreliable: run_dynamic FALSE", isFALSE(r_un$run_dynamic))
check("unreliable: terminal gate_unreliable", identical(
  r_un$terminal_status, "gate_unreliable"
))

g_rj <- mk_gate("reject", q = 15, p = 0.001)
r_na <- logvar_egarch_route(
  mk_dec(g_rj, "no_answer", "not_asked", "no_answer_default"), FALSE, NA
)
check("no_answer: run_dynamic FALSE", isFALSE(r_na$run_dynamic))
check("no_answer: terminal no_answer", identical(r_na$terminal_status, "no_answer"))

r_ed <- logvar_egarch_route(
  mk_dec(g_rj, "declined", "not_asked", "user_response"), FALSE, NA
)
check("estimand decline: run_dynamic FALSE", isFALSE(r_ed$run_dynamic))
check("estimand decline: terminal estimand_declined", identical(
  r_ed$terminal_status, "estimand_declined"
))

r_dd <- logvar_egarch_route(
  mk_dec(g_rj, "approved", "declined", "user_response"), FALSE, NA
)
check("dependency decline: terminal dependency_declined", identical(
  r_dd$terminal_status, "dependency_declined"
))

d_ap <- mk_dec(g_rj, "approved", "approved", "user_response")
r_ap <- logvar_egarch_route(d_ap, TRUE, "1.5-5")
check("both approved + present: run_dynamic TRUE", isTRUE(r_ap$run_dynamic))
check("both approved + present: terminal approved_pending_dynamic", identical(
  r_ap$terminal_status, "approved_pending_dynamic"
))
check("both approved + present: skip reason empty", identical(r_ap$skip_reason, ""))
check("both approved + missing package: hard fail", expect_stop(
  logvar_egarch_route(d_ap, FALSE, NA_character_), "approved_but_missing_package"
))
check("both approved + version mismatch: hard fail", expect_stop(
  logvar_egarch_route(d_ap, TRUE, "1.4-9"), "approved_but_missing_package"
))

# Strict validator: binding and staleness -----------------------------------
check("valid record validates against its gate", {
  logvar_egarch_decision_validate(d_ap, g_rj, plan_sha256 = test_plan_sha)
  TRUE
})
check("changed gate hash invalidates", expect_stop(
  logvar_egarch_decision_validate(
    d_ap, mk_gate("reject", sample_id = "sample_B", q = 15, p = 0.001),
    plan_sha256 = test_plan_sha
  ), "gate_record_hash_mismatch"
))
d_tamper <- d_ap
d_tamper$sample_id <- "tampered"
check("tampered sample_id invalidates", expect_stop(
  logvar_egarch_decision_validate(d_tamper, g_rj, plan_sha256 = test_plan_sha),
  "gate_field_mismatch"
))
d_stale_plan <- d_ap
d_stale_plan$plan_sha256 <- "stale_plan_hash"
check("stale plan sha invalidates", expect_stop(
  logvar_egarch_decision_validate(d_stale_plan, g_rj, plan_sha256 = test_plan_sha),
  "stale_plan_sha256"
))
d_stale_up <- d_ap
d_stale_up$upstream_plans_hash <- "stale_upstream_hash"
check("stale upstream hash invalidates", expect_stop(
  logvar_egarch_decision_validate(d_stale_up, g_rj, plan_sha256 = test_plan_sha),
  "stale_upstream_plans_hash"
))
d_prompt <- d_ap
d_prompt$estimand_prompt <- paste(d_ap$estimand_prompt, "extra")
check("edited estimand prompt invalidates", expect_stop(
  logvar_egarch_decision_validate(d_prompt, g_rj, plan_sha256 = test_plan_sha),
  "estimand_prompt_hash_mismatch"
))
d_ladder <- d_ap
d_ladder$decisions <- c(estimand = "declined", dependency = "approved")
check("broken ladder invalidates", expect_stop(
  logvar_egarch_decision_validate(d_ladder, g_rj, plan_sha256 = test_plan_sha),
  "inconsistent_decision_ladder"
))
check("noncanonical prompt fails the builder", expect_stop(
  logvar_egarch_decision_default(
    g_rj, "2026-07-15T00:00:00Z", "not the real prompt", dep_prompt
  ), "noncanonical_prompt_text"
))

# No premature package access during validation/routing ---------------------
check("no rugarch namespace touched during validate/route", isFALSE(.rugarch_touched))

# Static guard: production files never access the package namespace ----------
prod_agnostic <- c(
  "scripts-paper/log_var_eq_egarch_decision_core.R",
  "scripts-paper/log_var_eq_egarch_route_core.R",
  "scripts-paper/log_var_eq_egarch_cleanup.R",
  "scripts-paper/log_var_eq_egarch_route.R"
)
has_token <- function(f) any(grepl("rugarch", readLines(here::here(f)), fixed = TRUE))
check("core/route/cleanup/driver hold no rugarch token", !any(vapply(
  prod_agnostic, has_token, logical(1)
)))
forbidden <- "requireNamespace\\(\\s*[\"']rugarch|library\\(\\s*rugarch|rugarch::"
accesses <- function(f) any(grepl(forbidden, readLines(here::here(f))))
all_prod <- c(prod_agnostic, "scripts-paper/logvar_egarch_decision.R")
check("no production file accesses the package namespace", !any(vapply(
  all_prod, accesses, logical(1)
)))
dec_lines <- readLines(here::here("scripts-paper/logvar_egarch_decision.R"))
check("committed record names the package only as content", any(grepl(
  "rugarch", dec_lines,
  fixed = TRUE
)))

# Integration: the committed record against the real gate (if present) -------
gate_rds <- here::here("scripts-paper/output/log_var_eq_dynamics_gate.rds")
if (file.exists(gate_rds)) {
  source(here::here("scripts-paper/logvar_egarch_decision.R"))
  real_gate <- readRDS(gate_rds)
  check("committed decision validates against the real gate", {
    logvar_egarch_decision_validate(logvar_egarch_decision, real_gate)
    TRUE
  })
  rc <- logvar_egarch_route(logvar_egarch_decision, FALSE, NA_character_)
  check("committed decision routes to gate_non_reject", identical(
    rc$terminal_status, "gate_non_reject"
  ))
  check("committed decisions are not_asked/not_asked", identical(
    unname(logvar_egarch_decision$decisions), c("not_asked", "not_asked")
  ))
  check("committed provenance is not_asked_default", identical(
    logvar_egarch_decision$decision_provenance, "not_asked_default"
  ))
  check("no package touched during committed validate/route", isFALSE(.rugarch_touched))
} else {
  cat("SKIP  real gate record absent; committed-record integration not run\n")
}

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
