# Offline branch and stale-decision checks for the gated EGARCH-X decision core.
# The suite exercises every router branch, the strict validator, and the rule
# that validation and routing never touch the optional package namespace.
# Run from the worktree root:
#   Rscript scripts-paper/tests/diagnostics/egarch/test_approval.R

source(file.path("scripts-paper", "config", "paths.R"))
source(paper_path("config", "artifacts.R"))
source(paper_path("log_variance", "extensions", "egarch", "decision_core.R"))
source(paper_path("log_variance", "extensions", "egarch", "cleanup.R"))
source(paper_path("tests", "diagnostics", "egarch", "fixtures.R"))

check("inline estimand prompt is canonical", identical(
  logvar_egarch_string_sha256(est_prompt), LOGVAR_EGARCH_ESTIMAND_PROMPT_SHA256
))
check("inline dependency prompt is canonical", identical(
  logvar_egarch_string_sha256(dep_prompt), LOGVAR_EGARCH_DEPENDENCY_PROMPT_SHA256
))
check("file hash fails closed on a missing file", expect_stop(
  logvar_egarch_file_sha256(file.path(tempdir(), "no_such_plan_file.md")),
  "unreadable_file_hash"
))

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
check("both approved + dotted 1.5.5 normalizes to the dashed 1.5-5", isTRUE(
  logvar_egarch_route(d_ap, TRUE, "1.5.5")$run_dynamic
))

check("valid record validates against its gate", {
  logvar_egarch_decision_validate(d_ap, g_rj, plan_sha256 = test_plan_sha)
  TRUE
})
check("default record uses the typed gate artifact path", identical(
  d_ap$gate_record_path, artifact_path("dynamics_gate")
))
d_path <- d_ap
d_path$gate_record_path <- file.path(out_dir, artifact_basename("dynamics_gate"))
check("flat gate path invalidates", expect_stop(
  logvar_egarch_decision_validate(d_path, g_rj, plan_sha256 = test_plan_sha),
  "gate_record_path_mismatch"
))
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
check("approvals on a non-reject gate invalidate", expect_stop(
  logvar_egarch_decision_validate(
    mk_dec(g_nr, "approved", "approved", "user_response"), g_nr,
    plan_sha256 = test_plan_sha
  ), "inconsistent_decision_ladder"
))
check("approved decisions with not_asked_default provenance invalidate", expect_stop(
  logvar_egarch_decision_validate(
    mk_dec(g_rj, "approved", "approved", "not_asked_default"), g_rj,
    plan_sha256 = test_plan_sha
  ), "inconsistent_provenance"
))
check("noncanonical prompt fails the builder", expect_stop(
  logvar_egarch_decision_default(
    g_rj, "2026-07-15T00:00:00Z", "not the real prompt", dep_prompt
  ), "noncanonical_prompt_text"
))

check("no rugarch namespace touched during validate/route", isFALSE(.rugarch_touched))

# Static guard: production files never access the package namespace ----------
prod_agnostic <- vapply(
  c("decision_core.R", "route_core.R", "cleanup.R", "run_route.R"),
  function(file) paper_path("log_variance", "extensions", "egarch", file),
  character(1)
)
has_token <- function(f) any(grepl("rugarch", readLines(f), fixed = TRUE))
check("core/route/cleanup/driver hold no rugarch token", !any(vapply(
  prod_agnostic, has_token, logical(1)
)))
forbidden <- "requireNamespace\\(\\s*[\"']rugarch|library\\(\\s*rugarch|rugarch::"
accesses <- function(f) any(grepl(forbidden, readLines(f)))
all_prod <- c(prod_agnostic, paper_path("config", "decisions", "egarch.R"))
check("no production file accesses the package namespace", !any(vapply(
  all_prod, accesses, logical(1)
)))
dec_lines <- readLines(paper_path("config", "decisions", "egarch.R"))
check("committed record names the package only as content", any(grepl(
  "rugarch", dec_lines,
  fixed = TRUE
)))

# Integration: the committed record against the real gate (if present) -------
gate_rds <- artifact_path("dynamics_gate")
if (file.exists(gate_rds)) {
  source(paper_path("config", "decisions", "egarch.R"))
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
