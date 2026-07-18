# Offline branch and stale-decision checks exercise every EGARCH-X router branch,
# the strict validator, and the ban on optional-package access during routing.
source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check
paper_source_once(paper_path("tests", "diagnostics", "egarch", "fixtures.R"))
paper_source_once(paper_path("config", "decisions", "egarch.R"))
paper_source_once(paper_path("log_variance", "extensions", "egarch", "cleanup.R"))
paper_source_once(paper_path(
  "tests", "diagnostics", "egarch", "committed_gate_fixture.R"
))
paper_source_once(paper_path(
  "tests", "diagnostics", "egarch", "tracked_decision_checks.R"
))
check("gate science hash excludes commit provenance", identical(
  logvar_egarch_gate_science_sha256(mk_gate("reject", commit = "a")),
  logvar_egarch_gate_science_sha256(mk_gate("reject", commit = "b"))
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
paper_source_once(paper_path("tests", "diagnostics", "egarch", "route_protocol_checks.R"))
s_nr <- logvar_egarch_route_status(g_nr, r_nr, d_nr, TRUE)
check("route status carries protocol-owned gate fields", all(
  c("protocol", "gate_q", "gate_p") %in% names(s_nr)
))
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
  logvar_egarch_decision_validate(d_ap, g_rj)
  TRUE
})
check("default record uses the typed gate artifact path", identical(
  d_ap$gate_record_path, artifact_path("dynamics_gate")
))
d_path <- d_ap
d_path$gate_record_path <- file.path(out_dir, artifact_basename("dynamics_gate"))
check("flat gate path invalidates", expect_stop(
  logvar_egarch_decision_validate(d_path, g_rj),
  "gate_record_path_mismatch"
))
check("changed gate hash invalidates", expect_stop(
  logvar_egarch_decision_validate(
    d_ap,
    mk_gate("reject", sample_id = "sample_B", q = 15, p = 0.001)
  ), "gate_record_hash_mismatch"
))
g_protocol <- g_rj
g_protocol$protocol$alpha <- 0.01
check("changed gate protocol invalidates", expect_stop(
  logvar_egarch_decision_validate(d_ap, g_protocol),
  "gate_record_hash_mismatch"
))
d_tamper <- d_ap
d_tamper$sample_id <- "tampered"
check("tampered sample_id invalidates", expect_stop(
  logvar_egarch_decision_validate(d_tamper, g_rj),
  "gate_field_mismatch"
))
d_stale_plan <- d_ap
d_stale_plan$plan_sha256 <- "stale_plan_hash"
check("stale plan sha invalidates", expect_stop(
  logvar_egarch_decision_validate(d_stale_plan, g_rj),
  "stale_plan_sha256"
))
d_stale_up <- d_ap
d_stale_up$upstream_plans_hash <- "stale_upstream_hash"
check("stale upstream hash invalidates", expect_stop(
  logvar_egarch_decision_validate(d_stale_up, g_rj),
  "stale_upstream_plans_hash"
))
d_prompt <- d_ap
d_prompt$estimand_prompt <- paste(d_ap$estimand_prompt, "extra")
check("edited estimand prompt invalidates", expect_stop(
  logvar_egarch_decision_validate(d_prompt, g_rj),
  "estimand_prompt_hash_mismatch"
))
d_ladder <- d_ap
d_ladder$decisions <- c(estimand = "declined", dependency = "approved")
check("broken ladder invalidates", expect_stop(
  logvar_egarch_decision_validate(d_ladder, g_rj),
  "inconsistent_decision_ladder"
))
check("approvals on a non-reject gate invalidate", expect_stop(
  logvar_egarch_decision_validate(
    mk_dec(g_nr, "approved", "approved", "user_response"),
    g_nr
  ), "inconsistent_decision_ladder"
))
check("approved decisions with not_asked_default provenance invalidate", expect_stop(
  logvar_egarch_decision_validate(
    mk_dec(g_rj, "approved", "approved", "not_asked_default"),
    g_rj
  ), "inconsistent_provenance"
))
check("noncanonical prompt fails the builder", expect_stop(
  logvar_egarch_decision_default(
    g_rj,
    "2026-07-15T00:00:00Z",
    "not the real prompt",
    logvar_egarch_dependency_prompt
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
if (file.exists(gate_rds) &&
  !is.null(readRDS(gate_rds)$protocol)) {
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
  .test$skip("committed decision integration", "current-protocol gate record absent")
}
.test$finish()
