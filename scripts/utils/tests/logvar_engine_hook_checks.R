# Hook-contract checks for the estimator-generic set engine: the versioned
# context passed to analyze_domain phases, legacy-arity adapters, claimed
# versus unclaimed domain failures, explicit lattice traversal, and extra
# starts as attained candidates. Sourced by test_logvar_engine.R after
# logvar_engine_context_checks.R, whose qs / b_tab / bf / gg / bkey /
# run_engine / new_dummy fixtures this file reuses.

# Context: precheck sees the versioned names (null precheck slot); sides sees
# the stored record
cap <- new.env(parent = emptyenv())
ctx_est <- new_dummy()
ctx_est$est$analyze_domain <- list(
  precheck = function(qs, b_tab, ctx) {
    cap$names <- names(ctx)
    cap$pre_null <- is.null(ctx$precheck)
    list(unresolved = integer(0), n_flagged = 0L, info = "rec")
  },
  sides = function(qs, b_tab, scan, ctx) {
    cap$sides_pre <- ctx$precheck
    cap$feas_fields <- names(ctx$check_feasible(c(0, 0)))
    before <- ctx$budget_state$counters[["probe"]]
    ctx$evaluate_fit(c(0.11, 0.09), phase = "probe")
    ctx$evaluate_fit(c(0.11, 0.09), phase = "probe")
    cap$probe_delta <- ctx$budget_state$counters[["probe"]] - before
    list(
      lower_unbounded = rep(FALSE, 2L), upper_unbounded = rep(FALSE, 2L),
      unresolved_endpoints = character(0), closure_diagnostics = list(),
      info = NULL
    )
  }
)
invisible(run_engine(ctx_est$est))
ctx_names <- c(
  "schema_version", "evaluate_fit", "check_feasible", "b_scales",
  "cache", "budget_state", "precheck"
)
check(
  "precheck sees the versioned context names with a null precheck slot",
  setequal(cap$names, ctx_names) && isTRUE(cap$pre_null)
)
check("sides receives the stored precheck record", identical(cap$sides_pre$info, "rec"))
check(
  "check_feasible returns feasible, max_violation, and hin",
  setequal(cap$feas_fields, c("feasible", "max_violation", "hin"))
)
check("evaluate_fit debits its phase only on a cache miss", cap$probe_delta == 1L)
# A legacy two-argument precheck hook is adapted rather than rejected
ca <- new_dummy()
ca$est$analyze_domain <- list(
  precheck = function(qs, b_tab) list(unresolved = integer(0), n_flagged = 0L, info = NULL)
)
check(
  "a legacy two-argument precheck hook is adapted, not rejected",
  tryCatch(is.data.frame(run_engine(ca$est)$table), error = function(e) FALSE)
)
# A claimed domain failure routes to sides and stays reliable; an unclaimed one is tau-fatal
fail_pt <- unname(bf[1, ])
r_claim <- run_engine(new_dummy(fail_at = fail_pt, claim = TRUE)$est)
check(
  "a claimed domain failure does not make the tau unreliable",
  !any(c(r_claim$schema$lower_status, r_claim$schema$upper_status) == "unreliable")
)
r_unc <- run_engine(new_dummy(fail_at = fail_pt, claim = FALSE)$est)
check(
  "an unclaimed inner-fit failure makes the tau unreliable",
  any(c(r_unc$schema$lower_status, r_unc$schema$upper_status) == "unreliable")
)
# Explicit lattice traversal needs no grid cap and scans in feasible-grid row order
lat <- new_dummy(traversal = "lattice")
r_lat <- run_engine(lat$est)
row_order_ok <- all(vapply(
  seq_len(gg),
  function(i) isTRUE(all.equal(lat$cc$order[[i]], unname(bf[i, ]))), logical(1)
))
check(
  "lattice traversal runs without a grid cap and scans in row order",
  is.data.frame(r_lat$table) && row_order_ok
)
# Extra starts as attained candidates: a more-extreme feasible start beats a failed
# polish; an infeasible start is skipped; duplicates are evaluated once
es <- new_dummy(objective_fail = TRUE)
sc <- run_engine(es$est, extra_starts = list(c(0.7, 0.7)))$schema
row0 <- which(sc$coef == "t0")
check(
  "an extra start moves the endpoint past the grid with extra-start provenance",
  abs(sc$upper[row0] - 1.4) < 1e-8 && grepl("extra-start", sc$upper_provenance[row0])
)
ed <- new_dummy()
r_dup <- run_engine(ed$est, extra_starts = list(c(0.5, 0.5), c(0.5, 0.5), c(5, 5)))
key_half <- bkey(c(0.5, 0.5))
n_half <- sum(vapply(ed$cc$order, function(b) bkey(b) == key_half, logical(1)))
check(
  "an infeasible extra start is skipped and the run still returns a table",
  is.data.frame(r_dup$table)
)
check("duplicated extra starts are evaluated once", n_half == 1L)
