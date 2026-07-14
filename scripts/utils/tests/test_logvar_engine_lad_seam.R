# LAD-specific integration checks against the Plan 7 engine seam: the versioned
# ctx handed to the analyze_domain hooks, precheck -> ctx$precheck -> sides
# threading, the exact/guarded claim_failure contract, named unresolved endpoints
# wired to unreliable, closure_diagnostics passthrough, a budget_state shared and
# reference-counted across two calls on one cache, and explicit lattice traversal.
# Quantreg-free: a LAD-shaped dummy supplies the contract returns so the seam is
# exercised on any machine. Run from the root:
#   Rscript scripts/utils/tests/test_logvar_engine_lad_seam.R

source(here::here("scripts/utils/profile_bounds_core.R"))
source(here::here("scripts/utils/profile_bounds.R"))
source(here::here("scripts-paper/log_var_eq_map.R"))
source(here::here("scripts-paper/log_var_eq_engine.R"))

.pass <- 0L
.fail <- 0L
check <- function(label, cond) {
  ok <- isTRUE(tryCatch(cond, error = function(e) FALSE))
  if (ok) .pass <<- .pass + 1L else .fail <<- .fail + 1L
  cat(sprintf("%s  %s\n", if (ok) "PASS" else "FAIL", label))
}

# A LAD-shaped dummy over a K = 2 ball: nonsmooth metadata, lattice traversal, no
# jacobian. fit_at_b domain-fails at the flagged grid point and returns an ok
# length-2 fit otherwise; analyze_domain is injected per check.
qsb <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
btab <- data.frame(
  coef = c("b1", "b2"), set_lower = c(-1, -1), set_upper = c(1, 1), status = "bounded"
)
seam_dummy <- function(analyze_domain = NULL, fail_at = NULL, traversal = "lattice") {
  fit_at_b <- function(b, start = NULL) {
    if (!is.null(fail_at) && isTRUE(all(abs(b - fail_at) < 1e-9))) {
      return(list(
        coef = c(t0 = NA_real_, t1 = NA_real_), fit_status = "domain_failure",
        converged = FALSE, warm_start = NULL,
        diagnostics = list(domain_state = "exact_domain_failure")
      ))
    }
    list(
      coef = c(t0 = sum(b), t1 = b[1] - b[2]), fit_status = "ok", converged = TRUE,
      objective = 0, score_norm = NA_real_, convergence_code = 0L,
      diagnostics = list(), warm_start = NULL
    )
  }
  list(
    metadata = list(
      estimator = "lad", target_functional = "theta_median",
      intercept_normalization = "median", sample_id = "seam", smoothness = "nonsmooth",
      inner_solver = "quantreg rq.fit br", response_scale = "log",
      spec_id = "seam-v1", cold_start_rtol = 1e-9, traversal = traversal
    ),
    coef_labels = c("t0", "t1"), fit_at_b = fit_at_b, analyze_domain = analyze_domain
  )
}
run_seam <- function(est, qs = qsb, grid_n = 7L, ...) {
  logvar_engine_set_at_tau(est, qs, btab,
    grid_n = grid_n, grid_floor = 1L, cold_start_check = FALSE, ...
  )
}
# trivial hook returns shared across checks
pc_ok <- function(info = list()) {
  function(qs, b_tab, ctx) list(unresolved = character(0), n_flagged = 0L, info = info)
}
sd_ok <- function(unresolved = character(0), closure = NULL, info = list()) {
  function(qs, b_tab, scan, ctx) {
    list(
      lower_unbounded = c(t0 = FALSE, t1 = FALSE),
      upper_unbounded = c(t0 = FALSE, t1 = FALSE),
      unresolved_endpoints = unresolved, closure_diagnostics = closure, info = info
    )
  }
}

# The engine hands precheck the versioned ctx (precheck = NULL), assigns the
# return to ctx$precheck, and threads the completed ctx into sides.
check("lad seam threads the versioned ctx from precheck into sides", {
  rec <- new.env(parent = emptyenv())
  ad <- list(
    precheck = function(qs, b_tab, ctx) {
      rec$names <- names(ctx)
      rec$pre0 <- ctx$precheck
      list(unresolved = character(0), n_flagged = 0L, info = list(token = "abc"))
    },
    sides = function(qs, b_tab, scan, ctx) {
      rec$token <- ctx$precheck$info$token
      sd_ok()(qs, b_tab, scan, ctx)
    }
  )
  run_seam(seam_dummy(ad))
  need <- c(
    "schema_version", "evaluate_fit", "check_feasible", "b_scales", "cache",
    "budget_state", "precheck"
  )
  setequal(rec$names, need) && is.null(rec$pre0) && identical(rec$token, "abc")
})

# claim_failure(b, fit, precheck, ctx): a claimed domain failure is not tau-fatal,
# an unclaimed one fails the tau closed.
check("lad seam claim_failure decides tau fatality of a domain failure", {
  mk <- function(claimed) {
    seam_dummy(list(
      precheck = pc_ok(list(cross = 1L)),
      claim_failure = function(b, fit, precheck, ctx) {
        list(
          claimed = claimed, domain_state = fit$diagnostics$domain_state,
          reason = "seam", probe_targets = if (claimed) 1L else NULL
        )
      },
      sides = sd_ok()
    ), fail_at = c(0, 0))
  }
  claimed <- run_seam(mk(TRUE))
  unclaimed <- run_seam(mk(FALSE))
  any(claimed$table$status == "bounded") && all(unclaimed$table$status != "bounded") &&
    identical(unclaimed$diagnostics$counters[["claimed_domain_failure"]], 0L)
})

# Named unresolved_endpoints become unreliable; closure_diagnostics pass through.
check("lad seam wires unresolved endpoints to unreliable and passes closure through", {
  closure <- list(list(coef = "t1", provenance = "one-sided crossing-limit approximation"))
  ad <- list(precheck = pc_ok(), sides = sd_ok("t1:min", closure, list(cross_all = 1L)))
  res <- run_seam(seam_dummy(ad))
  j <- match("t1", res$schema$coef)
  identical(res$schema$lower_status[j], "unreliable") &&
    identical(res$schema$upper_status[j], "bounded") &&
    identical(res$domain_info$closure_diagnostics, closure)
})

# Any unresolved entry from precheck fails the whole tau closed.
check("lad seam fails closed when precheck returns an unresolved witness", {
  ad <- list(precheck = function(qs, b_tab, ctx) {
    list(unresolved = "crossing_3", n_flagged = 1L, info = list())
  })
  all(run_seam(seam_dummy(ad))$table$status == "unreliable")
})

# One budget_state shared across two calls on one cache: the first debits scan,
# the second is all cache hits and debits nothing.
check("lad seam shares one budget_state across two calls; cache hits never debit", {
  est <- seam_dummy(list(precheck = pc_ok(), sides = sd_ok()))
  cache <- new.env(parent = emptyenv())
  bs <- logvar_budget_state(40000L)
  run_seam(est, cache = cache, budget_state = bs)
  scan1 <- bs$counters[["scan"]]
  hits1 <- bs$counters[["cache_hit"]]
  run_seam(est, cache = cache, budget_state = bs)
  is.environment(bs) && scan1 > 0L && identical(bs$counters[["scan"]], scan1) &&
    bs$counters[["cache_hit"]] > hits1
})

# Explicit lattice traversal reaches a > 5000 point grid; the nearest-neighbor
# default refuses it without max_grid_points.
check("lad seam lattice traversal bypasses the nearest-neighbor 5000 cap", {
  qs_big <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -100)
  go <- function(tr) {
    run_seam(seam_dummy(list(precheck = pc_ok(), sides = sd_ok()), traversal = tr),
      qs = qs_big, grid_n = 71L
    )
  }
  !inherits(try(go("lattice"), silent = TRUE), "try-error") &&
    inherits(try(go("nearest_neighbor"), silent = TRUE), "try-error")
})

# A probe-phase cap exhausted inside sides fails closed with budget disclosure.
check("lad seam discloses budget exhaustion signalled from a sides probe", {
  ad <- list(precheck = pc_ok(), sides = function(qs, b_tab, scan, ctx) {
    ctx$evaluate_fit(c(0.11, 0.07), phase = "probe")
    ctx$evaluate_fit(c(0.12, 0.08), phase = "probe")
    sd_ok()(qs, b_tab, scan, ctx)
  })
  bs <- logvar_budget_state(40000L, phase_caps = c(scan = 20000L, probe = 1L))
  res <- run_seam(seam_dummy(ad), budget_state = bs)
  isTRUE(res$diagnostics$budget_exhausted) &&
    !is.null(res$diagnostics$counters) && all(res$table$status == "unreliable")
})

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
