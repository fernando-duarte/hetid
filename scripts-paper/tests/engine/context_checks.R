# Context, cache, budget, and traversal checks for the estimator-generic set
# engine (scripts-paper/log_variance/engine/api.R). Red tests -- the engine does not
# are now required. They pin the shared engine seam
# contract (versioned hook context, exact-b cache with estimator/sample/spec
# stamp, shared per-phase budget state, cold-start cache bypass, claimed vs
# unclaimed domain failures, lattice traversal, extra starts as attained
# candidates) over a K = 2 ball system with counting dummy estimators. Sourced by
# test_engine.R defines check() and sources the engine; this file adds neither.

set.seed(42)

# K = 2 ball b1^2 + b2^2 <= 1, its bounded status table, and the feasible grid
qs <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
b_tab <- data.frame(
  coef = c("x1", "x2"), set_lower = c(-1, -1),
  set_upper = c(1, 1), status = "bounded"
)
coef_labels <- c("t0", "t1")
grid_n <- 7L
bf <- logvar_feasible_grid(qs, c(-1, -1), c(1, 1), grid_n)
gg <- nrow(bf)

# full-precision cache key (engine formatC scheme) and the shared run config
# (grid_floor = 1L stops densification enlarging the tiny grid)
bkey <- function(b) {
  paste(formatC(unname(b), digits = 17, format = "fg", flag = "#"),
    collapse = "|"
  )
}
run_engine <- function(est, cold = FALSE, ...) {
  logvar_engine_set_at_tau(est, qs, b_tab,
    b_seed = NULL, grid_n = grid_n,
    grid_floor = 1L, cold_start_check = cold, ...
  )
}

# a counting dummy estimator: fit_at_b is a deterministic smooth map with a
# closure counter, implementing the estimator contract fields; opts toggle the
# pathologies each check needs (cold/warm disagreement, one domain failure, a
# failing polish objective, all-divergent sides, an explicit traversal, a claim)
new_dummy <- function(spec_id = "dummy-v1", cold_disagree = FALSE,
                      fail_at = NULL, objective_fail = FALSE,
                      diverge = FALSE, traversal = NULL, claim = FALSE) {
  cc <- new.env(parent = emptyenv())
  cc$n <- 0L
  cc$order <- list()
  meta <- list(
    estimator = "dummy", target_functional = "theta_dummy",
    intercept_normalization = "none", sample_id = "sample-1",
    smoothness = "smooth", inner_solver = "closed-form",
    response_scale = "identity", spec_id = spec_id, cold_start_rtol = 1e-8
  )
  if (!is.null(traversal)) meta$traversal <- traversal
  fit_at_b <- function(b, start = NULL) {
    cc$n <- cc$n + 1L
    cc$order[[length(cc$order) + 1L]] <- unname(b)
    if (!is.null(fail_at) && isTRUE(all.equal(unname(b), fail_at))) {
      return(list(
        coef = NULL, fit_status = "domain_failure", converged = FALSE,
        objective = NA_real_, score_norm = NA_real_, convergence_code = 1L,
        diagnostics = list(), warm_start = NULL
      ))
    }
    val <- c(sum(b), diff(range(b)))
    if (cold_disagree && is.null(start)) val <- val + 10
    names(val) <- coef_labels
    list(
      coef = val, fit_status = "ok", converged = TRUE, objective = 0,
      score_norm = 0, convergence_code = 0L, diagnostics = list(),
      warm_start = val
    )
  }
  est <- list(metadata = meta, fit_at_b = fit_at_b)
  if (objective_fail) {
    est$coef_objective <- function(j) list(fn = function(b) NaN, gr = NULL)
  }
  ad <- list()
  if (diverge || !is.null(fail_at)) {
    flag <- rep(diverge, 2L)
    ad$sides <- function(qs, b_tab, scan, ctx) {
      list(
        lower_unbounded = flag, upper_unbounded = flag,
        unresolved_endpoints = character(0), closure_diagnostics = list(),
        info = NULL
      )
    }
  }
  if (claim && !is.null(fail_at)) {
    ad$claim_failure <- function(b, fit, precheck, ctx) {
      list(
        claimed = TRUE, domain_state = "certified", reason = "test",
        probe_targets = NULL
      )
    }
  }
  if (length(ad)) est$analyze_domain <- ad
  list(est = est, cc = cc)
}

# Cache: cold off, so each unique b is fit once; shared cache reused; stamp set
d1 <- new_dummy(spec_id = "dummy-v1")
cache1 <- new.env(parent = emptyenv())
invisible(run_engine(d1$est, cache = cache1))
n_unique <- length(unique(vapply(d1$cc$order, bkey, character(1))))
check("cache fits each unique evaluated point once", d1$cc$n == n_unique)
check(
  "cache carries the estimator, sample, and spec stamp",
  identical(cache1$estimator, "dummy") && identical(cache1$sample_id, "sample-1") &&
    identical(cache1$spec_id, "dummy-v1")
)
n_after1 <- d1$cc$n
invisible(run_engine(d1$est, cache = cache1))
check("re-running with the same cache adds no evaluations", d1$cc$n - n_after1 == 0L)
check("sharing a cache across specifications errors", tryCatch(
  {
    run_engine(new_dummy(spec_id = "dummy-v2")$est, cache = cache1)
    FALSE
  },
  error = function(e) TRUE
))
# Cold-start replication bypasses the cache (extra fits); warm/cold disagreement -> unreliable
dc <- new_dummy()
invisible(run_engine(dc$est, cold = TRUE))
uc <- length(unique(vapply(dc$cc$order, bkey, character(1))))
check("cold-start replication bypasses the cache with extra fits", dc$cc$n > uc)
sd <- run_engine(new_dummy(cold_disagree = TRUE)$est, cold = TRUE)$schema
check(
  "warm/cold disagreement flags the affected side unreliable",
  any(c(sd$lower_status, sd$upper_status) == "unreliable")
)
# A fit budget below the grid aborts the tau closed for all sides with disclosed counts
r_be <- run_engine(new_dummy()$est, max_fit_evals = 3L)
dg <- r_be$diagnostics
sb <- r_be$schema
check(
  "a starved fit budget fails the tau closed with full disclosure",
  all(sb$lower_status == "unreliable") && all(sb$upper_status == "unreliable") &&
    isTRUE(dg$budget_exhausted) && !is.null(dg$n_attempted) &&
    !is.null(dg$n_evaluated) && !is.null(dg$n_failed)
)
# Shared budget state carries debits across calls: second call exhausts though its grid alone fits
bs <- logvar_budget_state(max_fit_evals = gg + 2L)
r_sh1 <- run_engine(new_dummy(diverge = TRUE)$est, budget_state = bs)
r_sh2 <- run_engine(new_dummy(diverge = TRUE)$est, budget_state = bs)
check(
  "a shared budget state carries debits across engine calls",
  !any(r_sh1$schema$lower_status == "unreliable") &&
    all(r_sh2$schema$lower_status == "unreliable")
)
phases <- c(
  "scan", "probe", "refinement", "extra_start", "polish", "nonunique",
  "cold_start", "cache_hit", "claimed_domain_failure"
)
check(
  "the budget state exposes a named counter per phase",
  setequal(names(logvar_budget_state(max_fit_evals = 5L)$counters), phases)
)
# A scan phase cap trips before the global budget and fails the tau closed
bp <- logvar_budget_state(max_fit_evals = 1000L, phase_caps = list(scan = 3L))
r_pc <- run_engine(new_dummy()$est, budget_state = bp)
check(
  "a scan phase cap trips before the global budget and fails closed",
  all(r_pc$schema$lower_status == "unreliable") &&
    isTRUE(r_pc$diagnostics$budget_exhausted)
)
