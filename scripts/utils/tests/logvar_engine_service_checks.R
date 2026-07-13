# Engine service checks: estimator-owned domain dispatch, fail-closed fit
# propagation, deterministic traversal, the nesting helper, the sample
# hash, and figure-path stamping. Sourced by test_logvar_engine.R, which
# supplies check() and the map/engine layers.

set.seed(7)

# small deterministic dummy estimator over a K = 2 system; fail_when(b)
# marks the grid points whose inner fit must fail (unclaimed)
svc_dummy_est <- function(fail_when = NULL, spec_id = "svc-dummy-v1") {
  ok_fit <- list(
    coef = NULL, fit_status = "ok", converged = TRUE, objective = 0,
    score_norm = 0, convergence_code = 0L, diagnostics = list(),
    warm_start = NULL
  )
  bad_fit <- list(
    coef = NULL, fit_status = "nonconvergence", converged = FALSE,
    objective = NA_real_, score_norm = NA_real_, convergence_code = 5L,
    diagnostics = list(), warm_start = NULL
  )
  list(
    metadata = list(
      estimator = "svc-dummy", target_functional = "theta_test",
      intercept_normalization = "none", sample_id = "svc-sample",
      smoothness = "smooth", inner_solver = "closed form",
      response_scale = "level", spec_id = spec_id, cold_start_rtol = 1e-8
    ),
    fit_at_b = function(b, start = NULL) {
      if (!is.null(fail_when) && fail_when(b)) {
        return(bad_fit)
      }
      out <- ok_fit
      out$coef <- c(m1 = sum(b), m2 = b[1] - b[2])
      out
    }
  )
}

svc_qs <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
svc_btab <- data.frame(
  coef = c("x1", "x2"), set_lower = c(-1, -1), set_upper = c(1, 1),
  status = "bounded", row.names = NULL
)

# an estimator without analyze_domain sees no census: no functional solves
# run and no side is certified unbounded, even where log-OLS would cross
local({
  n_solves <- 0L
  old <- solve_linear_functional_bound
  assign("solve_linear_functional_bound", function(...) {
    n_solves <<- n_solves + 1L
    old(...)
  }, envir = globalenv())
  on.exit(
    assign("solve_linear_functional_bound", old, envir = globalenv()),
    add = TRUE
  )
  res <- logvar_engine_set_at_tau(
    svc_dummy_est(), svc_qs, svc_btab,
    grid_n = 7L, cold_start_check = FALSE
  )
  check(
    "domain dispatch runs no census without analyze_domain",
    n_solves == 0L && !any(res$table$status == "unbounded")
  )
})

# unclaimed inner-fit failures on feasible points fail the tau closed for
# every coefficient and side, with the failure count disclosed
svc_fail <- logvar_engine_set_at_tau(
  svc_dummy_est(fail_when = function(b) b[1] > 0.5), svc_qs, svc_btab,
  grid_n = 7L, cold_start_check = FALSE
)
check(
  "unresolved fit failures propagate to an unreliable tau",
  all(svc_fail$table$status == "unreliable") &&
    all(is.na(svc_fail$table$set_lower)) &&
    all(svc_fail$schema$fit_failure_count > 0L)
)

# nearest-neighbor traversal is deterministic: value-identical orderings for
# row-permuted inputs, ties broken by lowest index
svc_pts <- rbind(c(0, 0), c(1, 0), c(1, 0), c(2, 0))
svc_ord <- logvar_order_grid_nn(svc_pts, c(0, 0))
svc_perm <- c(3L, 1L, 4L, 2L)
svc_ord_p <- logvar_order_grid_nn(svc_pts[svc_perm, , drop = FALSE], c(0, 0))
check(
  "nearest-neighbor ordering breaks ties by lowest index",
  identical(svc_ord, c(1L, 2L, 3L, 4L))
)
check(
  "nearest-neighbor ordering is invariant to row permutation",
  identical(
    svc_pts[svc_ord, , drop = FALSE],
    svc_pts[svc_perm, , drop = FALSE][svc_ord_p, , drop = FALSE]
  )
)

# a permuted feasible grid leaves a deterministic dummy's extremes unchanged
# (grid injected by a scoped patch of logvar_feasible_grid)
local({
  base_grid <- logvar_feasible_grid(svc_qs, c(-1, -1), c(1, 1), 7L)
  old <- logvar_feasible_grid
  on.exit(assign("logvar_feasible_grid", old, envir = globalenv()), add = TRUE)
  run_with <- function(grid) {
    assign("logvar_feasible_grid", function(...) grid, envir = globalenv())
    logvar_engine_set_at_tau(
      svc_dummy_est(), svc_qs, svc_btab,
      grid_n = 7L, grid_floor = 1L, cold_start_check = FALSE
    )
  }
  res_a <- run_with(base_grid)
  res_b <- run_with(base_grid[rev(seq_len(nrow(base_grid))), , drop = FALSE])
  check(
    "permuted feasible grid leaves scan extremes unchanged",
    identical(res_a$table$set_lower, res_b$table$set_lower) &&
      identical(res_a$table$set_upper, res_b$table$set_upper)
  )
})

# nesting helper: a nested band sequence passes; a lower endpoint that rises
# with tau is flagged with its side and magnitude
svc_nested <- data.frame(
  tau = c(0.1, 0.2, 0.3), coef = "m1",
  lower = c(-1, -1.5, -2), upper = c(1, 1.5, 2),
  lower_status = "bounded", upper_status = "bounded"
)
svc_broken <- svc_nested
svc_broken$lower[3] <- -1.2
svc_viol <- logvar_check_nesting(svc_broken)
check("nesting helper passes a nested sequence", nrow(logvar_check_nesting(svc_nested)) == 0L)
check(
  "nesting helper flags a fabricated violation",
  nrow(svc_viol) == 1L && svc_viol$side == "lower" && svc_viol$tau == 0.3
)

# sample hash: stable under recomputation, sensitive to the data; figure
# paths are stamped by estimator label so two estimators coexist
svc_n <- 40L
svc_w2 <- matrix(rnorm(svc_n * 2L), svc_n, 2L)
svc_w1 <- drop(svc_w2 %*% c(0.4, -0.2)) + rnorm(svc_n, sd = 2)
svc_pcr <- scale(matrix(rnorm(svc_n * 2L), svc_n, 2L,
  dimnames = list(NULL, c("l.pc1", "l.pc2"))
), center = TRUE, scale = FALSE)
svc_qtr <- seq_len(svc_n)
svc_id_a <- logvar_sample_id(svc_qtr, svc_w1, svc_w2, svc_pcr)
svc_w1_mod <- svc_w1
svc_w1_mod[1] <- svc_w1_mod[1] + 1
check(
  "sample id is stable and data-sensitive",
  identical(svc_id_a, logvar_sample_id(svc_qtr, svc_w1, svc_w2, svc_pcr)) &&
    !identical(svc_id_a, logvar_sample_id(svc_qtr, svc_w1_mod, svc_w2, svc_pcr))
)
check(
  "figure path is estimator-stamped",
  !identical(
    logvar_bounds_tau_path("out", list(estimator = "logols")),
    logvar_bounds_tau_path("out", list(estimator = "ppml"))
  ) && grepl(
    "log_var_eq_bounds_tau_logols\\.pdf$",
    logvar_bounds_tau_path("out", list(estimator = "logols"))
  )
)
