# Seam-refinement checks for the start pools and selector services the PPML
# plan adds to the engine: the greedy separated pool picker, pool-driven
# polish trial counts, and the selector bookkeeping. Sourced by
# test_engine.R, which supplies check() and the engine layers.

# the pool picker ranks by extremeness and rejects clustered duplicates: two
# near-copies of the extreme corner collapse to one pool slot while the
# separated runner-up is kept
seam_pts <- list(c(0, 0), c(0.01, 0), c(0.6, 0.6), c(-0.7, 0.2))
seam_vals <- cbind(c(-2, -1.99, 1.4, 0.3), c(1, 0.99, -0.5, 0.2))
seam_pools <- logvar_engine_scan_pools(seam_vals, seam_pts, 3L, 0.1)
check(
  "scan pools reject clustered duplicates and keep separated candidates",
  length(seam_pools$arg_min_pool[[1]]) == 2L &&
    !any(vapply(
      seam_pools$arg_min_pool[[1]],
      function(b) isTRUE(all.equal(b, c(0.01, 0))), logical(1)
    )) &&
    identical(seam_pools$arg_min_pool[[1]][[1]], c(-0.7, 0.2))
)
check(
  "scan pools exclude the primary arg-extreme from the returned pool",
  !any(vapply(
    seam_pools$arg_min_pool[[1]],
    function(b) isTRUE(all.equal(b, c(0, 0))), logical(1)
  ))
)

# with starts_per_side = 3 the polish trial count reflects the pool; the
# default of one start per side stays at a single trial
seam_est <- function() {
  fit_at_b <- function(b, start = NULL) {
    paper_test_fit_result(
      c(m1 = sum(b), m2 = b[1] - b[2]),
      warm_start = NULL
    )
  }
  list(
    metadata = list(
      estimator = "seam-dummy", target_functional = "theta_seam",
      intercept_normalization = "none", sample_id = "seam-sample",
      smoothness = "smooth", inner_solver = "closed form",
      response_scale = "level", spec_id = "seam-v1", cold_start_rtol = 1e-8
    ),
    fit_at_b = fit_at_b
  )
}
seam_qs <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
seam_btab <- data.frame(
  coef = c("x1", "x2"), set_lower = c(-1, -1), set_upper = c(1, 1),
  status = "bounded", row.names = NULL
)
seam_run <- function(...) {
  logvar_engine_set_at_tau(
    seam_est(), seam_qs, seam_btab,
    grid_n = 7L, grid_floor = 1L, cold_start_check = FALSE, ...
  )
}
seam_trials <- function(res) {
  vapply(res$diagnostics$polish, function(r) r$n_trials, integer(1))
}
check(
  "the default start count uses the primary search control",
  identical(
    seam_trials(seam_run()),
    seam_trials(seam_run(
      starts_per_side =
        LOGVAR_SEARCH_CONTROL$primary_starts_per_side
    ))
  )
)
check(
  "starts_per_side = 3 feeds separated pool starts into the polish",
  any(seam_trials(seam_run(starts_per_side = 3L)) > 1L)
)

# selector bookkeeping lands in the result diagnostics with input and output
# counts
seam_sel <- function(b_feas, max_grid_points) {
  list(
    grid = b_feas[seq_len(ceiling(nrow(b_feas) / 2L)), , drop = FALSE],
    selector_id = "seam-half", traversal = "engine_default"
  )
}
seam_res <- seam_run(grid_selector = seam_sel)
check(
  "the selector id and counts are recorded in the diagnostics",
  identical(seam_res$diagnostics$selector$selector_id, "seam-half") &&
    seam_res$diagnostics$selector$n_selector_output <
      seam_res$diagnostics$selector$n_selector_input
)
