# Set-map checks for the PPML estimator over the Plan 7 engine: singleton-set
# reproduction, endpoint feasibility, fresh-evaluation agreement, grid-only
# survival of a broken polish, traversal invariance, cold-start replication,
# and the Task 4 start-pool / grid_selector seam refinements. The shared
# peng_* fixtures defined here are reused by test_logvar_ppml_table.R (sourced
# after this file). Sourced by test_logvar_ppml.R. Stage-dependent checks run
# through peng_try so the absence of the Task 3/4 pieces fails cleanly rather
# than aborting the run.

set.seed(101)
peng_try <- function(expr) tryCatch(isTRUE(expr), error = function(e) FALSE)

# a K = 2 ball b1^2 + b2^2 <= 1, its bounded box, constraint scales, and a
# small positive synthetic PPML sample (w2 has K = 2 columns to match the ball)
peng_qs <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
peng_btab <- data.frame(
  coef = c("x1", "x2"), set_lower = c(-1, -1), set_upper = c(1, 1), status = "bounded"
)
peng_labels <- c("t0", "t1")
peng_omega <- .derive_constraint_scales(peng_qs, .derive_theta_scale(peng_qs))
peng_n <- 60L
peng_w2 <- matrix(rnorm(peng_n * 2L), peng_n, 2L)
peng_pcr <- scale(matrix(rnorm(peng_n * 4L), peng_n, 4L,
  dimnames = list(NULL, c("l.pc1", "l.pc2", "l.pc3", "l.pc4"))
), center = TRUE, scale = FALSE)
peng_w1 <- drop(peng_w2 %*% c(0.3, -0.2)) + rnorm(peng_n, sd = 1.5)
peng_qtr <- seq_len(peng_n)

# full-precision row keys for set comparisons (insensitive to options(digits))
peng_bkey <- function(m) {
  apply(m, 1L, function(b) {
    paste(formatC(unname(b), digits = 17L, format = "fg", flag = "#"), collapse = "|")
  })
}

# a counting dummy estimator over the ball: a deterministic smooth map with a
# closure counter recording evaluation order; opts toggle a cold/warm
# disagreement and a polish objective that errors
peng_dummy <- function(cold_disagree = FALSE, objective_fail = FALSE) {
  cc <- new.env(parent = emptyenv())
  cc$n <- 0L
  cc$order <- list()
  fit_at_b <- function(b, start = NULL) {
    cc$n <- cc$n + 1L
    cc$order[[length(cc$order) + 1L]] <- unname(b)
    val <- c(sum(b), b[1] - b[2]) + if (cold_disagree && is.null(start)) 10 else 0
    names(val) <- peng_labels
    list(
      coef = val, fit_status = "ok", converged = TRUE, objective = 0,
      score_norm = 0, convergence_code = 0L, diagnostics = list(), warm_start = val
    )
  }
  est <- list(metadata = list(
    estimator = "peng", target_functional = "theta_peng",
    intercept_normalization = "none", sample_id = "peng-sample", smoothness = "smooth",
    inner_solver = "cf", response_scale = "identity", spec_id = "peng-v1",
    cold_start_rtol = 1e-8
  ), fit_at_b = fit_at_b)
  if (objective_fail) {
    est$coef_objective <- function(j) list(fn = function(b) stop("boom"), gr = NULL)
  }
  list(est = est, cc = cc)
}
peng_run <- function(est, ...) {
  logvar_engine_set_at_tau(est, peng_qs, peng_btab,
    b_seed = NULL, grid_n = 7L, grid_floor = 1L, cold_start_check = FALSE, ...
  )
}

# a grid_selector factory: mutate the feasible grid, tag it with an id and a
# traversal ("engine_default" or "as_selected"); reused by the table file
peng_sel <- function(id, tr, mut) {
  function(b_feas, max_grid_points) {
    list(grid = mut(b_feas), selector_id = id, traversal = tr)
  }
}

# Singleton B_tau reproduction, endpoint feasibility, and fresh-evaluation
# agreement all ride on the real PPML estimator (red target)
peng_pv <- tryCatch(
  {
    b0 <- c(0.15, -0.1)
    qs_s <- list(A_i = list(diag(2)), b_i = list(-2 * b0), c_i = sum(b0^2))
    btab_s <- data.frame(
      coef = c("x1", "x2"), set_lower = b0, set_upper = b0, status = "bounded"
    )
    fit0 <- logvar_ppml_fit(b0, peng_w1, peng_w2, cbind(1, peng_pcr))
    mk <- function(anchor) {
      logvar_ppml_estimator(peng_w1, peng_w2, peng_pcr, peng_qtr,
        b_point = NULL, scale_anchor_b = anchor, scale_anchor_source = "test"
      )
    }
    res_s <- logvar_engine_set_at_tau(mk(b0), qs_s, btab_s,
      b_seed = NULL, grid_n = 5L, grid_floor = 1L, cold_start_check = FALSE
    )
    singleton <-
      isTRUE(all.equal(res_s$table$set_lower, unname(fit0$coef), tolerance = 1e-6)) &&
        isTRUE(all.equal(res_s$table$set_upper, unname(fit0$coef), tolerance = 1e-6))
    est <- mk(c(0, 0))
    sc <- logvar_engine_set_at_tau(est, peng_qs, peng_btab,
      b_seed = NULL, grid_n = 5L, grid_floor = 1L, cold_start_check = FALSE,
      max_grid_points = 25L
    )$schema
    feas <- fresh <- TRUE
    chk <- function(j, arg, val) {
      feas <<- feas && .feasibility_residual(peng_qs, arg, peng_omega) <= 1e-4
      fresh <<- fresh &&
        isTRUE(all.equal(unname(est$fit_at_b(arg)$coef[j]), val, tolerance = 1e-6))
    }
    for (j in seq_len(nrow(sc))) {
      if (identical(sc$lower_status[j], "bounded")) chk(j, sc$arg_lower[[j]], sc$lower[j])
      if (identical(sc$upper_status[j], "bounded")) chk(j, sc$arg_upper[[j]], sc$upper[j])
    }
    list(singleton = singleton, feas = feas, fresh = fresh)
  },
  error = function(e) list(singleton = FALSE, feas = FALSE, fresh = FALSE)
)
check("peng singleton B_tau reproduces the direct PPML fit", isTRUE(peng_pv$singleton))
check("peng endpoints satisfy the mean-set feasibility residual", isTRUE(peng_pv$feas))
check("peng fresh evaluation reproduces each endpoint value", isTRUE(peng_pv$fresh))

# Grid-only endpoints survive a broken polish: the erroring objective leaves
# both sides at the grid scan extremes with "grid" provenance
check("peng grid-only endpoints survive a broken polish objective", peng_try({
  res <- peng_run(peng_dummy(objective_fail = TRUE)$est)
  bf <- logvar_feasible_grid(peng_qs, peng_btab$set_lower, peng_btab$set_upper, 7L)
  mp <- t(apply(bf, 1L, function(b) c(sum(b), b[1] - b[2])))
  all(res$schema$lower_provenance == "grid") && all(res$schema$upper_provenance == "grid") &&
    isTRUE(all.equal(res$table$set_lower, apply(mp, 2L, min))) &&
    isTRUE(all.equal(res$table$set_upper, apply(mp, 2L, max)))
}))

# Permuting the feasible grid leaves the converged extremes unchanged
local({
  old <- logvar_feasible_grid
  on.exit(assign("logvar_feasible_grid", old, envir = globalenv()), add = TRUE)
  ok <- peng_try({
    base <- old(peng_qs, c(-1, -1), c(1, 1), 7L)
    run_with <- function(grid) {
      assign("logvar_feasible_grid", function(...) grid, envir = globalenv())
      peng_run(peng_dummy()$est)
    }
    ra <- run_with(base)
    rb <- run_with(base[rev(seq_len(nrow(base))), , drop = FALSE])
    identical(ra$table$set_lower, rb$table$set_lower) &&
      identical(ra$table$set_upper, rb$table$set_upper)
  })
  check("peng permuted feasible grid leaves scan extremes unchanged", ok)
})

# Cold-start replication flags a warm-dependent pathological dummy unreliable
check("peng cold-start replication flags a warm-dependent dummy", peng_try({
  sd <- peng_run(peng_dummy(cold_disagree = TRUE)$est, cold_start_check = TRUE)$schema
  any(c(sd$lower_status, sd$upper_status) == "unreliable")
}))

# starts_per_side: the default path equals an explicit 1L (exact reproduction),
# and the generic scan accepts a pool request of 3L
check("peng starts_per_side default reproduces an explicit 1L", peng_try(identical(
  peng_run(peng_dummy()$est)$table, peng_run(peng_dummy()$est, starts_per_side = 1L)$table
)))
check("peng generic scan accepts starts_per_side = 3L", peng_try({
  res <- peng_run(peng_dummy()$est, starts_per_side = 3L)
  is.data.frame(res$table) && nrow(res$table) == 2L
}))

# grid_selector seam: NULL preserves the default output; a valid subset in
# engine_default traversal is accepted with its id recorded; duplicate and
# invented rows are rejected
peng_subset <- peng_sel("sel-subset", "engine_default", function(g) {
  g[seq_len(max(1L, ceiling(nrow(g) / 2L))), , drop = FALSE]
})
peng_dup <- peng_sel("sel-dup", "engine_default", function(g) {
  g[c(1L, seq_len(nrow(g))), , drop = FALSE]
})
peng_invent <- peng_sel("sel-invent", "engine_default", function(g) rbind(g, c(5, 5)))
check("peng grid_selector = NULL preserves the default table", peng_try(identical(
  peng_run(peng_dummy()$est)$table, peng_run(peng_dummy()$est, grid_selector = NULL)$table
)))
check("peng grid_selector accepts a valid subset and records its id", peng_try({
  res <- peng_run(peng_dummy()$est, grid_selector = peng_subset)
  is.data.frame(res$table) && nrow(res$table) == 2L &&
    "sel-subset" %in% unlist(res$diagnostics)
}))
check("peng grid_selector rejects duplicate or invented rows", peng_try({
  bad <- function(sel) {
    is.null(tryCatch(peng_run(peng_dummy()$est, grid_selector = sel),
      error = function(e) NULL
    ))
  }
  bad(peng_dup) && bad(peng_invent)
}))
