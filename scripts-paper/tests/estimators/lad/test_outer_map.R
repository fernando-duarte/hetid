# Offline engine-integration checks for the median (LAD) log-variance estimator
# over the shared engine. Pure engine and seam anchors ride on a dummy estimator.
# The logvar_lad_estimator(w1, w2, pcr, qtr) object supplies fit_at_b,
# analyze_domain precheck/sides/claim_failure, no
# jacobian_at_b, traversal = "lattice"); driver result carries
# $table/$schema/$closure_diagnostics. Run from the root.

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))
paper_source_once(paper_path("log_variance", "engine", "api.R"))
paper_source_once(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))
paper_source_once(paper_path("log_variance", "estimators", "ppml", "estimator.R"))

paper_source_once(paper_path("log_variance", "estimators", "lad", "estimator.R"))
paper_source_once(paper_path("log_variance", "estimators", "lad", "fit.R"))
paper_source_once(paper_path("log_variance", "estimators", "lad", "crossing_domain.R"))
# The offline refinement is intentionally loaded only by this dedicated outer-map suite.
paper_source_once(paper_path("log_variance", "estimators", "lad", "offline_refinement.R"))
paper_source_once(paper_path("log_variance", "estimators", "lad", "run_sets.R"))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check
have_qr <- requireNamespace("quantreg", quietly = TRUE)
qr_check <- function(label, cond) {
  if (!have_qr) {
    .test$skip(label, "quantreg unavailable")
    return(invisible())
  }
  check(label, isTRUE(tryCatch(cond, error = function(e) FALSE)))
}

# A counting dummy estimator over a K = 2 ball (LAD-shaped metadata, switchable
# traversal, no warm start); the pure-engine anchors ride on it.
lad_dummy <- function(traversal = "lattice", objective_fail = FALSE) {
  fit_at_b <- function(b, start = NULL) {
    list(
      coef = c(t0 = sum(b), t1 = b[1] - b[2]), fit_status = "ok", converged = TRUE,
      objective = 0, score_norm = NA, convergence_code = 0L,
      diagnostics = list(), warm_start = NULL
    )
  }
  est <- list(metadata = list(
    estimator = "lad", target_functional = "theta_median",
    intercept_normalization = "median", sample_id = "lad-dummy",
    smoothness = "nonsmooth", inner_solver = "quantreg rq.fit br",
    response_scale = "log", spec_id = "lad-dummy-v1", cold_start_rtol = 1e-9,
    traversal = traversal
  ), fit_at_b = fit_at_b)
  if (objective_fail) {
    est$coef_objective <- function(j) list(fn = function(b) stop("boom"), gr = NULL)
  }
  est
}
ox_fx <- local({
  set.seed(919L)
  n <- 60L
  pcr <- scale(matrix(rnorm(n * 4L), n, 4L), center = TRUE, scale = FALSE)
  colnames(pcr) <- paste0("l.pc", 1:4)
  x_mat <- cbind("(Intercept)" = 1, pcr)
  qtr <- seq_len(n)
  w2 <- matrix(rnorm(n * 2L), n, 2L)
  b0 <- c(0.15, -0.1)
  w1 <- drop(w2 %*% b0) + 1.2 * sample(c(-1, 1), n, TRUE) + rnorm(n, sd = 0.3)
  qsb <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
  btab <- data.frame(
    coef = c("b1", "b2"), set_lower = c(-1, -1), set_upper = c(1, 1), status = "bounded"
  )
  qs_s <- list(A_i = list(diag(2)), b_i = list(-2 * b0), c_i = sum(b0^2))
  btab_s <- data.frame(
    coef = c("b1", "b2"), set_lower = b0, set_upper = b0, status = "bounded"
  )
  environment()
})
lad_est <- function(w1 = ox_fx$w1) logvar_lad_estimator(w1, ox_fx$w2, ox_fx$pcr, ox_fx$qtr)
run_dummy <- function(est, grid_n = 7L, ...) {
  logvar_engine_set_at_tau(est, ox_fx$qsb, ox_fx$btab,
    grid_n = grid_n, grid_floor = 1L, cold_start_check = FALSE, ...
  )
}

# Pure anchor: the engine exposes the literal seam signature and LAD phase counters.
check("lad engine seam exposes the literal signature and phase counters", {
  need <- c(
    "est", "qs", "b_tab", "b_seed", "grid_n", "grid_floor", "extra_starts",
    "max_grid_points", "max_fit_evals", "starts_per_side", "grid_selector",
    "cache", "budget_state", "cold_start_check"
  )
  ct <- names(logvar_budget_state(40000L)$counters)
  all(need %in% names(formals(logvar_engine_set_at_tau))) &&
    all(c("probe", "refinement", "nonunique", "polish") %in% ct)
})
# Pure anchor: a 5001-point grid uses the lattice loop (no NN cap); default stops.
check("lad lattice traversal bypasses nearest-neighbor over 5001 points", {
  qs_big <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -100)
  run71 <- function(tr) {
    logvar_engine_set_at_tau(lad_dummy(tr), qs_big, ox_fx$btab,
      grid_n = 71L, grid_floor = 1L, cold_start_check = FALSE
    )
  }
  !inherits(try(run71("lattice"), silent = TRUE), "try-error") &&
    inherits(try(run71("nearest_neighbor"), silent = TRUE), "try-error")
})
# Pure anchor: a broken polish keeps grid provenance; extra starts never shrink.
check("lad grid endpoints survive a broken polish and refinement never loses value", {
  gp <- run_dummy(lad_dummy(objective_fail = TRUE))$schema
  base <- run_dummy(lad_dummy())$table
  ref <- run_dummy(lad_dummy(), extra_starts = list(c(0, 0)))$table
  all(gp$lower_provenance == "grid") && all(gp$upper_provenance == "grid") &&
    all(ref$set_lower <= base$set_lower + 1e-9) &&
    all(ref$set_upper >= base$set_upper - 1e-9)
})
# Pure anchor: an unclaimed inner failure and a scan-phase cap both fail closed.
check("lad unclaimed failure and a budget cap both fail closed with disclosure", {
  bad <- lad_dummy()
  bad$fit_at_b <- function(b, start = NULL) {
    list(coef = c(NA, NA), fit_status = "nonconvergence", converged = FALSE, warm_start = NULL)
  }
  unrel <- !any(run_dummy(bad)$table$status == "bounded")
  bs <- logvar_budget_state(1000L, phase_caps = c(scan = 3L))
  ex <- run_dummy(lad_dummy(), grid_n = 11L, budget_state = bs)
  unrel && isTRUE(ex$diagnostics$budget_exhausted) && !is.null(ex$diagnostics$counters)
})
# Pure anchor: the normalization constants equal their single-sourced expressions.
check("lad normalization constants match their single-sourced expressions", {
  abs(log(qchisq(0.5, 1)) - (-0.787597599)) < 1e-9 &&
    abs((log(qchisq(0.5, 1)) - (digamma(0.5) + log(2))) - 0.482765246) < 1e-9 &&
    abs(qchisq(0.5, 1) - 0.4549364231) < 1e-9
})
# Singleton reproduces the direct fit plus the nonsmooth hooks (no jacobian).
qr_check("lad singleton reproduces the direct fit and exposes the nonsmooth hooks", {
  est <- lad_est()
  ad <- est$analyze_domain
  esc <- logvar_lad_scale_reference(ox_fx$w1 - drop(ox_fx$w2 %*% ox_fx$b0))
  fit0 <- logvar_lad_fit(ox_fx$b0, ox_fx$w1, ox_fx$w2, ox_fx$x_mat, esc)
  res <- logvar_engine_set_at_tau(est, ox_fx$qs_s, ox_fx$btab_s,
    grid_n = 5L, grid_floor = 1L, cold_start_check = FALSE
  )
  is.function(ad$precheck) && is.function(ad$sides) && is.function(ad$claim_failure) &&
    is.null(est$jacobian_at_b) && identical(est$metadata$traversal, "lattice") &&
    isTRUE(all.equal(res$table$set_lower, unname(fit0$coef), tolerance = 5e-4)) &&
    isTRUE(all.equal(res$table$set_upper, unname(fit0$coef), tolerance = 5e-4))
})
# Every certified endpoint is feasible and reproduced by a fresh evaluation.
qr_check("lad endpoints are feasible and fresh-fit reproducible", {
  est <- lad_est()
  omega <- .derive_constraint_scales(ox_fx$qsb, .derive_theta_scale(ox_fx$qsb))
  sc <- logvar_engine_set_at_tau(est, ox_fx$qsb, ox_fx$btab,
    grid_n = 7L, grid_floor = 1L, cold_start_check = FALSE, max_grid_points = 60L
  )$schema
  a <- sc$arg_lower[[1]]
  identical(sc$lower_status[1], "bounded") &&
    .feasibility_residual(ox_fx$qsb, a, omega) <= 1e-4 &&
    isTRUE(all.equal(unname(est$fit_at_b(a)$coef[1]), sc$lower[1], tolerance = 1e-6))
})
# Probe/refinement/nonunique caps fail covered endpoints closed and disclose counts.
qr_check("lad probe/refinement/nonunique budget exhaustion discloses counts", {
  caps <- c(scan = 20000L, probe = 1L, refinement = 1L, nonunique = 1L)
  bs <- logvar_budget_state(40000L, phase_caps = caps)
  res <- logvar_engine_set_at_tau(lad_est(), ox_fx$qsb, ox_fx$btab,
    grid_n = 9L, grid_floor = 1L, cold_start_check = FALSE,
    starts_per_side = 3L, budget_state = bs
  )
  isTRUE(res$diagnostics$budget_exhausted) && !is.null(res$diagnostics$counters)
})
# Large normal simulation: LAD slopes track variance slopes; intercept gaps match.
qr_check("lad normalization simulation tracks variance slopes and intercept gaps", {
  set.seed(42L)
  n <- 20000L
  r <- scale(matrix(rnorm(n * 4L), n, 4L), center = TRUE, scale = FALSE)
  beta <- c(0.3, -0.2, 0.1, 0.05)
  e <- sqrt(exp(-1 + drop(r %*% beta))) * rnorm(n)
  xg <- cbind(1, r)
  lad <- logvar_lad_fit_response(2 * log(abs(e)), xg)
  vint <- unname(logvar_ppml_fit_response(e^2, xg)$coef[1])
  mint <- unname(stats::lm.fit(xg, log(e^2))$coefficients[1])
  max(abs(unname(lad$coef[-1]) - beta)) <= 0.05 &&
    abs((unname(lad$coef[1]) - mint) - 0.482765246) <= 0.05 &&
    abs((unname(lad$coef[1]) - vint) - (-0.787597599)) <= 0.05
})

.test$finish()
