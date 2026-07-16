# Set-map and integration checks for the Harvey variance estimator over the
# shared engine (scripts-paper/log_variance/engine/api.R with the Harvey estimator):
# a singleton mean set reproduces the direct fit, certified endpoints are
# feasible and fresh-fit reproducible, a broken polish leaves the grid extremes,
# the analyze_domain hooks dispatch to the exact fit-level recession schema, and
# the residual likelihood is invariant to a joint row permutation of the inputs.
# Estimator-dependent assertions run through he_try and fail closed rather than
# aborting on a missing required module. Sourced by test_harvey.R after the fixtures.

set.seed(414)
he_try <- function(expr) tryCatch(isTRUE(expr), error = function(e) FALSE)

# a K = 2 ball b1^2 + b2^2 <= 1, its bounded box, constraint scales, and a small
# positive synthetic system (w2 has two columns to match the ball; pcr carries
# the four lagged return PCs the constructor expects)
he_qs <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
he_btab <- data.frame(
  coef = c("x1", "x2"), set_lower = c(-1, -1), set_upper = c(1, 1), status = "bounded"
)
he_labels <- c("t0", "t1")
he_omega <- .derive_constraint_scales(he_qs, .derive_theta_scale(he_qs))
he_n <- 60L
he_w2 <- matrix(rnorm(he_n * 2L), he_n, 2L)
he_pcr <- scale(matrix(rnorm(he_n * 4L), he_n, 4L,
  dimnames = list(NULL, c("l.pc1", "l.pc2", "l.pc3", "l.pc4"))
), center = TRUE, scale = FALSE)
he_w1 <- drop(he_w2 %*% c(0.3, -0.2)) + rnorm(he_n, sd = 1.2)
he_qtr <- seq_len(he_n)
he_x <- cbind(1, he_pcr)
colnames(he_x) <- c("(Intercept)", colnames(he_pcr))
he_mk <- function(bp) {
  logvar_harvey_estimator(he_w1, he_w2, he_pcr, he_qtr, b_point = bp)
}
he_run <- function(est, cold = FALSE, ...) {
  logvar_engine_set_at_tau(est, he_qs, he_btab,
    b_seed = NULL, grid_n = 7L, grid_floor = 1L, cold_start_check = cold, ...
  )
}

# a counting dummy estimator over the ball with a polish objective that errors,
# mirroring the PPML engine block: the Harvey-specific assertion is provenance
he_dummy <- function(objective_fail = FALSE) {
  cc <- new.env(parent = emptyenv())
  cc$n <- 0L
  fit_at_b <- function(b, start = NULL) {
    cc$n <- cc$n + 1L
    val <- c(sum(b), b[1] - b[2])
    names(val) <- he_labels
    list(
      coef = val, fit_status = "ok", converged = TRUE, objective = 0,
      score_norm = 0, convergence_code = 0L, diagnostics = list(), warm_start = val
    )
  }
  est <- list(metadata = list(
    estimator = "harvey", target_functional = "theta_var_gaussian",
    intercept_normalization = "none", sample_id = "he-sample", smoothness = "smooth",
    inner_solver = "scoring", response_scale = "variance", spec_id = "he-v1",
    cold_start_rtol = 1e-8
  ), fit_at_b = fit_at_b)
  if (objective_fail) {
    est$coef_objective <- function(j) list(fn = function(b) stop("boom"), gr = NULL)
  }
  list(est = est, cc = cc)
}

# Singleton B_tau reproduction, endpoint feasibility, and fresh evaluation ride
# on the real Harvey estimator (the red target)
he_pv <- tryCatch(
  {
    b0 <- c(0.15, -0.1)
    qs_s <- list(A_i = list(diag(2)), b_i = list(-2 * b0), c_i = sum(b0^2))
    btab_s <- data.frame(
      coef = c("x1", "x2"), set_lower = b0, set_upper = b0, status = "bounded"
    )
    fit0 <- logvar_harvey_fit(b0, he_w1, he_w2, he_x)
    res_s <- logvar_engine_set_at_tau(he_mk(NULL), qs_s, btab_s,
      b_seed = NULL, grid_n = 5L, grid_floor = 1L, cold_start_check = FALSE
    )
    singleton <-
      isTRUE(all.equal(res_s$table$set_lower, unname(fit0$coef), tolerance = 5e-4)) &&
        isTRUE(all.equal(res_s$table$set_upper, unname(fit0$coef), tolerance = 5e-4))
    est <- he_mk(NULL)
    sc <- logvar_engine_set_at_tau(est, he_qs, he_btab,
      b_seed = NULL, grid_n = 5L, grid_floor = 1L, cold_start_check = FALSE,
      max_grid_points = 25L
    )$schema
    feas <- fresh <- TRUE
    chk <- function(j, arg, val) {
      feas <<- feas && .feasibility_residual(he_qs, arg, he_omega) <= 1e-4
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
check("he singleton B_tau reproduces the direct Harvey fit", isTRUE(he_pv$singleton))
check("he endpoints satisfy the mean-set feasibility residual", isTRUE(he_pv$feas))
check("he fresh evaluation reproduces each endpoint value", isTRUE(he_pv$fresh))

# Grid-only endpoints survive a broken polish objective at the grid extremes
check("he grid-only endpoints survive a broken polish objective", he_try({
  res <- he_run(he_dummy(objective_fail = TRUE)$est)
  bf <- logvar_feasible_grid(he_qs, he_btab$set_lower, he_btab$set_upper, 7L)
  mp <- t(apply(bf, 1L, function(b) c(sum(b), b[1] - b[2])))
  all(res$schema$lower_provenance == "grid") &&
    all(res$schema$upper_provenance == "grid") &&
    isTRUE(all.equal(res$table$set_lower, apply(mp, 2L, min))) &&
    isTRUE(all.equal(res$table$set_upper, apply(mp, 2L, max)))
}))

# analyze_domain dispatch: the precheck returns the engine's vector contract
# (an empty unresolved vector on a passing self-test, matching the log-OLS
# reference and the engine's length-based gate) and the sides hook returns
# named all-false fit-level recession vectors
check("he analyze_domain precheck returns the exact self-test schema", he_try({
  ad <- he_mk(NULL)$analyze_domain
  pre <- logvar_call_precheck(ad$precheck, he_qs, he_btab, NULL)
  length(pre$unresolved) == 0L && pre$n_flagged == 0 && !is.null(pre$info)
}))

check("he analyze_domain sides returns named all-false recession flags", he_try({
  ad <- he_mk(NULL)$analyze_domain
  scan <- list(
    min = setNames(c(-1, -1), c("x1", "x2")),
    max = setNames(c(1, 1), c("x1", "x2"))
  )
  sd <- logvar_call_sides(ad$sides, he_qs, he_btab, scan, NULL)
  lo <- sd$lower_unbounded
  up <- sd$upper_unbounded
  is.logical(lo) && !any(lo) && identical(names(lo), c("x1", "x2")) &&
    is.logical(up) && !any(up) && identical(names(up), c("x1", "x2")) &&
    identical(sd$info$method, "fit_level_recession")
}))

# Heavy-tailed multiplicative-variance responses at the estimation sample's
# scale and dynamic range (a t_3 innovation gives the naive OLS-residual
# response's ~7-order spread) must converge from a cold intercept-only start,
# and quickly. Two things this pins: (a) near the optimum the scoring step's
# true criterion decrease sits below Q's summation-noise floor, so acceptance
# must treat a noise-band criterion tie as equality (with the mandatory strict
# score progress), not stall the line search on a last-bit rounding; and (b)
# the observed-Newton direction is quadratically convergent, so every case must
# converge in well under a hundred iterations -- pure expected-information
# scoring is only linearly convergent and crawls past a thousand iterations
# here, so a regression that drops the Newton acceleration fails this check in
# a second rather than the pipeline in an hour.
check("he heavy-tailed responses converge fast from a cold start", he_try({
  ok <- TRUE
  max_iters <- 0L
  for (s in 1:20) {
    set.seed(s)
    pcr_s <- scale(matrix(rnorm(255L * 4L), 255L, 4L,
      dimnames = list(NULL, paste0("l.pc", 1:4))
    ), center = TRUE, scale = FALSE)
    x_s <- cbind(1, pcr_s)
    colnames(x_s) <- c("(Intercept)", colnames(pcr_s))
    y_s <- exp(-2 + drop(pcr_s %*% c(0.2, -0.3, 0.25, 0.6))) *
      stats::rt(255L, df = 3)^2
    f_s <- logvar_harvey_fit_response(y_s, x_s)
    ok <- ok && isTRUE(f_s$converged) && f_s$score_norm <= 1e-8
    max_iters <- max(max_iters, abs(f_s$convergence_code))
  }
  ok && max_iters < 100L
}))

# A joint row permutation of (w1, w2, pcr, qtr) leaves the point fit unchanged
check("he a joint row permutation leaves the point fit coefficients fixed", he_try({
  b0 <- c(0.15, -0.1)
  perm <- rev(seq_len(he_n))
  ea <- logvar_harvey_estimator(he_w1, he_w2, he_pcr, he_qtr, b_point = b0)
  eb <- logvar_harvey_estimator(he_w1[perm], he_w2[perm, , drop = FALSE],
    he_pcr[perm, , drop = FALSE], he_qtr[perm],
    b_point = b0
  )
  isTRUE(all.equal(unname(ea$point_fit$coef), unname(eb$point_fit$coef),
    tolerance = 1e-8
  ))
}))
