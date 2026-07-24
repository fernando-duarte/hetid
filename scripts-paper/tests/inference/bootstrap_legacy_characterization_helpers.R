# Test-only frozen legacy callback decomposition. Production must never source
# this file: its duplication is the independent oracle for the refactor.

paper_legacy_mean_from_est <- function(est, spec) {
  point <- if (is.null(est$point0)) {
    rep(NA_real_, length(spec$coefs))
  } else {
    c(
      hetid::recover_structural_coefficients(
        est$beta1r,
        est$beta2r,
        est$point0$theta
      ),
      est$point0$theta
    )
  }
  bounds <- lapply(spec$taus, function(tau) {
    interval <- coef_interval_tables(
      spec$gamma,
      tau,
      est$moments,
      est$beta1r,
      est$beta2r
    )
    table <- rbind(interval$beta1, interval$theta)
    bounded <- table$status == PAPER_ENDPOINT_STATUS[["bounded"]]
    list(
      lower = ifelse(bounded, table$set_lower, NA_real_),
      upper = ifelse(bounded, table$set_upper, NA_real_),
      status = table$status
    )
  })
  coarse <- sweep_fixed_gamma(
    spec$gamma,
    est$moments,
    spec$tau_grid,
    "boot"
  )
  tau_star <- tau_star_fixed(
    spec$gamma,
    est$moments,
    coarse,
    iters =
      PAPER_INFERENCE_SEARCH_CONTROL$tau_star$bootstrap_bisection_iterations
  )
  list(
    point = point,
    point_ok = !is.null(est$point0),
    bounds = bounds,
    tau_star = tau_star$tau_star,
    capped = tau_star$capped
  )
}

paper_legacy_logvar_from_est <- function(dat, est, spec) {
  complete <- stats::complete.cases(dat[, spec$pc_cols, drop = FALSE])
  w1 <- est$w1[complete]
  w2 <- est$w2[complete, , drop = FALSE]
  qtr <- dat$qtr[complete]
  pcr <- paper_normalize_model_matrix(
    dat[complete, spec$pc_cols],
    PAPER_ANALYSIS_CONTRACT$model$preprocessing$return_pc
  )
  colnames(pcr) <- spec$pc_cols
  point <- if (is.null(est$point0)) NULL else est$point0$theta
  boxes <- lapply(spec$taus, function(tau) {
    coef_interval_tables(
      spec$gamma,
      tau,
      est$moments,
      est$beta1r,
      est$beta2r
    )$theta
  })
  quadratics <- lapply(spec$taus, function(tau) {
    tau_quadratic_system(spec$gamma, tau, est$moments)
  })
  built <- list()
  for (id in spec$estimator_ids) {
    dependencies <- paper_logvar_estimator_spec(id)$dependencies
    stopifnot(all(dependencies %in% names(built)))
    built[[id]] <- tryCatch(
      spec$builders[[id]](w1, w2, pcr, qtr, point, built),
      error = function(error) NULL
    )
  }
  stats::setNames(
    lapply(spec$estimator_ids, function(id) {
      logvar_run_estimator(
        built[[id]],
        spec,
        boxes,
        quadratics,
        point
      )
    }),
    spec$estimator_ids
  )
}

paper_legacy_logvar_result <- function(
  primary,
  b_reps,
  block,
  seed,
  sens_block,
  sens_reps,
  inference_contract,
  c_sim,
  tau0,
  provenance
) {
  c(
    primary,
    list(
      b_reps = b_reps,
      block = block,
      seed = seed,
      sens_block = sens_block,
      sens_reps = sens_reps,
      inference_contract = inference_contract,
      coverage_target = "whole-set outer envelope",
      c_sim = c_sim,
      tau0 = tau0,
      provenance = provenance
    )
  )
}

paper_characterize_estimate_calls <- function(run) {
  stopifnot(is.function(run))
  original <- get("estimate_set_id_system", envir = .GlobalEnv)
  n_calls <- 0L
  counting <- function(dat, spec) {
    n_calls <<- n_calls + 1L
    original(dat, spec)
  }
  assign("estimate_set_id_system", counting, envir = .GlobalEnv)
  on.exit(
    assign("estimate_set_id_system", original, envir = .GlobalEnv),
    add = TRUE
  )
  value <- run()
  list(value = value, n_calls = n_calls)
}

paper_with_legacy_binding <- function(name, replacement, run) {
  stopifnot(is.character(name), length(name) == 1L, is.function(run))
  original <- get(name, envir = .GlobalEnv, inherits = FALSE)
  assign(name, replacement, envir = .GlobalEnv)
  on.exit(
    assign(name, original, envir = .GlobalEnv),
    add = TRUE
  )
  run()
}
