# Core machinery for the set-identification endpoint bootstrap: the one-draw
# re-estimation, the draw evaluation, and the diagnostics table, all
# parameterized by a spec list so the paper driver stays thin and the pieces
# are testable on synthetic systems. Statuses are decoupled: a rank-deficient
# tau = 0 system yields an NA point without discarding the draw's endpoint
# evaluations, every display tau is solved regardless of the draw's tau*, and
# each coefficient-tau side carries the shared solvers' own three-state
# status. Consumed and tested by the paper pipeline's mean-equation inference
# modules.

# Re-estimate the mean-equation system on one data frame: the W1/W2
# residualizations, the de-meaned instrument, the identification moments, and
# the closed-form tau = 0 point. Shared by the full-sample estimation
# (scripts-paper/mean_equation/estimate_identified_set.R) and the per-draw
# bootstrap so the two recipes cannot drift apart.
estimate_set_id_system <- function(dat, spec) {
  fit1 <- stats::lm(
    stats::reformulate(spec$x_cols, response = spec$y1_col),
    data = dat
  )
  beta1r <- stats::coef(fit1)
  w1 <- stats::residuals(fit1)
  # under the orthogonality null the news PCs are population-orthogonal to
  # X_t, so beta2R = 0 exactly and W2 is the raw news; otherwise W2 is the
  # sample residualization on X_t
  if (spec$impose_null) {
    w2 <- as.matrix(dat[spec$y2_cols])
    beta2r <- matrix(
      0, length(spec$y2_cols), length(beta1r),
      dimnames = list(spec$y2_cols, names(beta1r))
    )
  } else {
    fit2 <- stats::lm(
      as.matrix(dat[spec$y2_cols]) ~ .,
      data = dat[spec$x_cols]
    )
    w2 <- stats::residuals(fit2)
    beta2r <- t(stats::coef(fit2))
  }
  z <- dat[[spec$z_col]] - mean(dat[[spec$z_col]])
  moments <- hetid::compute_identification_moments(
    w1, w2, matrix(z, ncol = 1, dimnames = list(NULL, spec$z_col))
  )
  built <- build_pipeline_quadratic_system(
    spec$gamma, rep(0, ncol(spec$gamma)), moments
  )
  list(
    beta1r = beta1r, w1 = w1, beta2r = beta2r, w2 = w2, z = z,
    moments = moments,
    point0 = solve_point_identification(built$components),
    tau0_quadratic = built$quadratic
  )
}

# Evaluate the mean branch from one shared system estimate and display geometry.
set_id_boot_draw_from_est <- function(est, shared_geometry, mean_spec) {
  point <- if (is.null(est$point0)) {
    rep(NA_real_, length(mean_spec$coefs))
  } else {
    c(
      hetid::recover_structural_coefficients(
        est$beta1r, est$beta2r, est$point0$theta
      ),
      est$point0$theta
    )
  }
  bounds <- lapply(shared_geometry$display_slots, function(slot) {
    interval <- shared_geometry$tables[[slot]]
    table <- rbind(interval$beta1, interval$theta)
    bounded <- table$status == PAPER_ENDPOINT_STATUS[["bounded"]]
    list(
      lower = ifelse(bounded, table$set_lower, NA_real_),
      upper = ifelse(bounded, table$set_upper, NA_real_),
      status = table$status
    )
  })
  coarse <- sweep_fixed_gamma(
    shared_geometry$gamma,
    est$moments,
    mean_spec$tau_star_grid,
    "boot"
  )
  tau_star <- tau_star_fixed(
    shared_geometry$gamma,
    est$moments,
    coarse,
    iters = mean_spec$tau_star_iterations
  )
  list(
    point = point, point_ok = !is.null(est$point0),
    bounds = bounds,
    tau_star = tau_star$tau_star,
    capped = tau_star$capped
  )
}

# Compatibility wrapper: estimate once, build shared geometry, then delegate.
set_id_boot_draw <- function(dat, spec) {
  est <- estimate_set_id_system(dat, spec)
  iterations <- if ("tau_star_iterations" %in% names(spec)) {
    spec$tau_star_iterations
  } else {
    PAPER_INFERENCE_SEARCH_CONTROL$tau_star$bootstrap_bisection_iterations
  }
  mean_spec <- list(
    coefs = spec$coefs,
    tau_star_grid = spec$tau_grid,
    tau_star_iterations = iterations
  )
  geometry <- set_id_boot_geometry(
    est,
    spec$gamma,
    c(0, spec$taus),
    spec$taus
  )
  set_id_boot_draw_from_est(est, geometry, mean_spec)
}

# One diagnostics row per coefficient-tau pair: the full-sample set, per-
# status draw counts, robust scales and correlation, per-endpoint t
# statistics (endpoint over its robust scale; the upper-endpoint t backs a
# negative set's excludes-zero claim, the lower-endpoint t a positive set's),
# the calibrated interval, and the reason a table cell renders blank.
set_id_boot_diagnostics <- function(collected, inference, set_tables, taus,
                                    min_reps = boot_min_reps(
                                      nrow(collected$endpoint_draws[[1]]$status)
                                    )) {
  do.call(rbind, lapply(seq_along(taus), function(j) {
    st <- set_tables[[j]]
    tab <- rbind(st$beta1, st$theta)
    inf <- inference[[j]]
    status <- collected$endpoint_draws[[j]]$status
    counts <- t(apply(status, 2, function(s) {
      c(
        n_bounded = sum(s == PAPER_ENDPOINT_STATUS[["bounded"]]),
        n_unbounded = sum(s == PAPER_ENDPOINT_STATUS[["unbounded"]]),
        n_unreliable = sum(s == PAPER_ENDPOINT_STATUS[["unreliable"]]),
        n_failed = sum(s == PAPER_ENDPOINT_STATUS[["failed"]])
      )
    }))
    width <- tab$set_upper - tab$set_lower
    reason <- ifelse(
      tab$status != PAPER_ENDPOINT_STATUS[["bounded"]], "full-sample set not certified bounded",
      ifelse(
        is.finite(width) & width <= 0, "point-identified (width 0)",
        ifelse(
          is.finite(inf$ci_lower), "reported",
          ifelse(
            inf$n_finite < min_reps, "insufficient bounded draws",
            "degenerate endpoint scale"
          )
        )
      )
    )
    data.frame(
      coef = tab$coef, tau = taus[j],
      set_lower = tab$set_lower, set_upper = tab$set_upper, width = width,
      set_status = tab$status, counts,
      n_finite = inf$n_finite, min_reps = min_reps,
      se_lower = inf$se_lower, se_upper = inf$se_upper,
      t_lower = ifelse(
        is.finite(inf$se_lower) & inf$se_lower > 0,
        tab$set_lower / inf$se_lower, NA_real_
      ),
      t_upper = ifelse(
        is.finite(inf$se_upper) & inf$se_upper > 0,
        tab$set_upper / inf$se_upper, NA_real_
      ),
      se_width = inf$se_width, width_lower = inf$width_lower,
      width_upper = inf$width_upper,
      rho = inf$rho,
      c_stoye = inf$c_stoye, c_im = inf$c_im,
      ci_lower = inf$ci_lower, ci_upper = inf$ci_upper,
      reason = reason, row.names = NULL, stringsAsFactors = FALSE
    )
  }))
}
