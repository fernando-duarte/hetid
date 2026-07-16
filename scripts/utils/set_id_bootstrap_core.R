# Core machinery for the set-identification endpoint bootstrap: the one-draw
# re-estimation, the draw collection, and the diagnostics table, all
# parameterized by a spec list so drivers stay thin and the pieces are testable
# on synthetic systems. Statuses are decoupled: a rank-deficient
# tau = 0 system yields an NA point without discarding the draw's endpoint
# evaluations, every display tau is solved regardless of the draw's tau*, and
# each coefficient-tau side carries the shared solvers' own three-state
# status. This numbered-pipeline copy is tested in
# scripts/utils/tests/test_set_id_bootstrap_core.R; scripts-paper owns an
# independent implementation under scripts-paper/support/identification/.

# Re-estimate the mean-equation system on one data frame: the W1/W2
# residualizations, the de-meaned instrument, the identification moments, and
# the closed-form tau = 0 point. Its full-sample and per-draw callers should use
# this same implementation so their recipes cannot drift apart.
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
  qs0 <- build_pipeline_quadratic_system(
    spec$gamma, rep(0, ncol(spec$gamma)), moments
  )
  list(
    beta1r = beta1r, w1 = w1, beta2r = beta2r, w2 = w2, z = z,
    moments = moments, point0 = solve_point_identification(qs0$components)
  )
}

# Re-estimate the system on one resampled frame and evaluate the tau = 0
# point, the per-coefficient intervals at every display slack, and tau*.
set_id_boot_draw <- function(dat, spec) {
  est <- estimate_set_id_system(dat, spec)
  point <- if (is.null(est$point0)) {
    rep(NA_real_, length(spec$coefs))
  } else {
    c(
      hetid::recover_structural_coefficients(
        est$beta1r, est$beta2r, est$point0$theta
      ),
      est$point0$theta
    )
  }
  bounds <- lapply(spec$taus, function(tau) {
    it <- coef_interval_tables(
      spec$gamma, tau, est$moments, est$beta1r, est$beta2r
    )
    tab <- rbind(it$beta1, it$theta)
    ok <- tab$status == "bounded"
    list(
      lower = ifelse(ok, tab$set_lower, NA_real_),
      upper = ifelse(ok, tab$set_upper, NA_real_),
      status = tab$status
    )
  })
  coarse <- sweep_fixed_gamma(spec$gamma, est$moments, spec$tau_grid, "boot")
  ts <- tau_star_fixed(spec$gamma, est$moments, coarse, iters = 15L)
  list(
    point = point, point_ok = !is.null(est$point0),
    bounds = bounds, tau_star = ts$tau_star, capped = ts$capped
  )
}

# Collect raw draw results into per-tau endpoint/status matrices, the point-
# draw matrix, and the tau* vector. Errored draws arrive as their condition
# message and become all-NA rows with status "failed".
set_id_boot_collect <- function(boot_raw, spec) {
  n_coef <- length(spec$coefs)
  failed <- vapply(boot_raw, is.character, logical(1))
  causes <- if (any(failed)) table(unlist(boot_raw[failed])) else NULL
  na_draw <- list(
    point = rep(NA_real_, n_coef), point_ok = FALSE,
    bounds = rep(
      list(list(
        lower = rep(NA_real_, n_coef),
        upper = rep(NA_real_, n_coef),
        status = rep("failed", n_coef)
      )),
      length(spec$taus)
    ),
    tau_star = NA_real_, capped = FALSE
  )
  boot_raw[failed] <- list(na_draw)
  point_draws <- do.call(rbind, lapply(boot_raw, `[[`, "point"))
  colnames(point_draws) <- spec$coefs
  endpoint_draws <- lapply(seq_along(spec$taus), function(j) {
    m <- function(field) {
      out <- do.call(rbind, lapply(boot_raw, function(d) d$bounds[[j]][[field]]))
      colnames(out) <- spec$coefs
      out
    }
    list(lower = m("lower"), upper = m("upper"), status = m("status"))
  })
  list(
    point_draws = point_draws,
    n_point_deficient = sum(
      !vapply(boot_raw, `[[`, logical(1), "point_ok")
    ) - sum(failed),
    endpoint_draws = endpoint_draws,
    tau_star_draws = vapply(boot_raw, `[[`, numeric(1), "tau_star"),
    n_capped = sum(vapply(boot_raw, `[[`, logical(1), "capped")),
    n_failed = sum(failed),
    failure_causes = causes
  )
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
        n_bounded = sum(s == "bounded"),
        n_unbounded = sum(s == "unbounded"),
        n_unreliable = sum(s == "unreliable"),
        n_failed = sum(s == "failed")
      )
    }))
    width <- tab$set_upper - tab$set_lower
    reason <- ifelse(
      tab$status != "bounded", "full-sample set not certified bounded",
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
      se_width = inf$se_width, width_p05 = inf$width_p05,
      width_p95 = inf$width_p95,
      rho = inf$rho,
      c_stoye = inf$c_stoye, c_im = inf$c_im,
      ci_lower = inf$ci_lower, ci_upper = inf$ci_upper,
      reason = reason, row.names = NULL, stringsAsFactors = FALSE
    )
  }))
}
