# Core machinery for the set-identification endpoint bootstrap: the one-draw
# re-estimation, the draw collection, and the diagnostics table, all
# parameterized by a spec list so the paper driver stays thin and the pieces
# are testable on synthetic systems. Statuses are decoupled: a rank-deficient
# tau = 0 system yields an NA point without discarding the draw's endpoint
# evaluations, every display tau is solved regardless of the draw's tau*, and
# each coefficient-tau side carries the shared solvers' own three-state
# status. Consumed by scripts-paper/set_id_bootstrap.R; tested in
# scripts/utils/tests/test_set_id_bootstrap_core.R.

# Re-estimate the mean-equation system on one resampled frame and evaluate
# the tau = 0 point, the per-coefficient intervals at every display slack,
# and tau*.
set_id_boot_draw <- function(dat, spec) {
  fit1 <- stats::lm(
    stats::reformulate(spec$x_cols, response = spec$y1_col),
    data = dat
  )
  b1r <- stats::coef(fit1)
  w1b <- stats::residuals(fit1)
  if (spec$impose_null) {
    w2b <- as.matrix(dat[spec$y2_cols])
    b2r <- matrix(
      0, length(spec$y2_cols), length(b1r),
      dimnames = list(spec$y2_cols, names(b1r))
    )
  } else {
    fit2 <- stats::lm(
      as.matrix(dat[spec$y2_cols]) ~ .,
      data = dat[spec$x_cols]
    )
    w2b <- stats::residuals(fit2)
    b2r <- t(stats::coef(fit2))
  }
  zb <- dat[[spec$z_col]] - mean(dat[[spec$z_col]])
  mom <- hetid::compute_identification_moments(
    w1b, w2b, matrix(zb, ncol = 1, dimnames = list(NULL, spec$z_col))
  )
  qs0 <- build_pipeline_quadratic_system(
    spec$gamma, rep(0, ncol(spec$gamma)), mom
  )
  point0 <- solve_point_identification(qs0$components)
  point <- if (is.null(point0)) {
    rep(NA_real_, length(spec$coefs))
  } else {
    c(
      hetid::recover_structural_coefficients(b1r, b2r, point0$theta),
      point0$theta
    )
  }
  bounds <- lapply(spec$taus, function(tau) {
    it <- coef_interval_tables(spec$gamma, tau, mom, b1r, b2r)
    tab <- rbind(it$beta1, it$theta)
    ok <- tab$status == "bounded"
    list(
      lower = ifelse(ok, tab$set_lower, NA_real_),
      upper = ifelse(ok, tab$set_upper, NA_real_),
      status = tab$status
    )
  })
  coarse <- sweep_fixed_gamma(spec$gamma, mom, spec$tau_grid, "boot")
  ts <- tau_star_fixed(spec$gamma, mom, coarse, iters = 15L)
  list(
    point = point, point_ok = !is.null(point0),
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
# status draw counts, robust scales and correlation, the calibrated interval,
# and the reason a table cell renders blank.
set_id_boot_diagnostics <- function(collected, inference, set_tables, taus) {
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
    min_reps <- nrow(status) %/% 2L
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
      set_lower = tab$set_lower, set_upper = tab$set_upper,
      set_status = tab$status, counts,
      n_finite = inf$n_finite, min_reps = min_reps,
      se_lower = inf$se_lower, se_upper = inf$se_upper, rho = inf$rho,
      c_stoye = inf$c_stoye, c_im = inf$c_im,
      ci_lower = inf$ci_lower, ci_upper = inf$ci_upper,
      reason = reason, row.names = NULL, stringsAsFactors = FALSE
    )
  }))
}
