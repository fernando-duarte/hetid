# Core machinery for the vol-equation set-endpoint bootstrap: build the augmented
# resampling frame, then on one resampled frame re-estimate the mean equation,
# rebuild the log-var inputs, and re-run BOTH estimators' set endpoints at every
# tau. The reports require the whole pipeline (mean eq -> Lewbel set -> map ->
# endpoints) to run every draw; the set is never held fixed. Reuses the
# paper-owned estimate_set_id_system / coef_interval_tables /
# tau_quadratic_system definitions so the mean-eq recipe cannot drift.
# Consumed by run_set_bootstrap.R.

# Augment the mean-eq frame with the lagged asset-return PC columns (join by qtr),
# so one moving-block resample carries everything the mean-eq re-estimation and the
# log-var inputs need. The PCs are conditioned on (resampled, not re-estimated).
logvar_set_boot_prepare <- function(mean_eq, lag_pc) {
  pc_cols <- setdiff(names(lag_pc), "qtr")
  aug <- dplyr::left_join(mean_eq$data, lag_pc, by = "qtr")
  stopifnot(
    nrow(aug) == nrow(mean_eq$data),
    all(diff(as.numeric(aug$qtr)) == 1L) # gapless: blocks assume adjacency
  )
  list(data = aug, pc_cols = pc_cols)
}

# Finite per-tau search seed from the coefficient box midpoint, else 0.
logvar_box_seed <- function(box) {
  mid <- (box$set_lower + box$set_upper) / 2
  ifelse(is.finite(mid), mid, 0)
}

# Run one estimator's endpoints at every tau on already-prepared draw inputs. The
# builder returns an estimator object (or NULL on failure); a per-tau tryCatch
# demotes a failed solve to an all-"failed" record rather than dropping the draw.
logvar_run_estimator <- function(est_obj, spec, boxes, qss, b_point) {
  lapply(seq_along(spec$taus), function(j) {
    seed <- if (!is.null(b_point)) b_point else logvar_box_seed(boxes[[j]])
    sch <- if (is.null(est_obj)) {
      NULL
    } else {
      tryCatch(
        logvar_engine_set_at_tau(
          est_obj, qss[[j]], boxes[[j]],
          b_seed = seed,
          max_grid_points = spec$grid_cap, max_fit_evals = spec$fit_budget,
          cold_start_check = FALSE, tau = spec$taus[j]
        )$schema,
        error = function(e) NULL
      )
    }
    logvar_side_record(sch, spec$coefs)
  })
}

# One resampled draw: re-estimate the mean eq, restrict to the draw's complete
# lagged-PC rows, de-mean pcr, then run PPML and (warm-started from THIS draw's
# PPML fit, per the Harvey report) Harvey at every tau. Estimator constructors are
# called directly (the frozen sample_contract validators do not apply to a
# resample).
logvar_set_boot_draw <- function(dat, spec) {
  est <- estimate_set_id_system(dat, spec)
  lv <- stats::complete.cases(dat[, spec$pc_cols, drop = FALSE])
  w1 <- est$w1[lv]
  w2 <- est$w2[lv, , drop = FALSE]
  qtr <- dat$qtr[lv]
  pcr <- scale(as.matrix(dat[lv, spec$pc_cols]), center = TRUE, scale = FALSE)
  colnames(pcr) <- spec$pc_cols
  b_point <- if (is.null(est$point0)) NULL else est$point0$theta
  boxes <- lapply(spec$taus, function(tau) {
    coef_interval_tables(spec$gamma, tau, est$moments, est$beta1r, est$beta2r)$theta
  })
  qss <- lapply(spec$taus, function(tau) tau_quadratic_system(spec$gamma, tau, est$moments))
  ppml_obj <- tryCatch(spec$build_ppml(w1, w2, pcr, qtr, b_point), error = function(e) NULL)
  harvey_obj <- tryCatch(
    spec$build_harvey(w1, w2, pcr, qtr, b_point, ppml_obj),
    error = function(e) NULL
  )
  list(
    ppml = logvar_run_estimator(ppml_obj, spec, boxes, qss, b_point),
    harvey = logvar_run_estimator(harvey_obj, spec, boxes, qss, b_point)
  )
}

# Pull the four per-side vectors from a schema (or an all-"failed" stand-in).
logvar_side_record <- function(sch, coefs) {
  if (is.null(sch) || !identical(sch$coef, coefs)) {
    n <- length(coefs)
    return(list(
      lower = rep(NA_real_, n), upper = rep(NA_real_, n),
      lower_status = rep("failed", n), upper_status = rep("failed", n)
    ))
  }
  list(
    lower = sch$lower, upper = sch$upper,
    lower_status = sch$lower_status, upper_status = sch$upper_status
  )
}

# Stack raw draws (errored draws arrive as condition strings) into per-estimator
# per-tau B x p matrices for logvar_endpoint_envelope. Failed draws become
# all-"failed" rows -- never dropped.
logvar_set_boot_collect <- function(raw, spec) {
  na_est <- lapply(spec$taus, function(...) logvar_side_record(NULL, spec$coefs))
  fix <- function(d) if (is.character(d)) list(ppml = na_est, harvey = na_est) else d
  raw <- lapply(raw, fix)
  stack <- function(which_est, j, field) {
    out <- do.call(rbind, lapply(raw, function(d) d[[which_est]][[j]][[field]]))
    colnames(out) <- spec$coefs
    out
  }
  per_est <- function(which_est) {
    lapply(seq_along(spec$taus), function(j) {
      list(
        lower = stack(which_est, j, "lower"),
        upper = stack(which_est, j, "upper"),
        lower_status = stack(which_est, j, "lower_status"),
        upper_status = stack(which_est, j, "upper_status")
      )
    })
  }
  list(ppml = per_est("ppml"), harvey = per_est("harvey"))
}
