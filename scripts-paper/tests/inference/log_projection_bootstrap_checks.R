# The regularized log projections in the set bootstrap: real draws on the
# set_bootstrap_draw_checks.R fixture under estimated news (the stage refuses
# imposed-null news for them), anchor equality with the published recipe,
# resampled and point-deficient frames, the uncertified-Fuller hook, and the
# stored control. Sourced by test_set_bootstrap.R after the real-callback
# fixture, whose check() and lbd_* objects it reuses.

lpb_ids <- c("ppml", "harvey", "log_plus", "log_fuller")
lpb_spec <- lbd_spec
lpb_spec$impose_null <- FALSE
lpb_spec$estimator_ids <- lpb_ids
lpb_spec$builders <- logvar_set_boot_builders(
  scale_value = 1,
  logols_coef = stats::setNames(rep(0, length(lbd_spec$coefs)), lbd_spec$coefs),
  estimator_ids = lpb_ids
)
lpb_spec2 <- lpb_spec
lpb_spec2$estimator_ids <- lpb_ids[1:2]
lpb_spec2$builders <- lpb_spec$builders[1:2]
lpb_new <- c("log_plus", "log_fuller")
lpb_point_ok <- function(draw, status) {
  all(vapply(lpb_new, function(id) {
    cell <- draw[[id]][[1L]]
    all(cell$point_status == status) &&
      (status != "bounded" || all(is.finite(cell$point)))
  }, logical(1)))
}

lpb_draw <- logvar_set_boot_draw(lbd_dat, lpb_spec)
check(
  "the projections join the draw with bounded points, leaving PPML and Harvey bit-identical",
  identical(names(lpb_draw), lpb_ids) &&
    lpb_point_ok(lpb_draw, "bounded") &&
    identical(lpb_draw[1:2], logvar_set_boot_draw(lbd_dat, lpb_spec2))
)

# anchor equality: a strict volatility subset, the published recipe with
# non-positional identifiers and raw PCs centered once by the package
lpb_dat <- lbd_dat
lpb_dat$l.pc1[1:2] <- NA
lpb_draw_na <- logvar_set_boot_draw(lpb_dat, lpb_spec)
lpb_compat <- logvar_set_boot_compat_spec(lpb_spec)
lpb_est <- estimate_set_id_system(lpb_dat, lpb_compat)
lpb_keep <- stats::complete.cases(lpb_dat[lpb_compat$pc_cols])
lpb_ids_pub <- paste0("q", lpb_dat$qtr)
lpb_prep <- hetid::prepare_log_projection(
  lpb_est$w1, lpb_est$w2, as.matrix(lpb_dat[lpb_keep, lpb_compat$pc_cols]),
  lpb_ids_pub, lpb_ids_pub[lpb_keep]
)
for (id in lpb_new) {
  lpb_pub <- logvar_log_projection_estimator(
    lpb_prep, id, id, hetid::LOG_PROJECTION_CONTROL$MULTIPLIER, "published",
    LOGVAR_LOG_PROJECTION_CONTROL
  )
  lpb_fit <- lpb_pub$fit_at_b(lpb_est$point0$theta)
  check(
    paste(id, "anchor matches its published coefficient recipe"),
    logvar_fit_ok(lpb_fit) &&
      identical(names(lpb_fit$coef), lbd_spec$coefs) &&
      identical(lpb_draw_na[[id]][[1L]]$point, unname(lpb_fit$coef))
  )
}

# a resample repeats quarters: positional identifiers, the resample's own scale
lpb_rs <- lbd_dat[c(1:30, 1:30, 31:150), ]
lpb_est_rs <- estimate_set_id_system(lpb_rs, lpb_compat)
lpb_rows_rs <- bootstrap_stage_logvar_rows(lpb_rs, lpb_est_rs, lpb_compat, "qtr")
lpb_obj_rs <- lpb_spec$builders$log_plus(
  lpb_rows_rs$w1, lpb_rows_rs$w2, as.matrix(lpb_rows_rs$pc_data), lpb_rows_rs$key,
  lpb_est_rs$point0$theta, list(),
  list(
    w1 = lpb_est_rs$w1, w2 = lpb_est_rs$w2, pc_raw = as.matrix(lpb_rows_rs$pc_data),
    volatility_rows = which(bootstrap_stage_logvar_complete(lpb_rs, lpb_compat))
  )
)
lpb_prep_rs <- get("prep", environment(lpb_obj_rs$fit_at_b))
check(
  "a resample with repeated quarters builds and rescales from its own residuals",
  lpb_point_ok(logvar_set_boot_draw(lpb_rs, lpb_spec), "bounded") &&
    isTRUE(all.equal(lpb_prep_rs$log_scale_common, log(mean(lpb_est_rs$w1^2))))
)

lpb_deficient <- lbd_dat
lpb_deficient$z <- 0
check(
  "a point-deficient draw records unreliable projection points, not failures",
  lpb_point_ok(logvar_set_boot_draw(lpb_deficient, lpb_spec), "unreliable")
)

# collinear news residuals leave the Fuller scale uncertified (spec B: estimated,
# mean-zero news)
lpb_u <- rep(c(-1, 1), 8L)
lpb_v <- rep(c(-1, -1, 1, 1), 4L)
lpb_sample <- list(
  w1 = lpb_v, w2 = cbind(a = lpb_u, b = lpb_u + 2^-40 * lpb_v),
  pc_raw = cbind(l.pc1 = stats::rnorm(16L)), volatility_rows = seq_len(16L)
)
lpb_direct <- function(id) {
  lpb_spec$builders[[id]](
    lpb_sample$w1, lpb_sample$w2, lpb_sample$pc_raw, seq_len(16L), NULL, list(),
    lpb_sample
  )
}
check(
  "collinear news residuals give Fuller the uncertified hook and log_plus none",
  !is.null(lpb_direct("log_fuller")$analyze_domain) &&
    is.null(lpb_direct("log_plus")$analyze_domain)
)

lpb_lv <- bsr_stage_spec$log_variance
lpb_lv$fit_budget <- 300L
lpb_renamed <- lpb_lv
names(lpb_renamed$log_projection_control)[[1L]] <- "renamed"
check(
  "the stored log-projection control is validated by name",
  isTRUE(bootstrap_stage_controls_ok(lpb_lv)) &&
    !isTRUE(bootstrap_stage_controls_ok(lpb_renamed))
)
