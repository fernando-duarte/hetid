set.seed(20260714L)
lbd_n <- 150L
lbd_z <- exp(rnorm(lbd_n, 0, 0.4))
lbd_z <- lbd_z - mean(lbd_z)
lbd_e2 <- cbind(rnorm(lbd_n) * (1 + 0.9 * lbd_z), rnorm(lbd_n) * (1 + 0.7 * lbd_z))
lbd_x <- rnorm(lbd_n)
lbd_y1 <- 0.5 + 0.3 * lbd_x + 0.2 * lbd_e2[, 1] - 0.1 * lbd_e2[, 2] + rnorm(lbd_n, 0, 0.2)
lbd_dat <- data.frame(
  qtr = seq_len(lbd_n), y1 = lbd_y1, x = lbd_x,
  w2a = lbd_e2[, 1], w2b = lbd_e2[, 2], z = lbd_z,
  l.pc1 = rnorm(lbd_n), l.pc2 = rnorm(lbd_n)
)
lbd_spec <- list(
  coefs = c("(Intercept)", "l.pc1", "l.pc2"),
  gamma = matrix(1, 1, 2), taus = c(0, 0.05, 0.2),
  x_cols = "x", y1_col = "y1", y2_cols = c("w2a", "w2b"), z_col = "z",
  impose_null = TRUE, pc_cols = c("l.pc1", "l.pc2"),
  grid_cap = 5L,
  fit_budget = 300,
  estimator_ids = c("ppml", "harvey"),
  builders = list(
    ppml = function(w1, w2, pcr, qtr, b_point, built) {
      logvar_ppml_estimator(
        w1, w2, pcr, qtr, b_point,
        scale_anchor_b = c(0, 0),
        scale_anchor_source = "test"
      )
    },
    harvey = function(w1, w2, pcr, qtr, b_point, built) {
      ppml_obj <- built[["ppml"]]
      logvar_harvey_estimator(
        w1, w2, pcr, qtr, b_point,
        ppml_bundle = if (!is.null(ppml_obj)) {
          ppml_obj$start_bundle
        } else {
          NULL
        },
        ppml_start_at_b = if (!is.null(ppml_obj)) {
          ppml_obj$fit_at_b
        } else {
          NULL
        },
        ppml_bundle_source_id = ppml_obj$metadata$spec_id,
        ppml_start_at_b_source_id = ppml_obj$metadata$spec_id
      )
    }
  )
)
lbd_draw <- logvar_set_boot_draw(lbd_dat, lbd_spec)
lbd_allowed <- c("bounded", "unbounded", "unreliable", "failed")
check(
  "a draw returns the ppml/harvey x per-tau nested shape",
  identical(names(lbd_draw), c("ppml", "harvey")) &&
    length(lbd_draw$ppml) == length(lbd_spec$taus) &&
    length(lbd_draw$harvey) == length(lbd_spec$taus)
)
check(
  "every per-tau record has the four length-n_coef vectors with allowed statuses",
  all(vapply(c("ppml", "harvey"), function(e) {
    all(vapply(lbd_draw[[e]], function(r) {
      length(r$lower) == 3L && length(r$upper) == 3L &&
        all(r$lower_status %in% lbd_allowed) && all(r$upper_status %in% lbd_allowed)
    }, logical(1)))
  }, logical(1)))
)
# At tau = 0 the news set is the single point b_point, so that slot is one direct
# evaluation there: point is authoritative, the two sides are exact copies kept
# only for the pooled failure gate, and a point cannot diverge.
lbd_t0 <- lapply(lbd_draw, `[[`, 1L)
check(
  "the tau=0 slot carries a point whose two mirrors are bitwise identical",
  all(vapply(lbd_t0, function(r) {
    length(r$point) == 3L && length(r$point_status) == 3L &&
      identical(r$lower, r$point) && identical(r$upper, r$point) &&
      identical(r$lower_status, r$point_status) &&
      identical(r$upper_status, r$point_status)
  }, logical(1)))
)
check(
  "every tau=0 point is directly evaluated and no tau=0 status is unbounded",
  all(vapply(lbd_t0, function(r) {
    all(r$point_status %in% lbd_allowed) &&
      !any(r$point_status == "unbounded") &&
      all(r$point_status == "bounded") && all(is.finite(r$point))
  }, logical(1)))
)
# Pin the seam the direct evaluation rests on: fit_at_b returns its estimates in
# $coef on the spec's coefficient axis, and the stored point is that evaluation
# rather than a re-derived or stored-at-construction quantity.
lbd_compat <- logvar_set_boot_compat_spec(lbd_spec)
lbd_est <- estimate_set_id_system(lbd_dat, lbd_compat)
lbd_rows <- bootstrap_stage_logvar_rows(
  lbd_dat, lbd_est, lbd_compat, lbd_compat$key_col
)
lbd_pcr <- paper_normalize_model_matrix(
  lbd_rows$pc_data, lbd_compat$pc_preprocessing
)
colnames(lbd_pcr) <- lbd_compat$pc_cols
lbd_ppml_obj <- lbd_compat$builders$ppml(
  lbd_rows$w1, lbd_rows$w2, lbd_pcr, lbd_rows$key, lbd_est$point0$theta, list()
)
lbd_ppml_fit <- lbd_ppml_obj$fit_at_b(lbd_est$point0$theta)
check(
  "fit_at_b returns $coef on the spec's coefficient axis, in spec order",
  identical(lbd_ppml_obj$coef_labels, lbd_spec$coefs) &&
    identical(names(lbd_ppml_fit$coef), lbd_spec$coefs) &&
    logvar_fit_ok(lbd_ppml_fit)
)
# fit_at_b and the constructor's stored point fit are not interchangeable: the
# constructor evaluates without the fallback-start ladder, so on this fixture the
# two disagree in the last bit (5.6e-16). fit_at_b is what the published
# full-sample point column uses, so it is what the draws must record.
check(
  "the stored tau=0 point is that direct evaluation, coefficient for coefficient",
  identical(lbd_draw$ppml[[1]]$point, unname(lbd_ppml_fit$coef))
)
check(
  "harvey warm-starts from the draw PPML fit (not cold-start mass-failure)",
  !any(unlist(lapply(lbd_draw$ppml, `[`, c("lower_status", "upper_status"))) != "failed") ||
    any(unlist(lapply(lbd_draw$harvey, `[`, c("lower_status", "upper_status"))) != "failed")
)
lbd_dat_deficient <- lbd_dat
lbd_dat_deficient$z <- 0
lbd_draw_deficient <- tryCatch(
  logvar_set_boot_draw(lbd_dat_deficient, lbd_spec),
  error = function(e) e
)
check(
  "a point-deficient draw does not error",
  !inherits(lbd_draw_deficient, "error")
)
check(
  "a point-deficient draw's records stay within the allowed status vocabulary",
  all(vapply(c("ppml", "harvey"), function(e) {
    all(vapply(lbd_draw_deficient[[e]], function(r) {
      all(r$lower_status %in% lbd_allowed) && all(r$upper_status %in% lbd_allowed)
    }, logical(1)))
  }, logical(1)))
)
# a rank-deficient tau = 0 system leaves no news point to evaluate at all
check(
  "a point-deficient draw's tau=0 slot is unreliable, never unbounded",
  all(vapply(lapply(lbd_draw_deficient, `[[`, 1L), function(r) {
    identical(r$point_status, rep("unreliable", 3L)) &&
      all(is.na(r$point)) && identical(r$lower, r$point) &&
      identical(r$lower_status, r$point_status)
  }, logical(1)))
)
lbd_collected <- logvar_set_boot_collect(list(lbd_draw, "boom"), lbd_spec)
check(
  "collect stacks real logvar_set_boot_draw output into B x p matrices",
  identical(dim(lbd_collected$ppml[[1]]$lower), c(2L, 3L)) &&
    identical(colnames(lbd_collected$harvey[[1]]$upper), lbd_spec$coefs) &&
    all(lbd_collected$ppml[[1]]$lower_status[2, ] == "failed")
)
check(
  "the tau=0 mirrors survive collection bitwise, point fields and all",
  identical(lbd_collected$ppml[[1]]$lower, lbd_collected$ppml[[1]]$point) &&
    identical(lbd_collected$ppml[[1]]$upper, lbd_collected$ppml[[1]]$point) &&
    identical(
      lbd_collected$harvey[[1]]$lower_status,
      lbd_collected$harvey[[1]]$point_status
    ) &&
    identical(
      lbd_collected$harvey[[1]]$upper_status,
      lbd_collected$harvey[[1]]$point_status
    ) &&
    !any(lbd_collected$ppml[[1]]$point_status == "unbounded")
)
