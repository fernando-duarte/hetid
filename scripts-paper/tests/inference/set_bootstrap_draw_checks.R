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
lbd_collected <- logvar_set_boot_collect(list(lbd_draw, "boom"), lbd_spec)
check(
  "collect stacks real logvar_set_boot_draw output into B x p matrices",
  identical(dim(lbd_collected$ppml[[1]]$lower), c(2L, 3L)) &&
    identical(colnames(lbd_collected$harvey[[1]]$upper), lbd_spec$coefs) &&
    all(lbd_collected$ppml[[1]]$lower_status[2, ] == "failed")
)
