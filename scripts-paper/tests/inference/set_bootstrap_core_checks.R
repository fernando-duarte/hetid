# Offline checks for the vol-equation set-endpoint bootstrap's per-draw
# full-pipeline re-estimator and collector from the set-bootstrap core.
# logvar_box_seed/logvar_side_record/logvar_set_boot_collect/logvar_set_boot_prepare
# are pure functions exercised on hand-built inputs. logvar_set_boot_draw needs a
# real estimate_set_id_system + PPML/Harvey engine call, so it is exercised on a
# small synthetic heteroskedastic system to assert the return SHAPE and status
# vocabulary, not particular numeric bounds.

# logvar_box_seed -----------------------------------------------------------
check(
  "box seed is the midpoint of a finite box",
  identical(
    logvar_box_seed(list(set_lower = c(-1, 2), set_upper = c(1, 4))),
    c(0, 3)
  )
)
check(
  "box seed is 0 on an infinite bound",
  identical(
    logvar_box_seed(list(set_lower = c(-Inf, 2), set_upper = c(1, Inf))),
    c(0, 0)
  )
)

# logvar_side_record ---------------------------------------------------------
lsr_sch <- data.frame(
  coef = c("a", "b"), lower = c(-1, -2), upper = c(1, 2),
  lower_status = c("bounded", "unbounded"), upper_status = c("bounded", "bounded"),
  stringsAsFactors = FALSE
)
check(
  "side record reads the four vectors off a valid schema",
  identical(logvar_side_record(lsr_sch, c("a", "b")), list(
    lower = c(-1, -2), upper = c(1, 2),
    lower_status = c("bounded", "unbounded"), upper_status = c("bounded", "bounded")
  ))
)
lsr_na <- logvar_side_record(NULL, c("a", "b"))
check(
  "a NULL schema becomes an all-failed record of the right length",
  identical(lsr_na$lower_status, c("failed", "failed")) &&
    identical(lsr_na$upper_status, c("failed", "failed")) &&
    all(is.na(lsr_na$lower)) && all(is.na(lsr_na$upper))
)
lsr_mismatch <- logvar_side_record(lsr_sch, c("a", "c"))
check(
  "a coef-label mismatch also becomes an all-failed record",
  identical(lsr_mismatch$lower_status, c("failed", "failed")) &&
    length(lsr_mismatch$lower) == 2L
)

# logvar_set_boot_collect ----------------------------------------------------
lsc_spec <- list(coefs = c("a", "b"), taus = c(0.05, 0.2))
lsc_rec <- function(lo, up, lst, ust) {
  list(lower = lo, upper = up, lower_status = lst, upper_status = ust)
}
lsc_good <- list(
  ppml = list(
    lsc_rec(c(-1, -2), c(1, 2), c("bounded", "bounded"), c("bounded", "bounded")),
    lsc_rec(c(-1.5, NA), c(1.5, NA), c("bounded", "unbounded"), c("bounded", "unbounded"))
  ),
  harvey = list(
    lsc_rec(c(-3, -4), c(3, 4), c("bounded", "bounded"), c("bounded", "bounded")),
    lsc_rec(c(-3.5, -4.5), c(3.5, 4.5), c("bounded", "bounded"), c("bounded", "bounded"))
  )
)
lsc_collected <- logvar_set_boot_collect(list(lsc_good, "solver exploded"), lsc_spec)
check(
  "collect stacks draws into B x p matrices with the spec's coefficient names",
  identical(dim(lsc_collected$ppml[[1]]$lower), c(2L, 2L)) &&
    identical(colnames(lsc_collected$ppml[[1]]$lower), c("a", "b"))
)
check(
  "collect preserves per-estimator per-tau values on the good draw",
  identical(unname(lsc_collected$harvey[[2]]$upper[1, ]), c(3.5, 4.5))
)
check(
  "an errored (character) draw becomes an all-failed row, never dropped",
  nrow(lsc_collected$ppml[[1]]$lower) == 2L &&
    all(is.na(lsc_collected$ppml[[1]]$lower[2, ])) &&
    all(lsc_collected$ppml[[1]]$lower_status[2, ] == "failed") &&
    all(lsc_collected$harvey[[2]]$upper_status[2, ] == "failed")
)

# logvar_set_boot_prepare -----------------------------------------------------
lbp_data <- data.frame(qtr = 1:6, y = rnorm(6))
lbp_lag <- data.frame(qtr = 1:6, l.pc1 = rnorm(6), l.pc2 = rnorm(6))
lbp_out <- logvar_set_boot_prepare(list(data = lbp_data), lbp_lag)
check(
  "prepare returns the joined pc columns and the original row count",
  identical(lbp_out$pc_cols, c("l.pc1", "l.pc2")) && nrow(lbp_out$data) == 6L
)
check(
  "prepare's join carries the lagged pc values by qtr",
  identical(lbp_out$data$l.pc1, lbp_lag$l.pc1)
)
check(
  "prepare's gapless stopifnot fires on a qtr gap",
  inherits(
    tryCatch(
      logvar_set_boot_prepare(list(data = lbp_data[-3, ]), lbp_lag[-3, ]),
      error = function(e) e
    ),
    "error"
  )
)

# logvar_set_boot_draw --------------------------------------------------------
# a small synthetic triangular system: a heteroskedastic Y2 (news) block
# drives the Lewbel identification, plus two independent PC columns feed the
# auxiliary log-variance regression.
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
  grid_cap = 5L, fit_budget = 300,
  build_ppml = function(w1, w2, pcr, qtr, b_point) {
    logvar_ppml_estimator(
      w1, w2, pcr, qtr, b_point,
      scale_anchor_b = c(0, 0), scale_anchor_source = "test"
    )
  },
  build_harvey = function(w1, w2, pcr, qtr, b_point, ppml_obj) {
    logvar_harvey_estimator(
      w1, w2, pcr, qtr, b_point,
      ppml_bundle = if (!is.null(ppml_obj)) ppml_obj$start_bundle else NULL,
      ppml_start_at_b = if (!is.null(ppml_obj)) ppml_obj$fit_at_b else NULL
    )
  }
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

# build_harvey warm-starts from the draw's ppml_obj (ppml_bundle/ppml_start_at_b)
check(
  "harvey warm-starts from the draw PPML fit (not cold-start mass-failure)",
  !any(unlist(lapply(lbd_draw$ppml, `[`, c("lower_status", "upper_status"))) != "failed") ||
    any(unlist(lapply(lbd_draw$harvey, `[`, c("lower_status", "upper_status"))) != "failed")
)

# a point-deficient draw (z has no variation -> moments carry no
# identifying heteroskedasticity signal) yields all-"failed"/NA without erroring
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

# collecting real draws from logvar_set_boot_draw stacks cleanly
lbd_collected <- logvar_set_boot_collect(list(lbd_draw, "boom"), lbd_spec)
check(
  "collect stacks real logvar_set_boot_draw output into B x p matrices",
  identical(dim(lbd_collected$ppml[[1]]$lower), c(2L, 3L)) &&
    identical(colnames(lbd_collected$harvey[[1]]$upper), lbd_spec$coefs) &&
    all(lbd_collected$ppml[[1]]$lower_status[2, ] == "failed")
)
