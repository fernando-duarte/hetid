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
lsr_coefs <- c("a", "b")
lsr_est <- function(labels, fit) {
  list(coef_labels = labels, fit_at_b = function(b, start = NULL) fit)
}
lsr_pt_failed <- logvar_failed_record(lsr_coefs, point = TRUE)
check(
  "a failed tau=0 record mirrors an all-failed point carrying no value",
  identical(lsr_pt_failed$point_status, c("failed", "failed")) &&
    identical(lsr_pt_failed$lower_status, lsr_pt_failed$point_status) &&
    identical(lsr_pt_failed$upper, lsr_pt_failed$point) &&
    all(is.na(lsr_pt_failed$point))
)
check(
  "an absent estimator context becomes a failed tau=0 record",
  identical(logvar_point_record(NULL, c(0, 0), lsr_coefs), lsr_pt_failed)
)
check(
  "a coefficient-axis disagreement becomes a failed tau=0 record",
  identical(
    logvar_point_record(lsr_est(c("a", "c"), NULL), c(0, 0), lsr_coefs),
    lsr_pt_failed
  )
)
lsr_pt_reject <- logvar_point_record(
  lsr_est(lsr_coefs, list(fit_status = "nonconvergence", converged = FALSE)),
  c(0, 0), lsr_coefs
)
check(
  "a rejected direct fit is unreliable and never unbounded",
  identical(lsr_pt_reject$point_status, c("unreliable", "unreliable")) &&
    identical(lsr_pt_reject$lower_status, lsr_pt_reject$point_status) &&
    identical(lsr_pt_reject$upper, lsr_pt_reject$point) &&
    all(is.na(lsr_pt_reject$point))
)
lsr_evals <- 0L
lsr_counted <- list(
  coef_labels = lsr_coefs,
  fit_at_b = function(b, start = NULL) {
    lsr_evals <<- lsr_evals + 1L
    NULL
  }
)
check(
  "a missing tau=0 point is unreliable without evaluating any fit",
  identical(logvar_point_record(lsr_counted, NULL, lsr_coefs), lsr_pt_reject) &&
    identical(lsr_evals, 0L)
)
lsr_pt_ok <- logvar_point_record(
  lsr_est(lsr_coefs, list(
    coef = c(a = 1.5, b = -0.5),
    fit_status = LOGVAR_FIT_STATUS[["ok"]], converged = TRUE
  )),
  c(0, 0), lsr_coefs
)
check(
  "an accepted direct fit is a bounded point holding the fit's coefficients",
  identical(lsr_pt_ok$point, c(1.5, -0.5)) &&
    identical(lsr_pt_ok$point_status, c("bounded", "bounded")) &&
    identical(lsr_pt_ok$lower, lsr_pt_ok$point) &&
    identical(lsr_pt_ok$upper_status, lsr_pt_ok$point_status)
)
# One fixture on a valid union axis: slot one is the tau = 0 point evaluation,
# the rest are searched slots. A tau axis with no zero is not a legal logvar axis
# (bootstrap_stage_logvar_tau0_slot rejects it), so there is no 2-slot variant.
lsc_spec <- list(
  coefs = c("a", "b"),
  taus = c(0, 0.05, 0.2),
  estimator_ids = c("ppml", "harvey")
)
lsc_rec <- function(lo, up, lst, ust) {
  list(lower = lo, upper = up, lower_status = lst, upper_status = ust)
}
lsc_good <- list(
  ppml = list(
    logvar_point_mirrors(c(-1, 2), c("bounded", "bounded")),
    lsc_rec(c(-1, -2), c(1, 2), c("bounded", "bounded"), c("bounded", "bounded")),
    lsc_rec(c(-1.5, NA), c(1.5, NA), c("bounded", "unbounded"), c("bounded", "unbounded"))
  ),
  harvey = list(
    logvar_point_mirrors(c(-3, NA), c("bounded", "unreliable")),
    lsc_rec(c(-3, -4), c(3, 4), c("bounded", "bounded"), c("bounded", "bounded")),
    lsc_rec(c(-3.5, -4.5), c(3.5, 4.5), c("bounded", "bounded"), c("bounded", "bounded"))
  )
)
lsc_collected <- logvar_set_boot_collect(list(lsc_good, "solver exploded"), lsc_spec)
check(
  "collect stacks draws into B x p matrices with the spec's coefficient names",
  identical(dim(lsc_collected$ppml[[2]]$lower), c(2L, 2L)) &&
    identical(colnames(lsc_collected$ppml[[2]]$lower), c("a", "b"))
)
check(
  "collect preserves per-estimator per-tau values on the good draw",
  identical(unname(lsc_collected$harvey[[3]]$upper[1, ]), c(3.5, 4.5))
)
check(
  "an errored (character) draw becomes an all-failed row, never dropped",
  nrow(lsc_collected$ppml[[2]]$lower) == 2L &&
    all(is.na(lsc_collected$ppml[[2]]$lower[2, ])) &&
    all(lsc_collected$ppml[[2]]$lower_status[2, ] == "failed") &&
    all(lsc_collected$harvey[[3]]$upper_status[2, ] == "failed")
)
check(
  "collect adds the point fields to the tau=0 slot and to no other slot",
  identical(names(lsc_collected$ppml[[1]]), c(
    "lower", "upper", "lower_status", "upper_status", "point", "point_status"
  )) &&
    identical(
      names(lsc_collected$ppml[[3]]),
      c("lower", "upper", "lower_status", "upper_status")
    )
)
lsc_t0 <- lsc_collected$ppml[[1]]
check(
  "the collected tau=0 mirrors are bitwise identical to the point",
  identical(lsc_t0$lower, lsc_t0$point) &&
    identical(lsc_t0$upper, lsc_t0$point) &&
    identical(lsc_t0$lower_status, lsc_t0$point_status) &&
    identical(lsc_t0$upper_status, lsc_t0$point_status)
)
check(
  "an errored draw's tau=0 slot is a failed point with no value and no unbounded",
  identical(dim(lsc_t0$point), c(2L, 2L)) &&
    identical(colnames(lsc_t0$point), lsc_spec$coefs) &&
    identical(unname(lsc_t0$point[1, ]), c(-1, 2)) &&
    all(lsc_t0$point_status[2, ] == "failed") &&
    all(is.na(lsc_t0$point[2, ])) &&
    !any(lsc_t0$point_status == "unbounded")
)
