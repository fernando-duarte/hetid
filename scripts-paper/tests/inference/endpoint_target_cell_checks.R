# The published-cell half of the endpoint-target checks: the cell shapes and the
# one gate policy in support/identification/endpoint_target_cells.R, plus the
# tau = 0 point statistic in endpoint_point_statistic.R. Sourced by
# endpoint_targets_checks.R, which owns the prologue, the seed and the shared
# et_ helpers, and which holds the target-algebra half.

# Test: half-infinite shapes
et_lower_vals <- stats::rnorm(et_b, -2, 0.1)
et_upper_vals <- stats::rnorm(et_b, -1, 0.1)
et_bounded <- rep(PAPER_ENDPOINT_STATUS[["bounded"]], et_b)
et_unbounded <- rep(PAPER_ENDPOINT_STATUS[["unbounded"]], et_b)
et_full <- function(lower_status, upper_status, set_lower, set_upper) {
  data.frame(
    coef = "a", set_lower = set_lower, set_upper = set_upper,
    lower_status = lower_status, upper_status = upper_status,
    stringsAsFactors = FALSE
  )
}
et_row <- function(lower, upper, lower_status, upper_status, f) {
  endpoint_target_row(
    lower, upper, lower_status, upper_status, f, et_alpha, et_min_reps,
    et_stability, et_tol
  )
}
et_live_up <- et_row(
  rep(-Inf, et_b), et_upper_vals, et_unbounded, et_bounded,
  et_full("unbounded", "bounded", -Inf, -1)
)
et_live_lo <- et_row(
  et_lower_vals, rep(Inf, et_b), et_bounded, et_unbounded,
  et_full("bounded", "unbounded", -2, Inf)
)
stopifnot(
  identical(et_live_up$side, "upper"), et_live_up$ci_lower == -Inf,
  is.finite(et_live_up$ci_upper), et_live_up$ci_upper > -1,
  et_live_up$c_p_lower == et_live_up$c_s,
  et_live_up$c_p_upper == et_live_up$c_s,
  identical(et_live_up$reason, "reported"), et_live_up$n_common == 100,
  # the dead side fails its own gate on an infinite anchor and must not blank
  # the cell, and the live side needs no search over the truth position
  !et_live_up$gate_lower, et_live_up$c_p_evals == 0,
  identical(et_live_lo$side, "lower"), et_live_lo$ci_upper == Inf,
  is.finite(et_live_lo$ci_lower), et_live_lo$ci_lower < -2,
  et_live_lo$c_p_lower == et_live_lo$c_s,
  et_live_lo$c_p_upper == et_live_lo$c_s,
  identical(et_live_lo$reason, "reported"), !et_live_lo$gate_upper
)
et_pass("a cell with one live side reports that side instead of blanking")

# Test: gate arithmetic over all four statuses
et_side <- function(n_bounded, other, n_other, value = NULL) {
  vals <- if (is.null(value)) {
    stats::rnorm(n_bounded, -1, 0.1)
  } else {
    rep(value, n_bounded)
  }
  list(
    vals = c(vals, rep(NA_real_, n_other)),
    status = c(
      rep(PAPER_ENDPOINT_STATUS[["bounded"]], n_bounded),
      rep(PAPER_ENDPOINT_STATUS[[other]], n_other)
    )
  )
}
et_gated <- function(side) {
  et_row(
    et_lower_vals, side$vals, et_bounded, side$status,
    et_full("bounded", "bounded", -2, -1)
  )
}
et_drop <- et_gated(et_side(60L, "failed", 40L))
et_thin <- et_gated(et_side(40L, "failed", 60L))
et_unb <- et_gated(et_side(80L, "unbounded", 20L))
et_unr <- et_gated(et_side(80L, "unreliable", 20L))
et_deg <- et_gated(et_side(100L, "failed", 0L, value = -1))
stopifnot(
  # failed draws leave the stability denominator, so 60 of 60 clears the share
  isTRUE(et_drop$gate_upper), identical(et_drop$side, "two-sided"),
  et_drop$n_non_failed_upper == 60, et_drop$frac_upper == 1,
  et_drop$n_common == 60,
  # yet they still count against the absolute threshold on all 100 draws
  identical(et_thin$side, "none"),
  identical(et_thin$reason, "insufficient bounded draws"),
  et_thin$n_non_failed_upper == 40, et_thin$frac_upper == 1,
  # unbounded and unreliable draws stay in that denominator
  identical(et_unb$reason, "boundedness unstable across draws"),
  et_unb$n_non_failed_upper == 100, et_unb$frac_upper == 0.8,
  identical(et_unr$reason, "boundedness unstable across draws"),
  et_unr$n_non_failed_upper == 100, et_unr$frac_upper == 0.8,
  identical(et_deg$reason, "degenerate endpoint scale"), et_deg$se_upper == 0
)
et_pass("gate counts, denominators and blank reasons over all four statuses")

# Test: the side scale uses each side's own pool
et_own_lower <- et_lower_vals
et_own_lower[1:10] <- -2.9
et_own_upper <- et_upper_vals
et_own_upper[1:10] <- NA_real_
et_own_status <- et_bounded
et_own_status[1:10] <- PAPER_ENDPOINT_STATUS[["unbounded"]]
et_own <- et_row(
  et_own_lower, et_own_upper, et_bounded, et_own_status,
  et_full("bounded", "bounded", -2, -1)
)
et_own_pool <- is.finite(et_own_upper)
et_own_z_l <- (et_own_lower - (-2)) / et_own$se_lower
et_own_z_u <- ((-1) - et_own_upper) / et_own$se_upper
stopifnot(
  identical(et_own$side, "two-sided"),
  isTRUE(all.equal(et_own$se_lower, robust_scale(et_own_lower))),
  !isTRUE(all.equal(et_own$se_lower, robust_scale(et_own_lower[-(1:10)]))),
  et_own$n_lower == 100, et_own$n_upper == 90, et_own$n_common == 90,
  isTRUE(all.equal(et_own$c_s, root_critical(
    et_root_s(et_own_z_l, et_own_z_u)[et_own_pool], et_alpha
  )))
)
et_pass("each side's scale keeps draws the two-sided root pool has to drop")

# Test: point_t_statistic
et_pt_vals <- stats::rnorm(70L, 0.5, 0.2)
et_pt_status <- matrix(
  c(
    rep(PAPER_ENDPOINT_STATUS[["bounded"]], 70L),
    rep(PAPER_ENDPOINT_STATUS[["unreliable"]], 5L),
    rep(PAPER_ENDPOINT_STATUS[["failed"]], 25L)
  ),
  ncol = 1L
)
et_pt <- point_t_statistic(
  0.4, matrix(
    c(et_pt_vals, rep(NA_real_, 30L)),
    ncol = 1L,
    dimnames = list(NULL, "th1")
  ),
  et_pt_status, et_min_reps, et_stability
)
et_pt_reason <- function(point_hat, n_bounded, other, n_other, value = NULL) {
  side <- et_side(n_bounded, other, n_other, value)
  point_t_statistic(
    point_hat,
    matrix(side$vals, ncol = 1L, dimnames = list(NULL, "th1")),
    matrix(side$status, ncol = 1L), et_min_reps, et_stability
  )$reason
}
stopifnot(
  identical(et_pt$reason, "reported"), identical(et_pt$coef, "th1"),
  isTRUE(all.equal(et_pt$se, stats::mad(et_pt_vals))),
  isTRUE(all.equal(et_pt$statistic, 0.4 / stats::mad(et_pt_vals))),
  et_pt$n_bounded + et_pt$n_unbounded + et_pt$n_unreliable +
    et_pt$n_failed == 100,
  et_pt$n_non_failed == 75, et_pt$n_valid_point == 70,
  isTRUE(all.equal(et_pt$frac_bounded, 70 / 75)),
  identical(
    et_pt_reason(NA_real_, 70L, "failed", 30L),
    "full-sample point not available"
  ),
  identical(
    et_pt_reason(0.4, 40L, "failed", 60L), "insufficient bounded draws"
  ),
  identical(
    et_pt_reason(0.4, 60L, "unreliable", 40L),
    "boundedness unstable across draws"
  ),
  identical(
    et_pt_reason(0.4, 100L, "failed", 0L, value = -1), "degenerate point scale"
  ),
  # a point evaluation cannot diverge, so an unbounded draw is an error and not
  # a data condition: it must raise rather than reach any gate
  grepl("unbounded", conditionMessage(tryCatch(
    et_pt_reason(0.4, 60L, "unbounded", 40L),
    error = identity
  )), fixed = TRUE)
)
et_pass("point statistic, its status arithmetic and every blank reason")
