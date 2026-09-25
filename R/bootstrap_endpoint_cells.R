bootstrap_endpoint_cell <- function(lc, uc, f, alpha, control, min_reps, target) {
  bounded <- "bounded"
  unbounded <- "unbounded"
  blank <- list(
    side = "none", c_s = NA_real_, c_p_lower = NA_real_, c_p_upper = NA_real_,
    evals = NA_integer_, best_lambda = NA_real_, interior = NA,
    n_common = 0L, ci_lower = NA_real_, ci_upper = NA_real_,
    search_stop = "not_run"
  )
  if (all(c(f$lower_status, f$upper_status) == bounded)) {
    return(bootstrap_two_sided_cell(lc, uc, f, alpha, control, min_reps, target, blank))
  }
  # One live side: the truth can sit anywhere on the infinite ray, so the worst
  # position is at the finite endpoint, the credit vanishes and both targets
  # coincide at the live side's own quantile. No lambda optimization is needed,
  # and the dead side's draw status must not exclude a bounded live-side draw.
  if (identical(f$lower_status, unbounded) && identical(f$upper_status, bounded)) {
    if (!uc$gate) {
      return(c(blank, list(reason = uc$reason)))
    }
    c_s <- bootstrap_containment_critical(uc$ok, alpha, uc$z)
    return(list(
      side = "upper", c_s = c_s, c_p_lower = c_s, c_p_upper = c_s,
      evals = 0L, best_lambda = NA_real_, interior = FALSE,
      n_common = uc$n_ok, ci_lower = -Inf,
      ci_upper = bootstrap_finite_arithmetic(f$upper + c_s * uc$se, "upper padding"),
      search_stop = "half_infinite", reason = "reported"
    ))
  }
  if (identical(f$lower_status, bounded) && identical(f$upper_status, unbounded)) {
    if (!lc$gate) {
      return(c(blank, list(reason = lc$reason)))
    }
    c_s <- bootstrap_containment_critical(lc$ok, alpha, lc$z)
    return(list(
      side = "lower", c_s = c_s, c_p_lower = c_s, c_p_upper = c_s,
      evals = 0L, best_lambda = NA_real_, interior = FALSE,
      n_common = lc$n_ok,
      ci_lower = bootstrap_finite_arithmetic(f$lower - c_s * lc$se, "lower padding"),
      ci_upper = Inf, search_stop = "half_infinite", reason = "reported"
    ))
  }
  if (all(c(f$lower_status, f$upper_status) == unbounded)) {
    return(c(blank, list(reason = "full-sample set unbounded on both sides")))
  }
  c(blank, list(reason = "full-sample side unavailable or unreliable"))
}

bootstrap_two_sided_cell <- function(lc, uc, f, alpha, control, min_reps, target, blank) {
  if (!(lc$gate && uc$gate)) {
    return(c(blank, list(reason = if (!lc$gate) lc$reason else uc$reason)))
  }
  # both-bounded pool: a two-sided root needs z on both sides in one draw, so a
  # draw bounded on one side only feeds that side's scale but not this pool.
  pool <- lc$ok & uc$ok
  # the absolute count applies to the pair of sides, not to each side alone.
  # Two side gates can both clear while their intersection does not: a cell with
  # 5,100 bounded lower draws and 8,500 bounded upper draws can have only 3,600
  # jointly bounded, and the quantile below runs on the intersection.
  if (sum(pool) < min_reps) {
    return(c(blank, list(reason = "insufficient bounded draws")))
  }
  c_s <- bootstrap_containment_critical(pool, alpha, lc$z, uc$z)
  p <- list(
    c_p_lower = NA_real_, c_p_upper = NA_real_, evals = 0L,
    best_lambda = NA_real_, interior = NA, search_stop = "not_requested"
  )
  critical <- c_s
  if (target == "pointwise") {
    width <- bootstrap_finite_arithmetic(f$upper - f$lower, "interval width")
    p <- bootstrap_pointwise_critical(
      lc$z, uc$z, pool, width / lc$se, width / uc$se,
      alpha, control$tolerance, c_s, control$max_evals
    )
    critical <- p$c_p_upper
  }
  list(
    side = "two-sided", c_s = c_s, c_p_lower = p$c_p_lower,
    c_p_upper = p$c_p_upper, evals = p$evals, best_lambda = p$best_lambda,
    interior = p$interior, n_common = sum(pool),
    ci_lower = bootstrap_finite_arithmetic(f$lower - critical * lc$se, "lower padding"),
    ci_upper = bootstrap_finite_arithmetic(f$upper + critical * uc$se, "upper padding"),
    search_stop = p$search_stop, reason = "reported"
  )
}
