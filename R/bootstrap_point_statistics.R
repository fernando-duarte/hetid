#' Summarize Bootstrap Point Estimates With Robust Scales and Empirical Tails
#'
#' @param point Named numeric vector of full-sample estimates; `NA` is allowed.
#' @param draws Numeric matrix with draws in rows and columns named exactly as
#'   `point`, in order.
#' @param status Character matrix with the same dimensions and dimnames as
#'   `draws`, containing `bounded`, `unreliable`, or `failed`.
#' @param min_reps Minimum bounded draw count, a positive integer.
#' @param stability Minimum bounded share among non-failed draws, in `[0, 1]`.
#' @details Bounded draws must be finite, failed draws must be `NA`, and
#'   unreliable draws may be finite or `NA`. A point cannot be unbounded.
#'   NaN and infinite values are rejected. Scales use `stats::mad()` over bounded
#'   draws. The reported statistic is `point / scale`; its normal-tail p-value
#'   is separate from the empirical absolute-deviation p-value.
#'
#'   With eligible deviations `d = draws - point`, the empirical two-sided
#'   p-value is `(1 + sum(abs(d) >= abs(point))) / (B + 1)`. Directional tails
#'   use `d <= -abs(point)` and `d >= abs(point)` with the same correction.
#'   Ties are included; the minimum is `1/(B+1)`. The robust scale cancels from
#'   these comparisons, so these empirical p-values are not studentized.
#'   Gates do not establish bootstrap validity or coverage; see
#'   [bootstrap_set_interval()]. Numerical overflow raises a structured error.
#' @return A data frame of estimates, scales, statistics, empirical and normal
#'   p-values, status counts, eligibility counts and reasons for unavailable rows.
#' @export
#' @examples
#' draws <- matrix(-1:4, 6, 1, dimnames = list(NULL, "a"))
#' status <- matrix("bounded", 6, 1, dimnames = dimnames(draws))
#' bootstrap_point_statistics(c(a = 2), draws, status, 3, 0.8)
bootstrap_point_statistics <- function(point, draws, status, min_reps, stability) {
  assert_bad_argument_ok(
    bootstrap_is_numeric(point) && is.null(dim(point)) && length(point) > 0L &&
      !any(is.infinite(point) | is.nan(point)), "point must be finite or NA",
    arg = "point"
  )
  assert_instrument_names(names(point), "point")
  validate_bootstrap_matrix(draws, names(point), bootstrap_is_numeric)
  validate_bootstrap_matrix(status, names(point), is.character, dim(draws), rownames(draws))
  validate_bootstrap_side(draws, status, "lower")
  assert_bad_argument_ok(
    !any(status == "unbounded") && !any(is.infinite(draws)),
    "point draws cannot be unbounded or infinite"
  )
  validate_bootstrap_gate(min_reps, stability)
  rows <- lapply(seq_along(point), function(k) {
    s <- status[, k]
    side <- bootstrap_endpoint_gate(draws[, k], s, point[k], min_reps, stability)
    reason <- if (!is.finite(point[k])) "full-sample point not available" else side$reason
    if (identical(reason, "degenerate endpoint scale")) reason <- "degenerate point scale"
    if (side$gate) reason <- "reported"
    statistic <- if (side$gate) {
      bootstrap_finite_arithmetic(point[[k]] / side$se, "point statistic")
    } else {
      NA_real_
    }
    boot <- bootstrap_point_tails(draws[side$ok, k], point[[k]], side$gate)
    data.frame(
      coef = names(point)[k], point = point[[k]], se = side$se,
      statistic = statistic, p_value = boot$p_value,
      p_value_normal = 2 * stats::pnorm(-abs(statistic)),
      p_lower = boot$p_lower, p_upper = boot$p_upper,
      as.list(stats::setNames(vapply(
        BOOTSTRAP_ENDPOINT_STATUS,
        function(value) sum(s == value), integer(1)
      ), paste0("n_", BOOTSTRAP_ENDPOINT_STATUS))),
      n_valid_point = side$n_ok, n_non_failed = side$n_valid, frac_bounded = side$frac,
      min_reps = min_reps, reason = reason, row.names = NULL, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

bootstrap_point_tails <- function(draws, point, gate) {
  if (!gate) {
    return(list(p_value = NA_real_, p_lower = NA_real_, p_upper = NA_real_))
  }
  deviation <- bootstrap_finite_arithmetic(draws - point, "point deviations")
  observed <- abs(point)
  n <- length(deviation)
  list(
    p_value = (1 + sum(abs(deviation) >= observed)) / (n + 1),
    p_lower = (1 + sum(deviation <= -observed)) / (n + 1),
    p_upper = (1 + sum(deviation >= observed)) / (n + 1)
  )
}
