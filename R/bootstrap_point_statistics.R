#' Summarize Bootstrap Point Estimates With Robust Scales and Empirical Tails
#'
#' Reports coefficient-wise statistics and p-values when the supplied bootstrap
#' draws pass count, stability and robust-scale gates.
#'
#' @param point Nonempty numeric vector of full-sample estimates with unique,
#'   nonempty, nonmissing names; \code{NA} estimates are allowed.
#' @param draws Numeric matrix with draws in rows and columns named exactly as
#'   \code{point}, in order. Optional row names must be unique, nonempty and
#'   nonmissing.
#' @param status Character matrix with the same dimensions and dimnames as
#'   \code{draws}, containing \code{"bounded"}, \code{"unreliable"}, or
#'   \code{"failed"}, with no missing statuses.
#' @param min_reps Positive integer scalar giving the minimum bounded draw count.
#'   Must not exceed \code{.Machine$integer.max}.
#' @param stability Numeric scalar in \eqn{[0, 1]} giving the minimum bounded
#'   share among non-failed draws, including unreliable draws in the denominator.
#' @details Bounded draws must be finite, failed draws must be \code{NA}, and
#'   unreliable draws may be finite or \code{NA}. A point cannot be unbounded.
#'   NaN and infinite values are rejected. Only bounded draws enter the scale
#'   and empirical tails. Scales use \code{stats::mad()} with its default
#'   normal-consistency factor and are in the same units as \code{point}.
#'   Reporting requires a finite full-sample point, at least \code{min_reps}
#'   bounded draws, a bounded share of at least \code{stability}, and a positive
#'   finite scale. The scale is \code{NA} with fewer than two bounded draws.
#'   The reported statistic is \code{point / se}; its normal-tail p-value
#'   is separate from the empirical absolute-deviation p-value.
#'
#'   With eligible deviations \code{d = draws - point} and \code{B} bounded
#'   draws, the empirical two-sided p-value is
#'   \code{(1 + sum(abs(d) >= abs(point))) / (B + 1)}. Directional tails use
#'   \code{d <= -abs(point)} and \code{d >= abs(point)} with the same correction.
#'   Ties are included; the minimum is \code{1/(B+1)}. The robust scale cancels from
#'   these comparisons, so these empirical p-values are not studentized.
#'   Gates do not establish bootstrap validity or coverage; see
#'   \code{\link{bootstrap_set_interval}()}. Overflow in a reported statistic or
#'   deviation raises a structured \code{hetid_error}; a nonfinite scale instead
#'   fails the reporting gate.
#' @return A data frame with one row per element of \code{point}, in input order:
#'   \describe{
#'     \item{\code{coef}, \code{point}, \code{se}}{Coefficient name, full-sample
#'       estimate and robust scale. The scale is retained even if reporting fails.}
#'     \item{\code{statistic}, \code{p_value}, \code{p_value_normal}}{The estimate
#'       divided by its scale, empirical two-sided p-value and two-sided standard
#'       normal-tail p-value for a zero null.}
#'     \item{\code{p_lower}, \code{p_upper}}{Empirical directional tail p-values
#'       defined in Details.}
#'     \item{\code{n_bounded}, \code{n_unbounded}, \code{n_unreliable},
#'       \code{n_failed}}{Counts of each supplied status; \code{n_unbounded} is zero.}
#'     \item{\code{n_valid_point}, \code{n_non_failed}, \code{frac_bounded}}{Bounded
#'       draw count, non-failed draw count and their ratio (zero if all draws fail
#'       or there are no draws).}
#'     \item{\code{min_reps}, \code{reason}}{Requested count threshold and reporting
#'       outcome: \code{"reported"}, \code{"full-sample point not available"},
#'       \code{"insufficient bounded draws"},
#'       \code{"boundedness unstable across draws"}, or \code{"degenerate point scale"}.}
#'   }
#'   When a reporting gate fails, \code{statistic} and all p-values are \code{NA};
#'   the estimate, scale and status counts remain available.
#' @export
#' @examples
#' draws <- matrix(-1:4, 6, 1, dimnames = list(NULL, "a"))
#' status <- matrix("bounded", 6, 1, dimnames = dimnames(draws))
#' bootstrap_point_statistics(c(a = 2), draws, status, 3, 0.8)
bootstrap_point_statistics <- function(point, draws, status, min_reps, stability) {
  assert_bad_argument_ok(
    is.numeric(point) && is.null(dim(point)) && length(point) > 0L &&
      !any(is.infinite(point) | is.nan(point)), "point must be finite or NA",
    arg = "point"
  )
  assert_instrument_names(names(point), "point")
  validate_bootstrap_matrix(draws, names(point), is.numeric)
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
