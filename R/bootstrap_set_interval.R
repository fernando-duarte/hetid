#' Calibrate Intervals From Paired Bootstrap Endpoints
#'
#' Uses the joint distribution of supplied endpoints, with separate robust scales
#' for each side. All tuning choices governing draw eligibility are explicit.
#' @param full Nonempty data frame with character \code{coef}, numeric
#'   \code{lower}, \code{upper}, and character \code{lower_status},
#'   \code{upper_status} columns. Coefficient names must be unique, nonempty,
#'   and nonmissing; column names must be unique. See Details for endpoint and
#'   status restrictions.
#' @param draws List containing numeric \code{lower}, \code{upper} and character
#'   \code{lower_status}, \code{upper_status} matrices of identical dimensions.
#'   Rows identify paired draws; coefficient columns must be named exactly as
#'   \code{full$coef}, in that order. Optional row names must be unique, nonempty,
#'   nonmissing, and identical across matrices. Extra evidence is retained.
#' @param target Single character string, exactly \code{"pointwise"} or
#'   \code{"containment"}; see Details.
#' @param alpha Finite numeric scalar giving the nominal tail probability,
#'   strictly between zero and one.
#' @param min_reps Minimum eligible draw count, an integer-valued numeric scalar
#'   from one through \code{.Machine$integer.max}.
#' @param stability Finite numeric scalar giving the minimum eligible share among
#'   non-failed draws, in \code{[0, 1]}.
#' @param control List of uniquely named pointwise search overrides. Supported
#'   entries are \code{TOLERANCE}, a positive finite numeric scalar, and
#'   \code{MAX_EVALS}, an integer-valued numeric scalar from two through
#'   \code{.Machine$integer.max}. Omitted entries use
#'   \code{BOOTSTRAP_INFERENCE_DEFAULTS}: \code{TOLERANCE = 1e-4} and
#'   \code{MAX_EVALS = 10000}. The default empty list uses both defaults.
#' @template bootstrap-inference
#' @return A plain list with \code{summary} (a data frame with one row per
#'   coefficient in \code{full} order), \code{sides} (a coefficient-named list of
#'   lower and upper eligibility masks, scales and roots), \code{simultaneous}
#'   (a diagnostic containment calculation), original \code{full} and
#'   \code{draws}, and \code{target}, \code{alpha}, \code{min_reps},
#'   \code{stability}, and the resolved \code{control} settings. Side roots can be
#'   nonfinite when no interval consumes them; the diagnostic reports its own
#'   availability. Pointwise intervals use the conservative upper critical value
#'   even if the search stops at its budget; containment intervals use the
#'   containment critical value.
#' @section Returned Diagnostics:
#' In \code{summary}, \code{ci_lower} and \code{ci_upper} are the padded interval
#' endpoints in the same units as the supplied endpoints. They are missing when
#' an interval is unavailable; a half-infinite interval has one infinite endpoint.
#' \code{side} identifies the live sides, and \code{reason} explains availability.
#' \code{se_lower} and \code{se_upper} are the side scales. Draw counts, shares,
#' and gates describe eligibility. \code{c_s} is the containment critical value;
#' \code{c_p_lower} and \code{c_p_upper} bound the pointwise critical value, with
#' gap \code{c_p_gap}. For two-sided containment, the pointwise values are missing.
#' \code{c_p_evals}, \code{c_p_lambda}, \code{c_p_interior}, and
#' \code{search_stop} describe the search. \code{root_rank} and
#' \code{tail_resolution} describe the interval's root pool.
#'
#' Each entry of \code{sides} contains \code{lower} and \code{upper} lists, with
#' logical draw mask \code{ok}, bounded and non-failed counts \code{n_ok} and
#' \code{n_valid}, bounded share \code{frac}, scale \code{se}, logical
#' \code{gate}, failure \code{reason}, and numeric standardized deviations
#' \code{z}. Deviations are missing outside the eligible mask or if the gate fails.
#' \code{simultaneous} contains \code{critical}, \code{reason},
#' \code{n_common}, \code{meets_min_reps}, \code{root_rank},
#' \code{tail_resolution}, and the coefficient-by-side logical matrix
#' \code{active_sides}; see Details for its scope and availability.
#' @export
#' @examples
#' full <- data.frame(
#'   coef = "a", lower = 0, upper = 1,
#'   lower_status = "bounded", upper_status = "bounded"
#' )
#' v <- seq(-0.3, 0.3, length.out = 20)
#' m <- function(x) matrix(x, 20, 1, dimnames = list(NULL, "a"))
#' draws <- list(
#'   lower = m(v), upper = m(1 + v),
#'   lower_status = m("bounded"), upper_status = m("bounded")
#' )
#' bootstrap_set_interval(full, draws, "pointwise", 0.1, 10, 0.85)$summary
bootstrap_set_interval <- function(full, draws, target, alpha, min_reps, stability,
                                   control = list()) {
  validate_bootstrap_full(full)
  validate_bootstrap_draws(draws, full$coef)
  assert_bad_argument_ok(
    is.character(target) && length(target) == 1L &&
      !is.na(target) && target %in% c("pointwise", "containment"),
    "target must be pointwise or containment",
    arg = "target"
  )
  assert_scalar_finite(alpha, "alpha")
  validate_bootstrap_gate(min_reps, stability, alpha)
  control <- bootstrap_inference_control(control)
  sides <- lapply(seq_len(nrow(full)), function(k) {
    list(
      lower = bootstrap_endpoint_side(
        draws$lower[, k], draws$lower_status[, k],
        full$lower[k], 1, min_reps, stability
      ),
      upper = bootstrap_endpoint_side(
        draws$upper[, k], draws$upper_status[, k],
        full$upper[k], -1, min_reps, stability
      )
    )
  })
  names(sides) <- full$coef
  rows <- lapply(seq_len(nrow(full)), function(k) {
    lc <- sides[[k]]$lower
    uc <- sides[[k]]$upper
    cell <- bootstrap_endpoint_cell(lc, uc, full[k, ], alpha, control, min_reps, target)
    data.frame(
      coef = full$coef[k], se_lower = lc$se, se_upper = uc$se,
      n_lower = lc$n_ok, n_upper = uc$n_ok, n_common = cell$n_common,
      n_non_failed_lower = lc$n_valid, n_non_failed_upper = uc$n_valid,
      frac_lower = lc$frac, frac_upper = uc$frac, gate_lower = lc$gate, gate_upper = uc$gate,
      side = cell$side, c_s = cell$c_s, c_p_lower = cell$c_p_lower,
      c_p_upper = cell$c_p_upper, c_p_gap = cell$c_p_upper - cell$c_p_lower,
      c_p_evals = cell$evals, c_p_lambda = cell$best_lambda, c_p_interior = cell$interior,
      ci_lower = cell$ci_lower, ci_upper = cell$ci_upper, reason = cell$reason,
      search_stop = cell$search_stop, root_rank = bootstrap_root_rank(cell$n_common, alpha),
      tail_resolution = 1 / (cell$n_common + 1), stringsAsFactors = FALSE, row.names = NULL
    )
  })
  list(
    summary = do.call(rbind, rows), sides = sides,
    simultaneous = bootstrap_simultaneous_diagnostic(sides, full, alpha, min_reps),
    full = full, draws = draws, target = target, alpha = alpha,
    min_reps = min_reps, stability = stability, control = control
  )
}
