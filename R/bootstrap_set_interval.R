#' Calibrate Intervals From Paired Bootstrap Endpoints
#'
#' Uses the joint distribution of supplied endpoints, with separate robust scales
#' for each side. All tuning choices governing draw eligibility are explicit.
#' @param full Data frame with unique character `coef`, numeric `lower`, `upper`,
#'   and character `lower_status`, `upper_status` columns.
#' @param draws List containing the four endpoint/status matrices, with draws in
#'   rows and coefficient columns named exactly as `full$coef`, in that order.
#'   Optional row names must agree across matrices. Extra evidence is retained.
#' @param target Exactly `"pointwise"` or `"containment"`; see Details.
#' @param alpha Nominal tail probability strictly between zero and one.
#' @param min_reps Minimum eligible draw count, a positive integer.
#' @param stability Minimum eligible share among non-failed draws, in `[0, 1]`.
#' @param control Named list with `tolerance` (default `1e-4`) and `max_evals`
#'   (default `10000`) for the pointwise search.
#' @template bootstrap-inference
#' @return A list with `summary` (one row per coefficient), `sides` (eligibility
#'   masks, scales and roots), `simultaneous` (diagnostic containment calculation),
#'   original `full` and `draws`, and the requested settings. Side roots can be
#'   nonfinite when no interval consumes them; the diagnostic reports its own
#'   availability. The interval uses the
#'   conservative upper critical value even if the search stops at its budget.
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
