#' Bracket a Mean-Set Boundedness Transition
#'
#' A coarse sweep and bisection retain the largest verified bounded tau.
#' An unresolved midpoint stops refinement. Widths and solver termination do
#' not establish boundedness. No claim is made beyond the inspected evidence.
#'
#' @param fit A hetid_tau0_fit carrying a finite unique tau-zero point. The current
#'   gamma/moments system is solved again using attr(fit, "tol"). The stored point
#'   must be within tol * max(1, max(abs(current_point))) in sup norm. Stale anchors
#'   and systems without a unique consistent current point are argument errors.
#' @param control List containing the MEAN_TAU_CONTROL settings.
#' @return A list with tau_star, sweep and bracket. Tau_star equals the bracket's
#'   lower, bounded endpoint. The bracket also carries upper (NA when absent),
#'   status, inconclusive taus, and sweep_max. Status is capped, unresolved_above,
#'   unresolved_below, unresolved_band or bracketed. The sweep records each tau,
#'   bounded/unbounded/unreliable status and coarse/bisection origin. The coarse
#'   progression is seq(0, CAP, by = SWEEP_STEP); a nondivisible CAP is not added.
#'   Capped means its last inspected progression value is bounded.
#' @export

#' @examples
#' t <- seq_len(40)
#' x <- matrix(sin(t), ncol = 1, dimnames = list(NULL, "x"))
#' z <- seq(-1, 1, length.out = length(t))
#' y2 <- matrix(x[, 1] + (1 + z) * cos(2 * t),
#'   ncol = 1,
#'   dimnames = list(NULL, "news")
#' )
#' y1 <- 0.5 + 0.2 * x[, 1] + 0.7 * y2[, 1] + sin(3 * t)
#' fit <- compute_tau0_system(y1, y2, x, z)
#' control <- MEAN_TAU_CONTROL
#' control$CAP <- 0.1
#' control$SWEEP_STEP <- 0.05
#' control$BISECTION_ITERATIONS <- 2L
#' transition <- find_mean_tau_star(fit, control = control)
#' transition$bracket
#' transition$sweep
find_mean_tau_star <- function(fit, control = MEAN_TAU_CONTROL) {
  validate_profile_fit(fit)
  validate_mean_tau_control(control)
  with_rng_scope(
    {
      taus <- seq(0, control$CAP, by = control$SWEEP_STEP)
      # the tau = 0 set is the point itself
      status <- c("bounded", vapply(taus[-1L], profile_mean_tau_status, character(1),
        fit = fit
      ))
      sweep_table <- data.frame(tau = taus, status = status, grid = "coarse")
      bounded <- taus[status == "bounded"]
      unbounded <- taus[status == "unbounded"]
      unknown <- taus[status == "unreliable"]
      lo <- if (length(bounded)) max(bounded) else 0
      hi <- if (length(unbounded)) min(unbounded) else NA_real_
      if (!is.na(hi)) {
        assert_bad_argument_ok(lo < hi, "Inconsistent boundedness transition evidence")
        for (iteration in seq_len(control$BISECTION_ITERATIONS)) {
          mid <- (lo + hi) / 2
          if (mid == lo || mid == hi) break
          state <- profile_mean_tau_status(fit, mid)
          sweep_table <- rbind(
            sweep_table,
            data.frame(tau = mid, status = state, grid = "bisection")
          )
          if (identical(state, "bounded")) {
            lo <- mid
          } else if (identical(state, "unbounded")) {
            hi <- mid
          } else {
            unknown <- c(unknown, mid)
            break
          }
        }
      }
      inconclusive <- if (is.na(hi)) {
        unknown[unknown > lo]
      } else {
        unknown[unknown > lo & unknown < hi]
      }
      sweep_max <- max(taus)
      state <- profile_tau_bracket_state(lo, hi, sweep_max, inconclusive)
      list(
        tau_star = lo, sweep = sweep_table,
        bracket = list(
          lower = lo, upper = hi, status = state,
          inconclusive = sort(unique(inconclusive)), sweep_max = sweep_max
        )
      )
    },
    kind = c("Mersenne-Twister", "Inversion", "Rejection")
  )
}

profile_mean_tau_status <- function(fit, tau) {
  dimension <- ncol(fit$w2)
  quadratic <- build_quadratic_system(fit$gamma, rep(tau, dimension), fit$moments)$quadratic
  evidence <- profile_evidence(
    quadratic, diag(dimension),
    matrix(fit$point$theta, nrow = 1L)
  )
  states <- c(evidence$summary$lower_state, evidence$summary$upper_state)
  if (any(states == "unbounded")) {
    "unbounded"
  } else if (all(states == "bounded")) {
    "bounded"
  } else {
    "unreliable"
  }
}

profile_tau_bracket_state <- function(lo, hi, sweep_max, inconclusive) {
  if (is.na(hi) && identical(lo, sweep_max)) {
    "capped"
  } else if (is.na(hi)) {
    "unresolved_above"
  } else if (lo == 0) {
    "unresolved_below"
  } else if (length(inconclusive)) {
    "unresolved_band"
  } else {
    "bracketed"
  }
}
