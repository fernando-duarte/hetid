#' Ordered Warm Profile Path over Mean-Set Slacks
#'
#' Solves the sorted union of requested and through slacks, then returns only
#' requested results in their original order. Each stage passes its checked
#' multistart points to the next stage. Tau keys retain 17 significant digits.
#'
#' @param fit A hetid_tau0_fit carrying a finite unique tau-zero point. The current
#'   gamma/moments system is solved again using attr(fit, "tol"). The stored point
#'   must be within tol * max(1, max(abs(current_point))) in sup norm. Stale anchors
#'   and systems without a unique consistent current point are argument errors.
#' @param taus Nonempty vector of distinct finite slacks in (0, 1).
#' @param through Additional finite slacks in (0, 1), or an empty vector.
#' @param control List containing the QUADRATIC_PROFILE_CONTROL settings.
#' @return List keyed by sprintf("%.17g", taus). Each entry has the coefficient
#'   tables and profile_points attribute described by profile_quadratic_coefficients(),
#'   plus its quadratic system. The original tau-zero point is offered as an
#'   anchor at every stage; warm starts carry the preceding stage's checked points.
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
#' control <- QUADRATIC_PROFILE_CONTROL
#' control$SOLVER_BOXES <- c(10, 100, 1000)
#' control$MULTISTART_ROUNDS <- 1L
#' path <- profile_mean_tau_path(fit, taus = 0.05, through = 0.025, control = control)
#' path[[1]]$theta
#' path[[1]]$beta1
profile_mean_tau_path <- function(fit, taus, through = numeric(),
                                  control = QUADRATIC_PROFILE_CONTROL) {
  validate_profile_fit(fit)
  validate_profile_control(control)
  validate_profile_taus(taus, "taus")
  validate_profile_taus(through, "through", allow_empty = TRUE)
  assert_bad_argument_ok(!anyDuplicated(taus), "taus must be distinct", arg = "taus")
  validate_profile_betas(fit$beta1r, fit$beta2r, ncol(fit$w2))
  with_rng_scope(
    {
      dimension <- ncol(fit$w2)
      warm <- list(fit$point$theta)
      anchors <- matrix(fit$point$theta, nrow = 1L)
      tables <- list()
      for (tau in sort(unique(c(through, taus)))) {
        quadratic <- build_quadratic_system(
          fit$gamma, rep(tau, dimension),
          fit$moments
        )$quadratic
        refined <- profile_tables_widened(
          quadratic, fit$beta1r, fit$beta2r,
          anchors, warm, control
        )
        warm <- attr(refined, "profile_points")
        refined$quadratic <- quadratic
        tables[[profile_tau_key(tau)]] <- refined
      }
      tables[profile_tau_key(taus)]
    },
    kind = c("Mersenne-Twister", "Inversion", "Rejection")
  )
}

profile_tau_key <- function(tau) sprintf("%.17g", tau)
