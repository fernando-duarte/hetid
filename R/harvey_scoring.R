#' Harvey Scoring and Acceptance
#'
#' Iteration helpers for the Harvey log-variance solver: the backtracking
#' line search, observed-Newton direction with Fisher fallback, scoring loop,
#' and post-stop acceptance check. Evaluation helpers are documented in
#' \code{\link{harvey_solver}}.
#' Validated fitting controls are passed through the solve; defaults come
#' from \code{\link{LOG_VARIANCE_HARVEY_CONTROL}}.
#'
#' These helpers assume finite, nonnegative responses and a finite design from
#' \code{\link{fit_log_variance}}; scoring also requires full column rank.
#' Missing values are not removed. The fitted variance \eqn{\exp(X\theta)} is
#' on the solver's response scale. Malformed direct calls are not validated here.
#'
#' @name harvey_scoring_module
#' @keywords internal
NULL

#' Backtrack Along a Proposed Direction
#'
#' Accepts any strict criterion decrease. An equal or higher criterion is
#' accepted only within the summation-rounding band and with a scaled-score
#' improvement exceeding the control's progress margin.
#'
#' Starts at the full step and halves it up to \code{LINE_SEARCH_HALVINGS}
#' times. Unusable trial evaluations are rejected. The rounding band uses
#' \code{Q_NOISE_MULTIPLIER}; the score margin uses \code{SCORE_PROGRESS_MULTIPLIER}.
#'
#' @param cur Current list returned by \code{\link{harvey_eval}}.
#' @param control Validated named list of Harvey fitting controls; defaults
#'   derive from \code{\link{LOG_VARIANCE_HARVEY_CONTROL}}.
#' @param dir Numeric direction vector of length \code{ncol(x_mat)}.
#' @inheritParams harvey_eval
#'
#' @return \code{NULL} when no halving is accepted (a stall), otherwise a list
#'   with the accepted \code{eval} and the number of \code{halves} taken.
#' @keywords internal
harvey_line_search <- function(cur, dir, y, x_mat, pos, col_abs,
                               control = log_variance_fit_control("harvey")) {
  ctrl <- control
  step_size <- 1
  q_noise <- ctrl$Q_NOISE_MULTIPLIER * .Machine$double.eps *
    (1 + sum(abs(cur$eta)) + sum(cur$r[pos]))
  margin <- ctrl$SCORE_PROGRESS_MULTIPLIER * .Machine$double.eps *
    max(1, cur$score_norm)
  for (halves in 0:ctrl$LINE_SEARCH_HALVINGS) {
    trial <- harvey_eval(cur$theta + step_size * dir, y, x_mat, pos, col_abs)
    if (!is.null(trial)) {
      tie <- abs(trial$q - cur$q) <= q_noise
      if (trial$q < cur$q ||
        (tie && cur$score_norm - trial$score_norm > margin)) {
        return(list(eval = trial, halves = halves))
      }
    }
    step_size <- step_size / 2
  }
  NULL
}

#' Observed-Newton Direction
#'
#' \eqn{(X' diag(r) X)^{-1} X'(r - 1)} when the observed information is well
#' conditioned, else \code{NULL} so the caller falls back to the
#' constant-information Fisher direction. The expected information
#' \eqn{0.5 X'X} is positive definite for the validated full-rank design.
#' Non-finite entries, nonpositive diagonal entries, a normalized reciprocal
#' condition number below \code{NEWTON_RCOND_TOLERANCE}, or a failed Cholesky
#' factorization reject the observed-Newton direction.
#'
#' @param cur Current \code{\link{harvey_eval}} result.
#' @param x_mat Numeric design matrix, intercept column included.
#' @param control Validated named list of Harvey fitting controls; defaults
#'   derive from \code{\link{LOG_VARIANCE_HARVEY_CONTROL}}.
#'
#' @return Numeric vector of length \code{ncol(x_mat)}, or \code{NULL} on rejection.
#' @keywords internal
harvey_newton_dir <- function(cur, x_mat,
                              control = log_variance_fit_control("harvey")) {
  obs <- crossprod(x_mat, cur$r * x_mat)
  d <- diag(obs)
  # Check the diagonal before sqrt() to avoid warnings from negative entries
  if (!all(is.finite(obs)) || any(!is.finite(d)) || any(d <= 0)) {
    return(NULL)
  }
  normalized <- obs / tcrossprod(sqrt(d))
  if (rcond(normalized) < control$NEWTON_RCOND_TOLERANCE) {
    return(NULL)
  }
  obs_chol <- tryCatch(chol(obs), error = function(cond) NULL)
  if (is.null(obs_chol)) {
    return(NULL)
  }
  harvey_chol_solve(obs_chol, cur$moment)
}

#' Run the Scoring Loop From One Evaluated Start
#'
#' The initial-start shortcut exits converged with \code{iters = 0} when the
#' scaled score already passes; otherwise each iteration prefers the
#' observed-Newton direction and falls back to the Fisher direction when the
#' observed information is ill conditioned or its line search stalls.
#' After a step, convergence needs a score at or below \code{SCORE_TOLERANCE}
#' \emph{and} a criterion or parameter change within \code{REL_CHANGE_TOLERANCE}
#' times the corresponding scale (at least one). \code{MAXIT} caps iterations.
#'
#' @param cur Evaluated-start list from \code{\link{harvey_eval}}.
#' @inheritParams harvey_eval
#' @inheritParams harvey_line_search
#' @param chol_xx Numeric upper triangular Cholesky factor of \code{crossprod(x_mat)}.
#'
#' @return List with the last \code{eval}, the \code{iters} taken (negative on
#'   a stall, marking the iteration it stalled at), the cumulative
#'   \code{halves} from accepted steps, and a \code{status} of \code{"converged"},
#'   \code{"line_search_stall"}, or \code{"iteration_cap"}.
#' @keywords internal
harvey_scoring <- function(cur, y, x_mat, pos, col_abs, chol_xx,
                           control = log_variance_fit_control("harvey")) {
  ctrl <- control
  if (cur$score_norm <= ctrl$SCORE_TOLERANCE) {
    return(list(eval = cur, iters = 0L, halves = 0L, status = "converged"))
  }
  total_halves <- 0L
  for (it in seq_len(ctrl$MAXIT)) {
    dir_newton <- harvey_newton_dir(cur, x_mat, control)
    taken <- if (is.null(dir_newton)) {
      NULL
    } else {
      harvey_line_search(cur, dir_newton, y, x_mat, pos, col_abs, control)
    }
    if (is.null(taken)) {
      dir_fisher <- harvey_chol_solve(chol_xx, cur$moment)
      taken <- harvey_line_search(cur, dir_fisher, y, x_mat, pos, col_abs, control)
    }
    if (is.null(taken)) {
      return(list(
        eval = cur, iters = -it, halves = total_halves,
        status = "line_search_stall"
      ))
    }
    total_halves <- total_halves + taken$halves
    moved <- taken$eval
    rel_q <- abs(moved$q - cur$q) <=
      ctrl$REL_CHANGE_TOLERANCE * max(1, abs(moved$q))
    rel_theta <- max(abs(moved$theta - cur$theta)) <=
      ctrl$REL_CHANGE_TOLERANCE * max(1, max(abs(moved$theta)))
    if (moved$score_norm <= ctrl$SCORE_TOLERANCE && (rel_q || rel_theta)) {
      return(list(
        eval = moved, iters = it, halves = total_halves, status = "converged"
      ))
    }
    cur <- moved
  }
  list(
    eval = cur, iters = ctrl$MAXIT, halves = total_halves,
    status = "iteration_cap"
  )
}

#' Fresh Post-Stop Acceptance Gate
#'
#' Recomputes the safe ratio and criterion from scratch at the stopped point,
#' then requires a finite strictly positive fitted variance and a
#' diagonally-normalized information \code{rcond} at or above \code{RCOND_TOLERANCE}.
#' It does not recheck score convergence, which the scoring loop establishes.
#' Normalizing by the diagonal makes the gate scale-invariant while catching
#' rank deficiency. \code{NULL} rejects the point.
#'
#' @inheritParams harvey_eval
#' @inheritParams harvey_line_search
#'
#' @return \code{NULL} on rejection, otherwise a list with the recomputed
#'   \code{eval}, the numeric \code{ncol(x_mat)} square observed-information matrix
#'   \code{info}, and its scalar normalized reciprocal condition number \code{rcond}.
#' @keywords internal
harvey_post_stop <- function(theta, y, x_mat, pos, col_abs,
                             control = log_variance_fit_control("harvey")) {
  ev <- harvey_eval(theta, y, x_mat, pos, col_abs)
  if (is.null(ev)) {
    return(NULL)
  }
  mu <- exp(ev$eta)
  if (!all(is.finite(mu)) || any(mu <= 0)) {
    return(NULL)
  }
  info <- harvey_info(theta, y, x_mat)
  d <- diag(info)
  if (any(!is.finite(d)) || any(d <= 0)) {
    return(NULL)
  }
  rc <- rcond(info / tcrossprod(sqrt(d)))
  if (!is.finite(rc) || rc < control$RCOND_TOLERANCE) {
    return(NULL)
  }
  list(eval = ev, info = info, rcond = rc)
}
