#' Harvey Scoring and Acceptance
#'
#' The iteration half of the Harvey log-variance solve, split from
#' \code{\link{harvey_solver}} for the file-length cap: the monotone
#' backtracking line search, the observed-Newton direction with its Fisher
#' fallback, the scoring loop, and the fresh post-stop acceptance gate. Ported
#' from the paper pipeline
#' (\code{scripts-paper/log_variance/estimators/harvey/solver_acceptance.R}).
#' Controls come from \code{\link{LOG_VARIANCE_HARVEY_CONTROL}} directly, the
#' way the PPML worker reads \code{\link{LOG_VARIANCE_CONTROL}}.
#'
#' @name harvey_scoring_module
#' @keywords internal
NULL

#' Backtrack Along a Proposed Direction
#'
#' Accepts a strict criterion decrease or, on a criterion tie, only a scaled
#' score improvement past the pinned margin. A tie is any difference within the
#' criterion's own summation rounding error, not literal equality of the stored
#' doubles: near the optimum the step's true decrease sits below that noise
#' floor, so two evaluations cannot be told apart by \code{q} and the score is
#' the only meaningful signal. Requiring strict score progress on a tie keeps
#' every tie acceptance monotone in the score, so the search cannot cycle.
#'
#' @param cur Current \code{\link{harvey_eval}} result
#' @param dir Numeric direction to step along
#' @inheritParams harvey_eval
#'
#' @return \code{NULL} when no halving is accepted (a stall), otherwise a list
#'   with the accepted \code{eval} and the number of \code{halves} taken
#' @keywords internal
harvey_line_search <- function(cur, dir, y, x_mat, pos, col_abs) {
  ctrl <- LOG_VARIANCE_HARVEY_CONTROL
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
#' constant-information Fisher direction. Newton converges quadratically near
#' the solution; Fisher (the always positive definite \eqn{0.5 X'X}) is the
#' globally safe direction far from it. Expected-information scoring alone is
#' only linearly convergent and crawls on heavy-tailed responses, which is why
#' the hybrid exists.
#'
#' @param cur Current \code{\link{harvey_eval}} result
#' @param x_mat Numeric design matrix, intercept column included
#'
#' @return Numeric direction vector, or \code{NULL}
#' @keywords internal
harvey_newton_dir <- function(cur, x_mat) {
  obs <- crossprod(x_mat, cur$r * x_mat)
  d <- diag(obs)
  # gate the diagonal before sqrt: a nonpositive entry reaches the same NULL
  # either way, but sqrt() would emit a spurious NaN warning first
  if (!all(is.finite(obs)) || any(!is.finite(d)) || any(d <= 0)) {
    return(NULL)
  }
  normalized <- obs / tcrossprod(sqrt(d))
  if (rcond(normalized) < LOG_VARIANCE_HARVEY_CONTROL$NEWTON_RCOND_TOLERANCE) {
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
#' The initial-start shortcut exits converged with code \code{0} when the
#' scaled score already passes; otherwise each iteration prefers the
#' observed-Newton direction and falls back to the Fisher direction when the
#' observed information is ill conditioned or its line search stalls.
#' Convergence needs a passed score \emph{and} a relative criterion or
#' parameter change, so a step that merely grazes the tolerance is not enough.
#' \code{MAXIT} is a safety ceiling: Newton gets there in a handful of
#' iterations, the Fisher fallback on a pathological point may need many more.
#'
#' @param cur Evaluated start from \code{\link{harvey_eval}}
#' @inheritParams harvey_eval
#' @param chol_xx Upper triangular Cholesky factor of \code{crossprod(x_mat)}
#'
#' @return List with the last \code{eval}, the \code{iters} taken (negative on
#'   a stall, marking the iteration it stalled at), the cumulative
#'   \code{halves}, and a \code{status} of \code{"converged"},
#'   \code{"line_search_stall"}, or \code{"iteration_cap"}
#' @keywords internal
harvey_scoring <- function(cur, y, x_mat, pos, col_abs, chol_xx) {
  ctrl <- LOG_VARIANCE_HARVEY_CONTROL
  if (cur$score_norm <= ctrl$SCORE_TOLERANCE) {
    return(list(eval = cur, iters = 0L, halves = 0L, status = "converged"))
  }
  total_halves <- 0L
  for (it in seq_len(ctrl$MAXIT)) {
    dir_newton <- harvey_newton_dir(cur, x_mat)
    taken <- if (is.null(dir_newton)) {
      NULL
    } else {
      harvey_line_search(cur, dir_newton, y, x_mat, pos, col_abs)
    }
    if (is.null(taken)) {
      dir_fisher <- harvey_chol_solve(chol_xx, cur$moment)
      taken <- harvey_line_search(cur, dir_fisher, y, x_mat, pos, col_abs)
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
#' diagonally-normalized information \code{rcond} above tolerance. Normalizing
#' by the diagonal makes the gate scale-invariant while still catching genuine
#' rank deficiency. \code{NULL} rejects the point.
#'
#' @inheritParams harvey_eval
#'
#' @return \code{NULL} on rejection, otherwise a list with the recomputed
#'   \code{eval}, the observed \code{info}, and its normalized \code{rcond}
#' @keywords internal
harvey_post_stop <- function(theta, y, x_mat, pos, col_abs) {
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
  if (!is.finite(rc) || rc < LOG_VARIANCE_HARVEY_CONTROL$RCOND_TOLERANCE) {
    return(NULL)
  }
  list(eval = ev, info = info, rcond = rc)
}
