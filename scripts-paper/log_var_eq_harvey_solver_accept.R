# Scoring and acceptance helpers for the Harvey variance solver, split out of
# log_var_eq_harvey_solver.R to keep both files under the hard 200-line cap: the
# Cholesky triangular solve, the guarded single-point evaluation, the monotone
# backtracking line search, the scoring loop, the fresh post-stop acceptance
# gate, the result skeleton, the recession map, and the stability-pair check. No
# full-vector y * exp(-eta), y / mu, or e / mu ever forms; a nonfinite quantity
# is a hard trial failure, never a clamp. Sourced by the solver before the fit.

# Upper Cholesky of X'X, recomputed only when the constructor passes none, so the
# factor-once contract is enforced by the signature rather than by convention.
hv_chol_xx <- function(x_mat, chol_xx) {
  if (is.null(chol_xx)) chol(crossprod(x_mat)) else chol_xx
}

# Solve (X'X) d = m from the upper factor R (R'R = X'X) with triangular solves
# only, mirroring the Plan 1 no-LU convention.
hv_chol_solve <- function(chol_xx, m) {
  backsolve(chol_xx, forwardsolve(chol_xx, m, upper.tri = TRUE, transpose = TRUE))
}

# One guarded evaluation at theta: returns NULL on any nonfinite required
# quantity (a hard trial failure), else the safe ratio, criterion, moment, and
# scaled score. The ratio's positive rows may be Inf; that is caught here.
hv_eval <- function(theta, y, x_mat, pos, col_abs) {
  if (!all(is.finite(theta))) {
    return(NULL)
  }
  eta <- drop(x_mat %*% theta)
  if (!all(is.finite(eta))) {
    return(NULL)
  }
  r <- tryCatch(logvar_harvey_ratio(theta, y, x_mat), error = function(cond) NULL)
  if (is.null(r) || !is.numeric(r) || length(r) != length(y) || anyNA(r) ||
    !all(is.finite(r[pos]))) {
    return(NULL)
  }
  q <- 0.5 * (sum(eta) + sum(r[pos]))
  moment <- drop(crossprod(x_mat, r - 1))
  score_norm <- max(abs(moment) / col_abs)
  if (!is.finite(q) || !is.finite(score_norm)) {
    return(NULL)
  }
  list(theta = theta, eta = eta, r = r, q = q, moment = moment, score_norm = score_norm)
}

# Backtracking line search along dir from cur: accept a strict criterion decrease
# or, on an exact tie, only a scaled-score improvement past the pinned margin.
# Returns NULL (line-search stall) after 30 halvings without an accepted trial.
hv_line_search <- function(cur, dir, y, x_mat, pos, col_abs) {
  step <- 1
  for (halves in 0:30) {
    trial <- hv_eval(cur$theta + step * dir, y, x_mat, pos, col_abs)
    if (!is.null(trial)) {
      if (trial$q < cur$q) {
        return(list(eval = trial, halves = halves))
      }
      if (trial$q == cur$q) {
        margin <- 10 * .Machine$double.eps * max(1, cur$score_norm)
        if (cur$score_norm - trial$score_norm > margin) {
          return(list(eval = trial, halves = halves))
        }
      }
    }
    step <- step / 2
  }
  NULL
}

# Full scoring solve from one preflighted start eval. The initial-start shortcut
# exits converged with code 0 when the scaled score already passes; otherwise
# iterate to maxit, converging only on a passed score plus a relative change.
hv_scoring <- function(cur, y, x_mat, pos, col_abs, chol_xx, tol_score = 1e-8,
                       maxit = 200L) {
  if (cur$score_norm <= tol_score) {
    return(list(eval = cur, iters = 0L, halves = 0L, status = "converged"))
  }
  total_halves <- 0L
  for (it in seq_len(maxit)) {
    dir <- hv_chol_solve(chol_xx, cur$moment)
    ls <- hv_line_search(cur, dir, y, x_mat, pos, col_abs)
    if (is.null(ls)) {
      return(list(
        eval = cur, iters = -it, halves = total_halves,
        status = "line_search_stall"
      ))
    }
    total_halves <- total_halves + ls$halves
    new <- ls$eval
    rel_q <- abs(new$q - cur$q) <= 1e-10 * max(1, abs(new$q))
    rel_t <- max(abs(new$theta - cur$theta)) <= 1e-10 * max(1, max(abs(new$theta)))
    if (new$score_norm <= tol_score && (rel_q || rel_t)) {
      return(list(eval = new, iters = it, halves = total_halves, status = "converged"))
    }
    cur <- new
  }
  list(eval = cur, iters = maxit, halves = total_halves, status = "iteration_cap")
}

# Fresh post-stop acceptance: recompute the safe ratio and criterion; require a
# finite strictly positive variance and a diagonally-normalized information rcond
# (scale-invariant, still catches genuine rank deficiency); store the observed
# information. NULL rejects.
hv_post_stop <- function(theta, y, x_mat, pos, col_abs) {
  ev <- hv_eval(theta, y, x_mat, pos, col_abs)
  if (is.null(ev)) {
    return(NULL)
  }
  mu <- tryCatch(logvar_harvey_mu(theta, x_mat), error = function(cond) NULL)
  if (is.null(mu) || !is.numeric(mu)) {
    mu <- exp(ev$eta)
  }
  if (!all(is.finite(mu)) || !all(mu > 0)) {
    return(NULL)
  }
  info <- tryCatch(logvar_harvey_info(theta, y, x_mat), error = function(cond) NULL)
  if (is.null(info)) {
    info <- 0.5 * crossprod(x_mat, ev$r * x_mat)
  }
  dg <- sqrt(diag(info))
  rc <- tryCatch(rcond(info / tcrossprod(dg)), error = function(cond) 0)
  if (!is.finite(rc) || rc < 1e-10) {
    return(NULL)
  }
  list(eval = ev, info = info, rcond = rc)
}

# Complete fit-result skeleton: every Plan 7 field is always present so success
# and each fail-closed branch share one shape.
hv_result <- function(coef = NULL, fit_status, converged = FALSE,
                      objective = NA_real_, score_norm = NA_real_,
                      convergence_code = -1L, warm_start = NULL,
                      error_class = NA_character_, n_zero = 0L,
                      rank_x_pos = NA_integer_, rcond_info = NA_real_,
                      n_halvings = NA_integer_, start_attempts = list(),
                      per_start_criteria = NULL, recession = NULL,
                      info_matrix = NULL) {
  list(
    coef = coef, fit_status = fit_status, converged = converged,
    objective = objective, score_norm = score_norm,
    convergence_code = convergence_code,
    diagnostics = list(
      warnings = character(0), messages = character(0), error_class = error_class,
      start_attempts = start_attempts, n_zero_response = n_zero,
      rank_x_pos = rank_x_pos, rcond_info = rcond_info, n_halvings = n_halvings,
      per_start_criteria = per_start_criteria, recession_certificate = recession,
      info_matrix = info_matrix
    ),
    warm_start = warm_start
  )
}

# Map a nonpassing recession classification to its fit status and error class.
hv_recession_map <- list(
  negative_recession = c("nonexistence", "negative_recession"),
  zero_recession = c("nonconvergence", "zero_recession_unresolved"),
  certificate_failure = c("nonconvergence", "recession_certificate_failed")
)

# Evaluate one stability pair without fitting: validate the response and start,
# require finite eta and strictly positive variance, a finite safe ratio, score,
# and information, then take one backtracked scoring proposal (the first trial).
hv_precheck_pair <- function(pair, label, x_mat, n, p, col_abs, chol_xx) {
  rec <- function(ok, reason, objective = NA_real_, score_norm = NA_real_,
                  proposal_ok = FALSE) {
    list(
      label = label, ok = ok, objective = objective, score_norm = score_norm,
      proposal_ok = proposal_ok, reason = reason
    )
  }
  y <- if (!is.null(pair$response)) pair$response else pair$y
  start <- pair$start
  if (!is.numeric(y) || length(y) != n || anyNA(y) || any(!is.finite(y)) ||
    any(y < 0)) {
    return(rec(FALSE, "invalid_response"))
  }
  if (!is.numeric(start) || length(start) != p || !all(is.finite(start))) {
    return(rec(FALSE, "invalid_start"))
  }
  pos <- y > 0
  cur <- hv_eval(start, y, x_mat, pos, col_abs)
  if (is.null(cur)) {
    return(rec(FALSE, "nonfinite_start_eval"))
  }
  mu <- exp(cur$eta)
  if (!all(is.finite(mu)) || !all(mu > 0)) {
    return(rec(FALSE, "nonpositive_mu", cur$q, cur$score_norm))
  }
  info <- 0.5 * crossprod(x_mat, cur$r * x_mat)
  if (!all(is.finite(info))) {
    return(rec(FALSE, "nonfinite_info", cur$q, cur$score_norm))
  }
  dir <- hv_chol_solve(chol_xx, cur$moment)
  trial <- hv_eval(start + dir, y, x_mat, pos, col_abs)
  proposal_ok <- !is.null(trial)
  rec(
    proposal_ok, if (proposal_ok) NA_character_ else "proposal_nonfinite",
    cur$q, cur$score_norm, proposal_ok
  )
}
