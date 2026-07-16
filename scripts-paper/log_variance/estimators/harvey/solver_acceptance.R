# Scoring and acceptance helpers for the Harvey variance solver, split out of
# solver.R to keep both files under the hard 200-line cap: the
# Cholesky triangular solve, the guarded single-point evaluation, the monotone
# backtracking line search, the scoring loop, the fresh post-stop acceptance
# gate, the result skeleton, the recession map, and the stability-pair check. No
# full-vector y * exp(-eta), y / mu, or e / mu ever forms; a nonfinite quantity
# is a hard trial failure, never a clamp. The result skeleton, recession map,
# and stability-pair check live in solver_result.R. Sourced
# by the solver before the fit.

# Upper Cholesky of X'X, recomputed only when the constructor passes none, so the
# factor-once contract is enforced by the signature rather than by convention.
hv_chol_xx <- function(x_mat, chol_xx) {
  if (is.null(chol_xx)) chol(crossprod(x_mat)) else chol_xx
}

# Solve (X'X) d = m from the upper factor R (R'R = X'X) with triangular solves
# only, mirroring the Harvey no-LU convention.
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
  # a finite eta can still overflow the fitted variance mu = exp(eta); reject
  # it as a hard trial failure so the finite strictly-positive-variance
  # invariant holds at every iterate, not only at the accepted fit
  if (!all(is.finite(eta)) || any(eta > log(.Machine$double.xmax))) {
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

# Backtracking line search along dir from cur: accept a strict criterion
# decrease or, on a criterion tie, only a scaled-score improvement past the
# pinned margin. A tie is any difference within the criterion's summation
# rounding error (eps times the absolute-term sums of Q), not literal
# equality of the stored doubles: near the optimum the scoring step's true
# decrease sits below that noise floor, so two evaluations cannot be
# distinguished by Q and the score is the only meaningful signal (the score
# check, not descent alone, decides convergence). The mandatory strict score
# progress keeps every tie acceptance monotone in the score, so the search
# cannot cycle. Returns NULL (stall) after 30 halvings with no acceptance.
hv_line_search <- function(cur, dir, y, x_mat, pos, col_abs) {
  step <- 1
  q_noise <- 4 * .Machine$double.eps *
    (1 + sum(abs(cur$eta)) + sum(cur$r[pos]))
  for (halves in 0:30) {
    trial <- hv_eval(cur$theta + step * dir, y, x_mat, pos, col_abs)
    if (!is.null(trial)) {
      if (trial$q < cur$q) {
        return(list(eval = trial, halves = halves))
      }
      if (abs(trial$q - cur$q) <= q_noise) {
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

# Observed-Newton direction (X' diag(r) X)^-1 X'(r - 1) when the observed
# information is well conditioned, else NULL so the caller falls back to the
# constant-information Fisher direction. Newton is quadratically convergent
# near the solution; Fisher (constant 0.5 X'X, always positive definite) is
# the globally safe direction far from it. This adaptive hybrid is the dossier
# section 2 / section 13 observed-Newton acceleration, needed because
# expected-information scoring alone is only linearly convergent and crawls
# (or hits the cap) on the estimation sample's heavy-tailed identified-set
# responses -- the plan deferred it on the premise that scoring converges in a
# handful of iterations, which the real data falsifies.
hv_newton_dir <- function(cur, x_mat) {
  obs <- crossprod(x_mat, cur$r * x_mat)
  if (!all(is.finite(obs))) {
    return(NULL)
  }
  dg <- sqrt(diag(obs))
  if (any(!is.finite(dg)) || any(dg <= 0) || rcond(obs / tcrossprod(dg)) < 1e-12) {
    return(NULL)
  }
  r_chol <- tryCatch(chol(obs), error = function(cond) NULL)
  if (is.null(r_chol)) {
    return(NULL)
  }
  backsolve(r_chol, forwardsolve(r_chol, cur$moment, upper.tri = TRUE, transpose = TRUE))
}

# Full solve from one preflighted start eval. The initial-start shortcut exits
# converged with code 0 when the scaled score already passes; otherwise each
# iteration prefers the observed-Newton direction and falls back to the Fisher
# direction when the observed information is ill conditioned or its line search
# stalls, converging only on a passed score plus a relative change. maxit is a
# safety ceiling: Newton reaches score 1e-8 in a handful of iterations, but the
# Fisher fallback on a pathological point may need many more.
hv_scoring <- function(cur, y, x_mat, pos, col_abs, chol_xx, tol_score = 1e-8,
                       maxit = 1000L) {
  if (cur$score_norm <= tol_score) {
    return(list(eval = cur, iters = 0L, halves = 0L, status = "converged"))
  }
  total_halves <- 0L
  for (it in seq_len(maxit)) {
    dir_n <- hv_newton_dir(cur, x_mat)
    ls <- if (!is.null(dir_n)) hv_line_search(cur, dir_n, y, x_mat, pos, col_abs) else NULL
    if (is.null(ls)) {
      dir_f <- hv_chol_solve(chol_xx, cur$moment)
      ls <- hv_line_search(cur, dir_f, y, x_mat, pos, col_abs)
    }
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
